export const meta = {
  name: 'implement-slices',
  description: 'Implement settled slices in their own git worktrees, each adversarially verified with bounded fix-up rounds',
  whenToUse: 'Execute a plan whose slices are already designed and briefed. args: {slices (required), base, cwd, worktreeRoot, branchPrefix, rules, gauntlet, maxFixRounds, sequential, prepare, models, effort}. Leaves verified branches; merging stays with the caller.',
  phases: [
    { title: 'Prepare', detail: 'create or confirm one worktree per slice' },
    { title: 'Implement', detail: 'one implementer per slice, in its worktree', model: 'sonnet' },
    { title: 'Verify', detail: 'one adversarial verifier per slice', model: 'opus' },
    { title: 'Fix-up', detail: 'bounded rounds for slices with must-fix findings', model: 'sonnet' },
  ],
}

// args.slices: [{key, brief, worktree?, branch?}]. A brief carries the
// settled design; this workflow executes it and never redesigns. Without a
// worktree, a slice gets <worktreeRoot>/<key> (default: a <repo>-worktrees
// directory beside the repository) on branch <branchPrefix><key>.
// sequential: run slices one at a time and halt at the first that does not
// pass. Slices given the same worktree and branch then build on each other.
// gauntlet: commands that must pass in each worktree before a slice is done.
// rules: project rules appended to the ground rules every implementer gets.

const A = Array.isArray(args) ? { slices: args } : (args ?? {})
const SLICES = A.slices
if (!Array.isArray(SLICES) || SLICES.length === 0) throw new Error('implement-slices: args.slices ([{key, brief, worktree?, branch?}]) is required')
for (const s of SLICES) {
  if (!s.key || !s.brief) throw new Error(`implement-slices: every slice needs key and brief, got ${JSON.stringify(s).slice(0, 120)}`)
}
if (new Set(SLICES.map(s => s.key)).size !== SLICES.length) throw new Error('implement-slices: slice keys must be unique')

const base = A.base ?? 'main'
const MAX_ROUNDS = A.maxFixRounds ?? 2
const MODELS = { prepare: 'sonnet', implement: 'sonnet', verify: 'opus', ...A.models }
const EFFORT = { prepare: 'low', implement: 'high', verify: 'high', ...A.effort }

const PREP = {
  type: 'object',
  properties: {
    slices: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          key: { type: 'string' },
          worktree: { type: 'string', description: 'absolute path' },
          branch: { type: 'string' },
          ok: { type: 'boolean', description: 'true only if the worktree exists and is on this branch' },
          note: { type: 'string' },
        },
        required: ['key', 'worktree', 'branch', 'ok', 'note'],
      },
    },
  },
  required: ['slices'],
}

const IMPL = {
  type: 'object',
  properties: {
    done: { type: 'boolean', description: 'true only if every part of the brief is complete, the gauntlet passes, and the work is committed' },
    summary: { type: 'string', description: 'what landed, in 5-15 sentences' },
    commits: { type: 'array', items: { type: 'string' }, description: 'short sha and subject of each commit you made' },
    files_changed: { type: 'array', items: { type: 'string' } },
    tests_added: { type: 'array', items: { type: 'string' } },
    gauntlet: { type: 'string', description: 'exact results of the required checks' },
    deviations: { type: 'string', description: 'what was not done, done differently, or wrong in the brief; empty if none' },
    bookkeeping: { type: 'string', description: 'text proposed for files the brief told you not to edit; empty if none' },
    open_questions: { type: 'string' },
  },
  required: ['done', 'summary', 'commits', 'files_changed', 'tests_added', 'gauntlet', 'deviations', 'bookkeeping'],
}

const VERIFY = {
  type: 'object',
  properties: {
    verdict: { type: 'string', enum: ['pass', 'must_fix'] },
    must_fix: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          title: { type: 'string' },
          evidence: { type: 'string', description: 'file:line and the concrete failing scenario' },
          repro: { type: 'string', description: 'the failing test or command you ran, and its output' },
        },
        required: ['title', 'evidence', 'repro'],
      },
    },
    should_fix: { type: 'array', items: { type: 'string' } },
    gauntlet: { type: 'string', description: 'results of your own run' },
    untracked_repros: { type: 'string', description: 'paths of any repro files you left in the worktree; empty if none' },
  },
  required: ['verdict', 'must_fix', 'should_fix', 'gauntlet', 'untracked_repros'],
}

const RULES = `Ground rules:
- Work only inside your worktree, on its branch. ${A.sequential ? 'Earlier slices may already have committed on this branch; build on their work, do not redo it.' : 'Other agents are working in sibling worktrees at the same time; never touch the main checkout or another worktree.'}
- Commit on this branch only. Stage files by explicit path, never \`git add -A\`, \`-a\` or \`.\`. Write each commit message to a file in the scratchpad and commit with \`git commit -F <file>\`, so the message never passes through shell quoting.
- If a test you write fails, look for a production bug before touching the test. Never loosen an assertion, add a sleep or skip a test to get to green.
- Report deviations honestly: anything in the brief you did not do, did differently, or found to be wrong about the code.${A.gauntlet ? `\n- Before you finish, this must pass in your worktree: ${A.gauntlet}` : ''}${A.rules ? `\n${A.rules}` : ''}`

function implementPrompt(s) {
  return `Implement slice "${s.key}" in the worktree ${s.worktree} (branch ${s.branch}, based on ${base}).

${RULES}

Brief:
${s.brief}`
}

function verifyPrompt(s, impl) {
  return `An implementer reports slice "${s.key}" done. Find what is wrong with it.

Worktree ${s.worktree}, branch ${s.branch}, based on ${base}. Review the commits listed in the report (\`git -C ${s.worktree} show <sha>\`) against the brief, with \`git -C ${s.worktree} diff ${base}...${s.branch}\` for the whole picture${A.gauntlet ? `, and run the gauntlet yourself: ${A.gauntlet}` : ', and run the tests that cover the change'}.

Check that the commits do all of what the brief asks; that the new tests would fail without the change; that nothing regressed; and that every claim in the report is supported by the diff. Uncommitted changes to tracked files in the worktree count against the slice.

Report must_fix only for defects you can evidence with file:line and a failing scenario or command output. Default to pass when you cannot produce evidence. Do not edit or commit tracked files; if you write a repro, leave it untracked and give its path.

Implementer's report:
${JSON.stringify(impl, null, 2)}

Brief:
${s.brief}`
}

function fixupPrompt(s, impl, verify) {
  return `A verifier found must-fix defects in slice "${s.key}". Fix each one in the worktree ${s.worktree} (branch ${s.branch}), add or adjust tests so each would have been caught, commit, and report as before, listing only this round's commits. If you believe a finding is wrong, say why in deviations instead of working around it.

${RULES}

Findings:
${JSON.stringify(verify.must_fix, null, 2)}

Your previous report:
${JSON.stringify(impl, null, 2)}

Brief:
${s.brief}`
}

let slices = SLICES.map(s => ({ ...s, branch: s.branch ?? `${A.branchPrefix ?? 'wf/'}${s.key}` }))
const skipped = []

if (A.prepare ?? true) {
  phase('Prepare')
  const prep = await agent(`Prepare one git worktree per slice below${A.cwd ? ` for the repository at ${A.cwd}` : ''}, based on ${base}.

${JSON.stringify(slices.map(s => ({ key: s.key, worktree: s.worktree ?? null, branch: s.branch })), null, 2)}

For each slice:
- With no worktree given, use ${A.worktreeRoot ? `${A.worktreeRoot}/<key>` : '<parent of the repository>/<repository name>-worktrees/<key>'}.
- If the path already holds a worktree of this repository, leave its contents alone; ok is true only if it is on the slice's branch.
- Otherwise create it with \`git worktree add -b <branch> <path> ${base}\`, or \`git worktree add <path> <branch>\` when the branch already exists.
- Several slices may name the same worktree and branch; prepare it once and report it for each.
- If the main checkout has an untracked .envrc that a new worktree lacks, copy it in. When a new worktree has an .envrc and direnv is available, run \`direnv allow <path>\`.
Change nothing else: no commits, no checkouts in the main checkout, no edits to tracked files. Report every slice, including the ones you could not prepare and why.`, { label: 'prepare', phase: 'Prepare', schema: PREP, model: MODELS.prepare, effort: EFFORT.prepare })
  if (!prep) throw new Error('implement-slices: worktree preparation returned nothing')
  const byKey = Object.fromEntries(prep.slices.map(p => [p.key, p]))
  for (const s of slices) {
    if (!byKey[s.key]?.ok) {
      log(`${s.key}: worktree not ready (${byKey[s.key]?.note || 'not reported'}); skipped`)
      skipped.push(s.key)
    }
  }
  slices = slices.filter(s => byKey[s.key]?.ok).map(s => ({ ...s, worktree: byKey[s.key].worktree, branch: byKey[s.key].branch }))
} else {
  const missing = slices.filter(s => !s.worktree).map(s => s.key)
  if (missing.length) throw new Error(`implement-slices: prepare is off, so every slice needs a worktree; missing for ${missing.join(', ')}`)
}

async function runSlice(s) {
  const record = { key: s.key, worktree: s.worktree, branch: s.branch, status: 'failed', rounds: 0, impl: null, verify: null, history: [] }
  let impl = await agent(implementPrompt(s), { label: `implement:${s.key}`, phase: 'Implement', schema: IMPL, agentType: 'implementer', model: MODELS.implement, effort: EFFORT.implement })
  record.impl = impl
  if (!impl) {
    record.note = 'implementer returned nothing'
    return record
  }
  if (!impl.done) {
    record.status = 'incomplete'
    return record
  }

  let verify = await agent(verifyPrompt(s, impl), { label: `verify:${s.key}`, phase: 'Verify', schema: VERIFY, agentType: 'verifier', model: MODELS.verify, effort: EFFORT.verify })
  while (verify && verify.verdict === 'must_fix' && record.rounds < MAX_ROUNDS) {
    record.rounds += 1
    record.history.push({ impl, verify })
    const fixed = await agent(fixupPrompt(s, impl, verify), { label: `fixup:${s.key}#${record.rounds}`, phase: 'Fix-up', schema: IMPL, agentType: 'implementer', model: MODELS.implement, effort: EFFORT.implement })
    if (!fixed) {
      record.verify = verify
      record.status = 'must-fix'
      record.note = `fix-up round ${record.rounds} returned nothing`
      return record
    }
    impl = fixed
    record.impl = impl
    verify = await agent(verifyPrompt(s, impl), { label: `reverify:${s.key}#${record.rounds}`, phase: 'Verify', schema: VERIFY, agentType: 'verifier', model: MODELS.verify, effort: EFFORT.verify })
  }

  record.verify = verify
  record.status = !verify ? 'unverified' : verify.verdict === 'pass' ? 'pass' : 'must-fix'
  if (record.status === 'must-fix') log(`${s.key}: still must-fix after ${record.rounds} fix-up round(s); needs the caller`)
  return record
}

const records = []
if (A.sequential) {
  for (let i = 0; i < slices.length; i++) {
    const r = await runSlice(slices[i])
    records.push(r)
    if (r.status !== 'pass') {
      const rest = slices.slice(i + 1).map(s => s.key)
      if (rest.length) log(`${r.key} ended ${r.status}; halting before ${rest.join(', ')}`)
      skipped.push(...rest)
      break
    }
  }
} else {
  const results = await parallel(slices.map(s => () => runSlice(s)))
  slices.forEach((s, i) => records.push(results[i] ?? { key: s.key, worktree: s.worktree, branch: s.branch, status: 'failed', note: 'slice chain threw' }))
}

const tally = {}
for (const r of records) tally[r.status] = (tally[r.status] ?? 0) + 1
if (skipped.length) tally.skipped = skipped.length
log(Object.entries(tally).map(([k, n]) => `${n} ${k}`).join(', '))

return { base, slices: records, skipped }
