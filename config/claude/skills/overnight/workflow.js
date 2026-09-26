export const meta = {
  name: 'overnight-batch',
  description: 'Implement, verify and hand off one batch of overnight tasks as PR-ready branches',
  phases: [
    { title: 'Implement', detail: 'one implementer per task in its own worktree', model: 'sonnet' },
    { title: 'Verify', detail: 'adversarial review of each branch against its acceptance criteria' },
    { title: 'Fix', detail: 'one repair round for branches that failed review', model: 'sonnet' },
  ],
}

// Input, passed as `args` by the orchestrator each cycle:
//   tasks   the JSON array printed by `overnight.nu next`
//   config  the JSON object printed by `overnight.nu config`
// Output: { results: [{ id, title, status: 'ready' | 'failed', branch, summary, pr_body?, failure? }] }
//
// Agents here never open PRs and never merge. The orchestrator turns each
// `ready` result into a PR and each `failed` one into a retry or a block, so
// every GitHub write stays in one place and stays idempotent.

const tasks = Array.isArray(args?.tasks) ? args.tasks : []
const config = args?.config || {}
if (tasks.length === 0) return { results: [] }

const base = config.base_branch || 'main'
const envBlock = Object.entries(config.env || {}).map(([k, v]) => `${k}=${v}`).join('\n')

const RESULT = {
  type: 'object',
  properties: {
    status: { type: 'string', enum: ['ready', 'failed'] },
    branch: { type: 'string' },
    summary: { type: 'string', description: 'What changed and why, 3-8 lines, written for the PR body' },
    checks_run: { type: 'array', items: { type: 'string' } },
    failure: { type: 'string', description: 'When failed: what blocked you, specifically, with evidence' },
  },
  required: ['status', 'branch', 'summary'],
}

const VERDICT = {
  type: 'object',
  properties: {
    approved: { type: 'boolean' },
    findings: { type: 'array', items: { type: 'string' }, description: 'Actionable, file:line where possible' },
    summary: { type: 'string' },
  },
  required: ['approved', 'findings', 'summary'],
}

function checksFor(task) {
  const all = config.checks || {}
  const wanted = task.checks && task.checks.length ? task.checks : Object.keys(all)
  const keys = wanted.filter(k => all[k])
  if (keys.length === 0) return '(none configured: run the test and lint commands the repository CLAUDE.md names)'
  return keys.map(k => `${k}: ${all[k]}`).join('\n')
}

function taskCard(task) {
  return [
    `Task #${task.id}: ${task.title}`,
    task.url ? `Link: ${task.url}` : null,
    '',
    'Description:',
    task.body || '(none)',
    '',
    'Acceptance criteria, the contract being implemented:',
    task.criteria || '(none given: derive them from the description and state them in your summary)',
  ].filter(l => l !== null).join('\n')
}

function houseRules(task) {
  return [
    `Branch: ${task.branch}, based on origin/${base}.`,
    'Checks that must pass before you push, run from the worktree root:',
    checksFor(task),
    envBlock ? `Set these environment variables for every check:\n${envBlock}` : null,
    config.commit_style ? `Commit message style: ${config.commit_style}` : null,
    '',
    'Hard limits:',
    '- Never push to any branch other than yours, never force-push, never merge, never open a PR.',
    '- Never weaken a check to get to green: no skipped or deleted tests, no lint suppressions, no loosened assertions. If a check fails for a reason outside your change, stop and report failed with the evidence.',
    '- Stay inside the scope of the task. Unrelated cleanups belong in a sentence of your summary, not in the diff.',
    '- Nobody will answer questions tonight. Decide, do, and report; an honest failed report beats a guessed implementation.',
  ].filter(l => l !== null).join('\n')
}

function implementPrompt(task) {
  const attempt = (task.attempts || 0) + 1
  return [
    `You are implementing one task unattended, overnight, in an isolated git worktree of this repository. This is attempt ${attempt}.`,
    '',
    taskCard(task),
    '',
    houseRules(task),
    '',
    'Steps:',
    `1. Run \`git fetch origin\`. If origin/${task.branch} exists, a previous attempt left work there: check it out, read its log, and continue from it. Otherwise run \`git checkout -b ${task.branch} origin/${base}\`.`,
    '2. Read the code the task touches before changing it. Follow the repository CLAUDE.md conventions; they apply to you.',
    '3. Implement the acceptance criteria, including the tests the repository expects for new behavior.',
    '4. Run every check listed above. All must pass.',
    `5. Commit in the repository style and run \`git push -u origin ${task.branch}\`.`,
    '6. Report: status ready or failed, the branch, a summary written for a PR body, the checks you ran, and on failure what blocked you.',
  ].join('\n')
}

function verifyPrompt(task, impl) {
  return [
    `Claim to verify: branch origin/${impl.branch || task.branch} satisfies the acceptance criteria of task #${task.id} and would pass this repository's CI without any gate having been weakened.`,
    '',
    `You are in an isolated worktree. Run \`git fetch origin\`, then examine \`git diff origin/${base}...origin/${impl.branch || task.branch}\` and the files as they exist on that branch. When the diff touches behavior, check the branch out here and run the relevant checks yourself rather than trusting the report; say which you ran.`,
    '',
    taskCard(task),
    '',
    'Checks the project defines:',
    checksFor(task),
    envBlock ? `Environment for checks:\n${envBlock}` : null,
    '',
    'The implementer reported:',
    impl.summary || '(no summary)',
    `Checks it says it ran: ${(impl.checks_run || []).join(', ') || 'none reported'}`,
    '',
    'Look specifically for: criteria not met or only partly met; new behavior without tests; tests weakened (ignored, skipped, deleted, assertions removed, lint suppressions added); scope creep; changes that break other callers; anything a maintainer would bounce.',
    '',
    'Approve only if you would merge this once CI is green. Findings must be specific and actionable.',
  ].filter(l => l !== null).join('\n')
}

function fixPrompt(task, impl, findings) {
  return [
    'A reviewer rejected the branch below. Address every finding, unattended, in an isolated git worktree of this repository.',
    '',
    taskCard(task),
    '',
    houseRules(task),
    '',
    'Findings to address:',
    ...findings.map(f => `- ${f}`),
    '',
    'Previous summary of the branch:',
    impl.summary || '(none)',
    '',
    'Steps:',
    `1. \`git fetch origin\` then \`git checkout ${impl.branch || task.branch}\`.`,
    '2. Fix each finding. If a finding is wrong, say so in your summary with the reason instead of changing code to satisfy it.',
    '3. Run every check listed above. All must pass.',
    '4. Commit and push to the same branch.',
    '5. Report as before: status, branch, summary, checks run, failure if any.',
  ].join('\n')
}

function ready(task, impl, verdict) {
  const closes = config.backend === 'github' ? `Closes #${task.id}` : `Task ${task.id}: ${task.title}`
  const pr_body = [
    closes,
    '',
    impl.summary,
    '',
    '## Verification',
    verdict.summary,
    '',
    `Checks run: ${(impl.checks_run || []).join(', ') || 'see CI'}`,
    '',
    '_Opened by an unattended overnight run. Review before merging._',
  ].join('\n')
  return { id: task.id, title: task.title, status: 'ready', branch: impl.branch || task.branch, summary: impl.summary, review: verdict.summary, pr_body }
}

function failed(task, summary, failure) {
  return { id: task.id, title: task.title, status: 'failed', branch: task.branch, summary: summary || '', failure }
}

const implementOpts = id => ({ label: `implement:${id}`, phase: 'Implement', schema: RESULT, model: 'sonnet', effort: 'high', isolation: 'worktree' })
const fixOpts = id => ({ label: `fix:${id}`, phase: 'Fix', schema: RESULT, model: 'sonnet', effort: 'high', isolation: 'worktree' })
const verifyOpts = (id, phase) => ({ label: `${phase === 'Fix' ? 'reverify' : 'verify'}:${id}`, phase, schema: VERDICT, agentType: 'verifier', isolation: 'worktree' })

log(`batch of ${tasks.length}: ${tasks.map(t => '#' + t.id).join(' ')}`)

const results = await pipeline(
  tasks,
  task => agent(implementPrompt(task), implementOpts(task.id)),
  async (impl, task) => {
    if (!impl) return failed(task, '', 'implementer returned nothing (terminal API error or skipped)')
    if (impl.status !== 'ready') return failed(task, impl.summary, impl.failure || 'implementer reported failed without a reason')

    const verdict = await agent(verifyPrompt(task, impl), verifyOpts(task.id, 'Verify'))
    if (verdict && verdict.approved) return ready(task, impl, verdict)

    const findings = verdict ? verdict.findings : ['verifier returned nothing; treat as not approved']
    const fixed = await agent(fixPrompt(task, impl, findings), fixOpts(task.id))
    if (!fixed || fixed.status !== 'ready') {
      const why = fixed ? `fix round failed: ${fixed.failure || 'no reason given'}` : 'fix round returned nothing'
      return failed(task, fixed ? fixed.summary : impl.summary, `${why}; review findings: ${findings.join(' | ')}`)
    }

    const again = await agent(verifyPrompt(task, fixed), verifyOpts(task.id, 'Fix'))
    if (again && again.approved) return ready(task, fixed, again)
    const remaining = again ? again.findings : ['verifier returned nothing on re-review']
    return failed(task, fixed.summary, `not approved after one fix round: ${remaining.join(' | ')}`)
  },
)

const final = results.map((r, i) => r || failed(tasks[i], '', 'pipeline stage threw'))
log(`batch done: ${final.filter(r => r.status === 'ready').length} ready, ${final.filter(r => r.status === 'failed').length} failed`)
return { results: final }
