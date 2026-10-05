export const meta = {
  name: 'review-diff',
  description: 'Lens reviewers find defects in a branch diff; adversarial refuters try to kill each finding',
  whenToUse: 'Post-code review of a branch, or of the uncommitted tree, before merging. args (all optional): {base, head, uncommitted, cwd, context, spec, lenses, refuters, cap, models, effort, report}; a bare string is taken as base.',
  phases: [
    { title: 'Review', detail: 'one read-only reviewer per lens', model: 'sonnet' },
    { title: 'Refute', detail: 'K refuters per finding, each from a different angle', model: 'opus' },
    { title: 'Report', detail: 'only when args.report names a file' },
  ],
}

const A = typeof args === 'string' ? { base: args } : (args ?? {})
const base = A.base ?? 'main'
const head = A.head ?? 'HEAD'
const REFUTERS = A.refuters ?? 3
const CAP = A.cap ?? 6
const MODELS = { review: 'sonnet', refute: 'opus', report: 'sonnet', ...A.models }
const EFFORT = { review: 'high', refute: 'high', report: 'low', ...A.effort }

const BUILTIN_LENSES = {
  correctness: 'Logic errors, unhandled edge cases (empty, zero, maximum, missing, non-ASCII), error handling that swallows or misreports failures, off-by-one and ordering mistakes, and behaviour that contradicts what the code, its callers or its docs promise.',
  tests: 'Test honesty. For each behaviour the change touches, name the test that would catch its regression. Report changed behaviour with no such test, tests that pass for the wrong reason (asserting on mocks, tautologies, assertions loosened until green), and tests skipped or deleted.',
  security: 'Trust boundaries: input validation, injection, authentication, authorization and tenancy checks, secrets in code or logs, unsafe deserialization, path traversal, and permissions the change widens.',
  concurrency: 'Shared state and ordering: races, missing or mis-scoped locks and transactions, partial failure mid-operation, retries that are not idempotent, cancellation and cleanup paths, and resource leaks.',
  conventions: "Fit with the codebase: the project's CLAUDE.md rules, the naming and structure of neighbouring code, docs the change should have updated and did not, and comments that narrate the change instead of explaining why.",
}

const ANGLES = [
  { key: 'code-path', prompt: 'Read the code path end to end, from where it is entered to the cited line. Does the scenario actually reach that line in the state described?' },
  { key: 'guards', prompt: 'Look for an existing guard, validation, type constraint or test that already prevents or catches this.' },
  { key: 'runtime', prompt: 'Check the library, framework or runtime behaviour the finding depends on, from its source or with a minimal experiment in the scratchpad.' },
]

const LENSES = resolveLenses(A.lenses, BUILTIN_LENSES)

const where = A.cwd ? ` in the repository at ${A.cwd} (run git as \`git -C ${A.cwd} ...\`)` : ''
const TARGET = A.uncommitted
  ? `the uncommitted changes${where}: \`git diff HEAD\`, plus the untracked files \`git status --short\` lists`
  : `the changes on ${head} since it diverged from ${base}${where}: \`git diff ${base}...${head}\`, with \`git log ${base}..${head}\` for intent`

const BACKGROUND = [
  A.context && `Context from the caller:\n${A.context}`,
  A.spec && `The change implements the design in ${A.spec}. Read it. Decisions recorded there are settled: report where the code departs from them, not whether they were right.`,
].filter(Boolean).join('\n\n')

const SEVERITY = { blocker: 0, important: 1, minor: 2 }
const bySeverity = (a, b) => SEVERITY[a.severity] - SEVERITY[b.severity]

const FINDINGS = {
  type: 'object',
  properties: {
    findings: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          title: { type: 'string' },
          severity: { type: 'string', enum: ['blocker', 'important', 'minor'] },
          file: { type: 'string' },
          line: { type: 'integer' },
          evidence: { type: 'string', description: 'short verbatim excerpt of the cited lines' },
          failure_scenario: { type: 'string', description: 'concrete input or state, and the wrong outcome it produces' },
          suggested_fix: { type: 'string' },
        },
        required: ['title', 'severity', 'file', 'line', 'evidence', 'failure_scenario', 'suggested_fix'],
      },
    },
    checked: { type: 'array', items: { type: 'string' }, description: 'what you verified and found sound' },
  },
  required: ['findings', 'checked'],
}

const VERDICT = {
  type: 'object',
  properties: {
    verdict: { type: 'string', enum: ['REFUTED', 'PLAUSIBLE', 'CONFIRMED'] },
    reasoning: { type: 'string' },
    evidence: { type: 'string', description: 'path:line and the verbatim lines that settle it' },
  },
  required: ['verdict', 'reasoning', 'evidence'],
}

function resolveLenses(spec, builtin) {
  if (!spec) return Object.entries(builtin).map(([key, prompt]) => ({ key, prompt }))
  return spec.map(l => {
    if (typeof l !== 'string') return l
    if (!builtin[l]) throw new Error(`unknown lens "${l}"; built-in lenses are ${Object.keys(builtin).join(', ')}, or pass {key, prompt}`)
    return { key: l, prompt: builtin[l] }
  })
}

// Majority-refuted kills a finding; any dissent short of that is surfaced
// as contested rather than dropped, and a finding whose refuters all died
// is unjudged, never silently confirmed or lost.
function judge(votes) {
  const cast = votes.filter(Boolean)
  const refuted = cast.filter(v => v.verdict === 'REFUTED').length
  const confirmed = cast.filter(v => v.verdict === 'CONFIRMED').length
  const status = cast.length === 0 ? 'unjudged'
    : refuted * 2 > cast.length ? 'refuted'
    : refuted > 0 ? 'contested'
    : confirmed > 0 ? 'confirmed'
    : 'plausible'
  return { status, votes: cast }
}

function reviewPrompt(lens) {
  return `You are the ${lens.key} reviewer of ${TARGET}.
${BACKGROUND ? `\n${BACKGROUND}\n` : ''}
Your lens: ${lens.prompt}

Rules:
- Read-only. Do not edit files, and do not run git stash, checkout, switch, reset or commit.
- Report what the change introduces or exposes, not pre-existing problems in code it leaves alone.
- Every finding needs evidence you opened yourself: file and line, a short verbatim excerpt, and a concrete failure scenario (input or state, then the wrong outcome). No scenario, no finding.
- Stay inside your lens; other reviewers cover the rest.
- Severity: blocker = wrong behaviour, data loss, a security hole or a broken build on a realistic path; important = a real defect on a less common path, or changed behaviour with no test; minor = real but low impact.
- At most ${CAP} findings, most severe first. Fewer, sharper findings beat a long list.
- List what you checked and found sound in \`checked\`, especially when you report nothing.`
}

function refutePrompt(lens, finding, angle) {
  return `A ${lens.key} reviewer reported this finding about ${TARGET}. Your job is to refute it.

${JSON.stringify(finding, null, 2)}

Your angle: ${angle.prompt}

Read the cited lines yourself; do not trust the paraphrase. Then decide:
- REFUTED: you can show the scenario cannot happen in the code as written, or that something already handles it. Quote what handles it.
- CONFIRMED: you walked the scenario yourself and it fails as described.
- PLAUSIBLE: it looks real, but you could neither reproduce it nor rule it out.
A real finding is not refuted for being minor. Do not edit tracked files; put any experiment in the scratchpad.`
}

function refute(lens, finding, index) {
  return parallel(Array.from({ length: REFUTERS }, (_, i) => () => {
    const angle = ANGLES[i % ANGLES.length]
    return agent(refutePrompt(lens, finding, angle), {
      label: `refute:${lens.key}#${index + 1}:${angle.key}`,
      phase: 'Refute',
      schema: VERDICT,
      model: MODELS.refute,
      effort: EFFORT.refute,
    }).then(v => v && { angle: angle.key, ...v })
  })).then(votes => ({ lens: lens.key, ...finding, ...judge(votes) }))
}

const failedLenses = []
const unverified = []
const checked = {}

const perLens = await pipeline(
  LENSES,
  lens => agent(reviewPrompt(lens), {
    label: `review:${lens.key}`,
    phase: 'Review',
    schema: FINDINGS,
    model: MODELS.review,
    effort: EFFORT.review,
  }),
  (review, lens) => {
    if (!review) {
      failedLenses.push(lens.key)
      return []
    }
    checked[lens.key] = review.checked
    const sorted = [...review.findings].sort(bySeverity)
    if (sorted.length > CAP) {
      unverified.push(...sorted.slice(CAP).map(f => ({ lens: lens.key, ...f })))
      log(`${lens.key}: ${sorted.length - CAP} finding(s) past the cap of ${CAP} left unverified`)
    }
    return parallel(sorted.slice(0, CAP).map((f, i) => () => refute(lens, f, i)))
  },
)

const all = perLens.filter(Boolean).flat().filter(Boolean)
const withStatus = s => all.filter(f => f.status === s).sort(bySeverity)
const result = {
  target: TARGET,
  lenses: LENSES.map(l => l.key),
  refuters: REFUTERS,
  confirmed: withStatus('confirmed'),
  plausible: withStatus('plausible'),
  contested: withStatus('contested'),
  unjudged: withStatus('unjudged'),
  refuted: withStatus('refuted'),
  unverified: unverified.sort(bySeverity),
  failedLenses,
  checked,
}

if (failedLenses.length) log(`lenses that returned nothing: ${failedLenses.join(', ')}`)
log(`${result.confirmed.length} confirmed, ${result.plausible.length} plausible, ${result.contested.length} contested, ${result.refuted.length} refuted, ${result.unjudged.length} unjudged, ${unverified.length} unverified`)

if (A.report) {
  await agent(`Write this code-review result to ${A.report}: Org if the path ends in .org, Markdown otherwise. One section per status in this order: confirmed, plausible, contested, unjudged, unverified, refuted. For each finding give its severity, lens, file:line, failure scenario and suggested fix, plus a one-line summary of the refuter votes. Close with the failed lenses and what each lens checked. Write the file and return its path.

${JSON.stringify(result)}`, { label: 'report', phase: 'Report', model: MODELS.report, effort: EFFORT.report })
}

return result
