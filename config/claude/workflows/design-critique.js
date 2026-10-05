export const meta = {
  name: 'design-critique',
  description: 'Lens critics attack a design or plan doc against the real code; adversarial refuters try to kill each finding',
  whenToUse: 'Critique a design, spec or plan document before implementing it. args: {doc (required), cwd, context, accepted, lenses, refuters, cap, models, effort, report}; a bare string is taken as doc.',
  phases: [
    { title: 'Critique', detail: 'one read-only critic per lens', model: 'sonnet' },
    { title: 'Refute', detail: 'K refuters per finding, each from a different angle', model: 'opus' },
    { title: 'Report', detail: 'only when args.report names a file' },
  ],
}

const A = typeof args === 'string' ? { doc: args } : (args ?? {})
if (!A.doc) throw new Error('design-critique: args.doc (path to the design document) is required')
const REFUTERS = A.refuters ?? 2
const CAP = A.cap ?? 7
const MODELS = { critique: 'sonnet', refute: 'opus', report: 'sonnet', ...A.models }
const EFFORT = { critique: 'high', refute: 'high', report: 'low', ...A.effort }

const BUILTIN_LENSES = {
  grounding: 'Check the design against the code it will change. Find claims about existing interfaces, data shapes, call sites or behaviour that the code contradicts; steps that cannot work as written; and pieces the design depends on that do not exist. Open the code; never infer from names.',
  tests: 'Ask how each behaviour the design promises would be proven. Find promises with no test that could catch their regression, planned tests that would pass for the wrong reason, and parts that are untestable as designed.',
  'failure-modes': 'Find what happens when things go wrong: concurrent access, partial failure mid-operation, retries, transactions, trust boundaries and tenant isolation, migration and rollback, and operability. Report the scenarios the design leaves unhandled or handles wrongly.',
}

const ANGLES = [
  { key: 'grounding', prompt: 'Check the finding against the code and the whole design document. Is the gap real, or is it handled elsewhere in the document, or does the code already behave the way the design needs?' },
  { key: 'proportion', prompt: 'Judge the proposed change at the scope the design sets for itself. If the gap is real but the fix is oversized, keep the finding and give a smaller correction.' },
]

const LENSES = resolveLenses(A.lenses, BUILTIN_LENSES)

const accepted = [].concat(A.accepted ?? [])
const BACKGROUND = [
  A.cwd && `The code lives in the repository at ${A.cwd}.`,
  A.context && `Context from the caller:\n${A.context}`,
  accepted.length && `Deliberately accepted (do not re-litigate these, but do report it if a stated cost or justification is wrong):\n${accepted.map(a => `- ${a}`).join('\n')}`,
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
          section: { type: 'string', description: 'where in the document the problem sits' },
          claim: { type: 'string', description: 'what is wrong, stated precisely' },
          evidence: { type: 'string', description: 'path:line in the code, or the quoted passage of the document, that shows it' },
          proposed_change: { type: 'string' },
        },
        required: ['title', 'severity', 'section', 'claim', 'evidence', 'proposed_change'],
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
    evidence: { type: 'string', description: 'path:line or quoted passage that settles it' },
    correction: { type: 'string', description: 'unless refuted: the precise change the design should adopt' },
  },
  required: ['verdict', 'reasoning', 'evidence', 'correction'],
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

function critiquePrompt(lens) {
  return `You are the ${lens.key} critic of the design in ${A.doc}. Your job is to find where it is wrong, unsound, incomplete, untestable, or contradicts the code.
${BACKGROUND ? `\n${BACKGROUND}\n` : ''}
Your lens: ${lens.prompt}

Rules:
- Read-only. Do not edit any file.
- Every finding needs evidence you opened yourself: a path:line in the code, or the quoted passage of the document.
- Stay inside your lens; other critics cover the rest.
- Severity: blocker = the design cannot work or would ship a defect as written; important = a real gap that will cost rework if left; minor = real but cheap to fix later.
- At most ${CAP} findings, most severe first. Fewer, sharper findings beat a long list.
- List what you checked and found sound in \`checked\`, especially when you report nothing.`
}

function refutePrompt(lens, finding, angle) {
  return `A ${lens.key} critic reported this finding about the design in ${A.doc}. Your job is to refute it.
${BACKGROUND ? `\n${BACKGROUND}\n` : ''}
${JSON.stringify(finding, null, 2)}

Your angle: ${angle.prompt}

Read the cited code and passages yourself; do not trust the paraphrase. Then decide:
- REFUTED: the gap is not real, is handled elsewhere in the document, or rests on a misreading of the code. Quote what shows it.
- CONFIRMED: you checked it against the code and the document and it holds.
- PLAUSIBLE: it looks real, but you could not settle it either way.
A real finding is not refuted for being minor. Unless you refute it, give the precise correction the design should adopt. Do not edit any file.`
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
  lens => agent(critiquePrompt(lens), {
    label: `critique:${lens.key}`,
    phase: 'Critique',
    schema: FINDINGS,
    model: MODELS.critique,
    effort: EFFORT.critique,
  }),
  (critique, lens) => {
    if (!critique) {
      failedLenses.push(lens.key)
      return []
    }
    checked[lens.key] = critique.checked
    const sorted = [...critique.findings].sort(bySeverity)
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
  doc: A.doc,
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
  await agent(`Write this design-critique result to ${A.report}: Org if the path ends in .org, Markdown otherwise. One section per status in this order: confirmed, plausible, contested, unjudged, unverified, refuted. For each finding give its severity, lens, document section, claim, evidence, and the refuters' correction (or the proposed change when there is none), plus a one-line summary of the votes. Close with the failed lenses and what each lens checked. Write the file and return its path.

${JSON.stringify(result)}`, { label: 'report', phase: 'Report', model: MODELS.report, effort: EFFORT.report })
}

return result
