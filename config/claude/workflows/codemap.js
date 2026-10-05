export const meta = {
  name: 'codemap',
  description: 'Parallel read-only readers produce evidence-backed fact sheets on parts of a codebase, merged into one map',
  whenToUse: 'Understand a codebase area before designing or changing it. args: {context (required), readers, cwd, synthesize, models, effort, report}; a bare string is taken as context. Without readers, a planner splits the question into 4-7 of them.',
  phases: [
    { title: 'Plan', detail: 'only when args.readers is absent', model: 'sonnet' },
    { title: 'Read', detail: 'one read-only reader per area', model: 'sonnet' },
    { title: 'Synthesize', detail: 'merge fact sheets, resolve conflicts', model: 'opus' },
    { title: 'Report', detail: 'only when args.report names a file' },
  ],
}

const A = typeof args === 'string' ? { context: args } : (args ?? {})
if (!A.context) throw new Error('codemap: args.context (the question or the change being planned) is required')
const MODELS = { plan: 'sonnet', read: 'sonnet', synthesize: 'opus', report: 'sonnet', ...A.models }
const EFFORT = { plan: 'medium', read: 'high', synthesize: 'high', report: 'low', ...A.effort }

const CONTEXT = `${A.cwd ? `Repository: ${A.cwd}\n\n` : ''}What the caller needs to understand:\n${A.context}`

const PLAN = {
  type: 'object',
  properties: {
    readers: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          key: { type: 'string', description: 'short kebab-case name' },
          prompt: { type: 'string', description: 'the area to cover, then numbered questions to answer' },
          external: { type: 'boolean', description: 'true when the answer lives in a third-party project, not this repository' },
        },
        required: ['key', 'prompt', 'external'],
      },
    },
  },
  required: ['readers'],
}

const FACTS = {
  type: 'object',
  properties: {
    summary: { type: 'string', description: '3-6 sentences answering the questions asked' },
    facts: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          claim: { type: 'string' },
          evidence: { type: 'string', description: 'path:line you opened, with the load-bearing line quoted' },
        },
        required: ['claim', 'evidence'],
      },
    },
    change_points: { type: 'array', items: { type: 'string' }, description: 'path:line where the work in the context would have to change; empty if the context plans no change' },
    open_questions: { type: 'array', items: { type: 'string' } },
  },
  required: ['summary', 'facts', 'change_points', 'open_questions'],
}

const MAP = {
  type: 'object',
  properties: {
    summary: { type: 'string', description: 'the answer to the context, in one or two paragraphs' },
    areas: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          area: { type: 'string' },
          role: { type: 'string' },
          files: { type: 'array', items: { type: 'string' } },
        },
        required: ['area', 'role', 'files'],
      },
    },
    flows: { type: 'array', items: { type: 'string' }, description: 'data or control flows that matter for the context, each as a chain of path:line hops' },
    conflicts: {
      type: 'array',
      items: {
        type: 'object',
        properties: {
          between: { type: 'string' },
          resolution: { type: 'string' },
          evidence: { type: 'string' },
        },
        required: ['between', 'resolution', 'evidence'],
      },
    },
    change_points: { type: 'array', items: { type: 'string' } },
    open_questions: { type: 'array', items: { type: 'string' }, description: 'most consequential first' },
  },
  required: ['summary', 'areas', 'flows', 'conflicts', 'change_points', 'open_questions'],
}

let readers = A.readers
if (!readers) {
  phase('Plan')
  const plan = await agent(`${CONTEXT}

Plan a read-only survey that answers this. Look at the repository layout, build files and any CLAUDE.md, then split the work into 4-7 readers, each covering one distinct subsystem or concern, with numbered questions it must answer. Add a reader for the tests and one for docs and conventions when they bear on the question. If part of the answer lives in a third-party project, give it its own reader with external set to true. Do not answer the questions yourself.`, { label: 'plan', phase: 'Plan', schema: PLAN, model: MODELS.plan, effort: EFFORT.plan })
  if (!plan) throw new Error('codemap: the planner returned nothing; pass args.readers explicitly')
  readers = plan.readers
  log(`planned readers: ${readers.map(r => r.key).join(', ')}`)
}
// Results are keyed by reader, so a repeated key would silently merge two areas.
readers = readers.map((r, i) => readers.findIndex(x => x.key === r.key) === i ? r : { ...r, key: `${r.key}-${i + 1}` })

function readPrompt(r) {
  return `${CONTEXT}

You are reader "${r.key}", mapping one part of this. ${r.prompt}

Rules:
- Read-only. Do not edit files, do not run git commands that change anything, and do not run builds or test suites.
- Every fact carries a path:line you actually opened, with the load-bearing line quoted. Do not infer from names; open the code.
- Do not propose designs. What you could not settle goes in open_questions.`
}

phase('Read')
const sheets = await parallel(readers.map(r => () => agent(readPrompt(r), {
  label: `read:${r.key}`,
  phase: 'Read',
  schema: FACTS,
  model: r.model ?? MODELS.read,
  effort: EFFORT.read,
  ...(r.external ? { agentType: 'upstream-researcher' } : {}),
})))

const byReader = {}
const failed = []
readers.forEach((r, i) => {
  if (sheets[i]) byReader[r.key] = sheets[i]
  else failed.push(r.key)
})
if (failed.length) log(`readers that returned nothing: ${failed.join(', ')}`)
log(`${Object.keys(byReader).length}/${readers.length} readers returned, ${Object.values(byReader).reduce((n, s) => n + s.facts.length, 0)} facts`)

let synthesis = null
if ((A.synthesize ?? true) && Object.keys(byReader).length > 1) {
  phase('Synthesize')
  synthesis = await agent(`${CONTEXT}

Independent readers mapped parts of the codebase. Merge their fact sheets into one map. Where two sheets disagree, open the code, settle which is right, and record it under conflicts. Keep every path:line anchor you rely on; drop facts that do not bear on the context.${failed.length ? `\n\nThese readers returned nothing, so their areas are uncovered; say so in open_questions: ${failed.join(', ')}` : ''}

${JSON.stringify(byReader)}`, { label: 'synthesize', phase: 'Synthesize', schema: MAP, model: MODELS.synthesize, effort: EFFORT.synthesize })
}

const result = { context: A.context, readers: byReader, failed, synthesis }

if (A.report) {
  await agent(`Write this codebase map to ${A.report}: Org if the path ends in .org, Markdown otherwise. Lead with the synthesis (summary, areas, flows, conflicts, change points, open questions) when present, then one section per reader with its summary and facts, keeping every path:line anchor. Write the file and return its path.

${JSON.stringify(result)}`, { label: 'report', phase: 'Report', model: MODELS.report, effort: EFFORT.report })
}

return result
