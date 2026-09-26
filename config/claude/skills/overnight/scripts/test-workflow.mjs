// Drives ../workflow.js with stubbed agent()/pipeline() so the batch logic can
// be checked without spawning anything. Needs node: `nix run nixpkgs#nodejs_22 -- test-workflow.mjs`
//   nix run nixpkgs#nodejs_22 -- test-workflow.mjs
import { readFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import assert from 'node:assert/strict'

const here = dirname(fileURLToPath(import.meta.url))
const src = readFileSync(join(here, '..', 'workflow.js'), 'utf8').replace(/^export const meta/m, 'const meta')
const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor
const run = new AsyncFunction('args', 'agent', 'pipeline', 'parallel', 'log', 'phase', src)

// Faithful enough to the documented contract: each item flows through every
// stage independently; a throwing stage drops the item to null.
async function pipeline(items, ...stages) {
  return Promise.all(items.map(async (item, i) => {
    let prev = item
    try { for (const s of stages) prev = await s(prev, item, i) } catch { return null }
    return prev
  }))
}
const parallel = thunks => Promise.all(thunks.map(t => t().catch(() => null)))
const log = () => {}
const phase = () => {}

const config = {
  backend: 'github', base_branch: 'main',
  checks: { rust: 'cargo test', web: 'pnpm test' },
  env: { CARGO_TARGET_DIR: '/tmp/t' },
  commit_style: 'type(scope): summary',
}
const task = (id, extra = {}) => ({ id, title: `task ${id}`, body: 'body', criteria: 'crit', checks: ['rust'], branch: `overnight/${id}-task`, attempts: 0, url: `https://x/${id}`, ...extra })

// Scenario table keyed by task id: what each agent call returns.
const script = {
  A: { implement: { status: 'ready', branch: 'overnight/A-task', summary: 'did A', checks_run: ['rust'] }, verify: { approved: true, findings: [], summary: 'fine' } },
  B: { implement: { status: 'failed', branch: 'overnight/B-task', summary: '', failure: 'tests red' } },
  C: { implement: { status: 'ready', branch: 'overnight/C-task', summary: 'did C' }, verify: { approved: false, findings: ['missing test'], summary: 'no' }, fix: { status: 'ready', branch: 'overnight/C-task', summary: 'fixed C' }, reverify: { approved: true, findings: [], summary: 'ok now' } },
  D: { implement: { status: 'ready', branch: 'overnight/D-task', summary: 'did D' }, verify: { approved: false, findings: ['f1'], summary: 'no' }, fix: { status: 'ready', branch: 'overnight/D-task', summary: 'tried' }, reverify: { approved: false, findings: ['still f1'], summary: 'no' } },
  E: { implement: null },
}
const calls = []
async function agent(prompt, opts) {
  const [kind, id] = opts.label.split(':')
  calls.push({ kind, id, opts, prompt })
  return script[id][kind] ?? null
}

// empty batch
assert.deepEqual(await run({ tasks: [], config }, agent, pipeline, parallel, log, phase), { results: [] })

const out = await run({ tasks: ['A', 'B', 'C', 'D', 'E'].map(id => task(id)), config }, agent, pipeline, parallel, log, phase)
const byId = Object.fromEntries(out.results.map(r => [r.id, r]))

assert.equal(byId.A.status, 'ready')
assert.equal(byId.A.title, 'task A', 'ready results carry the title the PR needs')
assert.equal(byId.B.title, 'task B', 'failed results carry the title too')
assert.match(byId.A.pr_body, /^Closes #A/)
assert.match(byId.A.pr_body, /did A/)
assert.equal(byId.B.status, 'failed')
assert.match(byId.B.failure, /tests red/)
assert.equal(byId.C.status, 'ready')
assert.equal(byId.C.summary, 'fixed C')
assert.equal(byId.D.status, 'failed')
assert.match(byId.D.failure, /not approved after one fix round.*still f1/)
assert.equal(byId.E.status, 'failed')
assert.match(byId.E.failure, /returned nothing/)

// agent wiring: tiers, isolation, verifier agent type, one fix round at most
const kindsFor = id => calls.filter(c => c.id === id).map(c => c.kind)
assert.deepEqual(kindsFor('A'), ['implement', 'verify'])
assert.deepEqual(kindsFor('B'), ['implement'])
assert.deepEqual(kindsFor('C'), ['implement', 'verify', 'fix', 'reverify'])
assert.deepEqual(kindsFor('D'), ['implement', 'verify', 'fix', 'reverify'])
for (const c of calls) {
  assert.equal(c.opts.isolation, 'worktree', `${c.opts.label} must run in a worktree`)
  if (c.kind === 'implement' || c.kind === 'fix') assert.equal(c.opts.model, 'sonnet')
  if (c.kind === 'verify' || c.kind === 'reverify') assert.equal(c.opts.agentType, 'verifier')
}

// prompt content the agents depend on
const impl = calls.find(c => c.kind === 'implement' && c.id === 'A').prompt
for (const needle of ['overnight/A-task', 'origin/main', 'rust: cargo test', 'CARGO_TARGET_DIR=/tmp/t', 'type(scope): summary', 'crit', 'never open a PR']) {
  assert.ok(impl.includes(needle), `implement prompt lacks: ${needle}`)
}
assert.ok(!impl.includes('web: pnpm test'), 'implement prompt must only list the checks the task names')
const fix = calls.find(c => c.kind === 'fix' && c.id === 'C').prompt
assert.ok(fix.includes('- missing test'))

console.log('workflow tests passed')
