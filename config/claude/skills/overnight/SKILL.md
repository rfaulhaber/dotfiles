---
name: overnight
description: Use when the user wants Claude to work through a backlog unattended for hours — "work the issue list overnight", "keep going until 7am", "run the queue while I sleep", "set up an overnight run" — and when checking on, stopping, or reviewing the morning after such a run. Also use when asked how the overnight queue, its labels, or its plan file work. Not for a single long task in the current turn; that is ordinary work.
---

# Overnight runs

## Overview

An interactive session drives a self-paced `/loop`. Each wakeup pulls a batch from a durable queue,
fans it out through a Workflow of sonnet implementers and opus verifiers in isolated worktrees, and
lands each success as a PR for the human to review in the morning. Nothing about the run lives in
the conversation: the queue, the deadline, the attempt counters and the report are labels and files,
so compaction, a restart, or a usage-limit wait costs nothing.

Why an open interactive session rather than a headless script: only an interactive session signed
in with a claude.ai subscription waits out a usage limit and resumes on its own (the
`autoContinueAtUsageLimit` setting, on by default, re-arms twice). `claude -p`, background sessions
and agent-team teammates fail the turn instead. Leave the terminal open.

## Pieces

| Piece | Where | Role |
|---|---|---|
| `scripts/overnight.nu` | this skill | queue, run state, report; both backends |
| `workflow.js` | this skill | one batch: implement, verify, one fix round |
| `references/loop-prompt.md` | this skill | the `/loop` prompt, with placeholders |
| `.claude/overnight.toml` | target repo, tracked | project config |
| `.claude/overnight/` | target repo, ignored | `run.json` and `report.md` |
| `overnight-plan.toml` | target repo, tracked | the queue for the `file` backend |

Invoke the helper as `nu <skill dir>/scripts/overnight.nu <command>` from the repository root.
`--help` lists commands. Query commands (`config`, `candidates`, `show`, `queue list`, `next`,
`deadline`, `run status`, `attempt`) print JSON on stdout; mutating commands (`queue add`,
`queue remove`, `mark`, `pr open`, `labels ensure`, `run start`, `run stop`, `report append`) print
nothing on success and exit non-zero with the error on failure. Diagnostics go to stderr.

Two queue backends share one command surface. **github**: a task is an issue, its state is a
`<prefix>:<state>` label, and the acceptance criteria live in a marked triage comment on the issue.
**file**: a task is a row in `overnight-plan.toml`. States are the same in both: `queued`,
`active`, `review` (PR open), `blocked`, `done`.

## Config

Create `.claude/overnight.toml` in the target repository when it is missing. Ask the user only for
what cannot be read from the repo: which backend, and which check commands are the hermetic subset
of CI.

```toml
backend = "github"            # or "file"
repo = "owner/name"           # github backend; also enables PR creation for the file backend
label_prefix = "overnight"
base_branch = "main"
branch_prefix = "overnight/"
batch = 3                     # tasks per cycle; each runs one agent at a time
max_attempts = 2
skip_labels = ["status:in-progress", "status:on-hold", "status:waiting", "kind:decision", "kind:milestone"]
commit_style = "type(scope): summary"

# Keys are what a task's `checks` list names. Run from the worktree root.
[checks]
rust = "nix develop --command bash -c 'cargo fmt --all --check && cargo clippy --all-targets -- -D warnings && cargo test --workspace'"

# Exported for every check. Worktrees would otherwise each build from scratch.
[env]
CARGO_TARGET_DIR = "/home/me/.cache/overnight/target"
```

Checks are the hermetic subset of CI. Anything that needs a running database or browser stays CI's
job on the PR; the verifier and the implementer only need a signal that is cheap enough to run in
every worktree.

## `/overnight plan [--deadline HH:MM]`

Triage is the one step that needs the expensive model and the human. Do it in the evening, together.

1. `overnight.nu config` must succeed. For the github backend, run `overnight.nu labels ensure`
   once; it is idempotent.
2. github: `overnight.nu candidates` lists open issues that carry no overnight label and none of
   `skip_labels`. Read `overnight.nu show <id>` for each one worth considering. file: read the
   plan file and the user's list.
3. Choose tasks that can succeed without a human. The bar:
   - The outcome is checkable by a test or a command. "Decide", "design", "investigate" are not.
   - Small or medium scope, one area, files unlikely to collide with the other queued tasks.
   - No production credentials, deployments, or data migrations.
   - Dependencies point only at tasks already closed. A task whose blocker is in the queue will
     wait for a later night; the queue only releases a task once its blockers are `done`, because
     stacking branches on unmerged PRs is where unattended runs go wrong.
4. Write acceptance criteria for each task as the implementer's contract: the behavior, the files or
   modules involved when known, and the tests that must exist. Name the check keys that apply.
   Then `overnight.nu queue add <id> --criteria "<text>" --checks "rust,web" --blocked-by "12,34"`
   (file backend also needs `--title`).
5. Show the user `overnight.nu queue list --table` and adjust until they approve (`queue remove
   <id>` takes one back out). A dozen tasks is a full night; the weekly usage cap, not the clock,
   is usually what ends the run.
6. `overnight.nu report append "planned N tasks: #a #b ..."`.

## `/overnight run [deadline]`

Preconditions, all of them:

- The session was started in the repository root with a permission mode that does not prompt
  (`claude --permission-mode auto`, or the classifier already active).
- The working tree is clean and on `base_branch`. Worktrees branch from local HEAD, so pull first.
- `.claude/overnight/` is in `.gitignore`.
- The repository's `.claude/settings.json` denies force-pushes, pushes to the base branch, and PR
  merges. The prompts forbid these too; the deny list is what holds if a prompt is ignored.

  ```json
  {
    "permissions": {
      "deny": [
        "Bash(git push --force*)", "Bash(git push -f*)", "Bash(git push --delete*)",
        "Bash(git push origin main*)", "Bash(git push * main)",
        "Bash(git reset --hard*)", "Bash(git branch -D*)", "Bash(gh pr merge*)"
      ]
    }
  }
  ```

- github backend, or file backend with `repo` set: `overnight.nu config` prints the config, which
  proves the GitHub token is in the environment, and `overnight.nu labels ensure` succeeds, which
  proves it can write. A fine-grained token answers a missing permission with 403 and the helper
  prints the permission it wanted; the backend needs `issues=write` and `pull_requests=write` on
  the repository.
- If the weekly cap matters, switch the session to a cheaper model before starting (`/model`);
  the orchestrator's own work per cycle is a few helper calls and one Workflow launch.

Then:

1. `overnight.nu run start --deadline <HH:MM or RFC 3339> [--batch N]`. A bare time means its
   next occurrence. The batch size recorded here is what `next` returns per cycle; it falls back
   to config `batch`.
2. Read `references/loop-prompt.md`, substitute the placeholders, and invoke the `loop` skill with
   the resulting text and no interval.
3. Tell the user: leave the terminal open; Esc interrupts a turn; `/overnight stop` ends the run
   cleanly at the next wakeup.

The loop prompt is authoritative for what a cycle does. In short: check the deadline, process any
finished batch into PRs or retries, launch the next batch through `workflow.js`, and reschedule.
The Workflow runs in the background and its completion notification wakes the loop; the 1800-second
wakeup is only a fallback.

## `/overnight status`, `/overnight stop`, `/overnight report`

- status: `overnight.nu run status` for the deadline and attempt counters, `overnight.nu next
  --limit 50` for what is still queued. For github, list issues by the `overnight:active`,
  `overnight:review` and `overnight:blocked` labels; for file, read the plan file.
- stop: `overnight.nu run stop`. The loop exits at its next wakeup; use Esc for immediate.
- report: `overnight.nu report show`, then `overnight.nu queue list --table` for PRs opened and
  tasks blocked with their notes. A `blocked` note that says the branch is ready but PR creation
  failed means the work exists on the named branch and only the PR is missing. Tasks still
  `active` after the run ended are orphans of a lost workflow notification or an interrupted
  cycle; re-queue them with `overnight.nu mark <id> queued` or leave them for the user to judge.

## Guardrails built in

- Every implementer and verifier works in its own git worktree and pushes only its own branch.
- A task gets `max_attempts` tries, then is marked `blocked` with the reason and the loop moves on.
- The deadline lives in `run.json` and is checked at the top of every cycle. One cycle can overrun
  it by at most the fallback wakeup.
- Usage-limit waits are the session's own: at most two automatic resumes, then the run stalls until
  someone runs `/rate-limit-options`. Plan for one night, not two.
- Nothing merges. PRs say so in their body.

## Common mistakes

| Mistake | Consequence | Instead |
|---|---|---|
| Running the loop from `claude -p` or a cron | Dies at the first usage limit | Interactive session left open |
| Queuing a task whose blocker is another queued task | Sits until the blocker is merged by a human | Queue independent work; chain across nights |
| Vague criteria ("improve X") | Implementer guesses, verifier rejects, two attempts burned | Behavior, files, tests to exist |
| Checks that need a database | Every attempt fails on setup | Hermetic subset; CI runs the rest on the PR |
| Setting `autoContinueAtUsageLimit` in the repo's `.claude/settings.json` | Project-level values are ignored, and if they are the only source the feature turns off | Leave the user-level default |
| Large batch on a Rust workspace | Worktrees compile in parallel, disk and CPU saturate | `batch = 2` or 3, shared `CARGO_TARGET_DIR` |
