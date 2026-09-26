# Loop prompt

Substitute every `{{...}}` placeholder, then pass the text below `---` verbatim as the prompt of the
`loop` skill with no interval. It is written to survive context compaction: every fact it needs comes
from the helper, not from memory.

- `{{SKILL_DIR}}`: this skill's base directory, absolute
- `{{MAX_ATTEMPTS}}`: `max_attempts` from `overnight.nu config`

---

Overnight orchestrator cycle. You coordinate; you never implement. Every turn ends with exactly one ScheduleWakeup call.

Helper: run `nu {{SKILL_DIR}}/scripts/overnight.nu <command>` from the repository root. Batch workflow: `{{SKILL_DIR}}/workflow.js`. Query commands print JSON; mutating commands print nothing on success and exit non-zero with the error on failure.

1. Deadline. Run `overnight.nu deadline`. If `passed` is true: run `overnight.nu report append "run ended: <reason>"`, then call ScheduleWakeup with `stop: true`. Nothing else this turn.

2. Results. If a Workflow completion notification arrived this turn, handle every entry of its `results`, one after another; an error on one entry never skips the others. Each entry carries `id`, `title`, `branch`, `status`, and on success `pr_body`, on failure `failure`.
   - `ready`: run `overnight.nu pr open <id> --branch <branch> --title "<title>" --body "<pr_body>"`. On success, `overnight.nu report append "#<id> PR opened: <url>"`. If it errors, run `overnight.nu mark <id> blocked --note "branch <branch> is ready but PR creation failed: <error>"` and `report append` the same line, so the branch is findable in the morning instead of the task sitting active forever.
   - `failed`: run `overnight.nu attempt <id>` once; it prints the attempt count n. If n is below {{MAX_ATTEMPTS}}, run `overnight.nu mark <id> queued --note "attempt n failed: <failure>"`; otherwise run `overnight.nu mark <id> blocked --note "gave up after n attempts: <failure>"`. Then `overnight.nu report append` a one-line summary.

3. In flight. If you launched a Workflow in an earlier cycle and its completion notification has not arrived, call ScheduleWakeup with 1800 seconds, `noop: true`, reason "batch still running". Nothing else this turn. Never launch a second batch on top of a running one and never poll it.

4. Next batch. Run `overnight.nu next` (its default limit is the batch size from `run start`). If it prints `[]`: run `overnight.nu report append "queue empty"` and call ScheduleWakeup with `stop: true`. Otherwise run `overnight.nu mark <id> active` for each task, then call the Workflow tool once with `scriptPath` set to `{{SKILL_DIR}}/workflow.js` and `args` set to `{"tasks": <the array next printed>, "config": <the object overnight.nu config prints>}`. Then call ScheduleWakeup with 1800 seconds, `noop: false`, reason "batch of N launched". The completion notification will wake you sooner.

Rules. Never run git, cargo, pnpm, nix or edit files yourself; the workflow's agents do that in worktrees. Never merge, never push. If `deadline`, `config` or `next` itself errors, `report append` the error text and call ScheduleWakeup with 300 seconds instead of improvising a fix. The only reasons to stop the loop are the deadline, an empty queue, or `deadline` reporting `stopped`.
