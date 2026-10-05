---
name: usage-review
description: Use when asked to review Claude Code usage, spend or trends — "do a usage review", "where are my tokens going", "what skills or workflows should I create", "how have I been using Claude" — or when planning the next periodic review. Extracts every transcript and the prompt history into one summary, prints the standard tables, and ends in a dated org write-up with ranked recommendations.
---

# Usage review

A periodic pass over how Claude Code is used across every project, looking for skills or saved
workflows to create, cheaper ways to run the expensive work, and friction that keeps recurring.
Earlier reviews are `notes/claude_usage_review_<yyyy>_<mm>.org` in the dotfiles repo
(gitignored), each with a matching `claude-usage-review-*` memory. Read the latest one first; its
recommendations are the baseline this review checks.

## Data

- `~/.claude/projects/<project>/<session>.jsonl` and `<session>/subagents/*.jsonl` hold the full
  transcripts: tool calls, errors, agents, models, `cost-state`. They are pruned after
  `cleanupPeriodDays` (set to 120 in the claude module), so nothing older survives.
- `~/.claude/history.jsonl` holds every prompt with its timestamp and project and is never
  pruned. It is the only source for a period whose transcripts are gone.
- Workflow agents write their transcripts under `<session>/subagents/workflows/wf_*/`, and they
  are most of the subagent volume. extract.py globs `subagents/**/agent-*.jsonl` to include
  them; the 2026-10 review read only the top level, so its subagent counts are low.
- Costs are Claude Code's list-price estimates from `cost-state`, not what the subscription
  billed. Only main sessions carry `cost-state`, and their totals already include their
  subagents. A forked session carries its parent's cost, so sums run slightly high. Say both in
  the write-up.
- Saved workflows are registered as skills, so running one by name can show up under Skill
  invocations instead of Workflow calls.

## Steps

1. **Extract** into the scratchpad. It reads about a gigabyte of transcripts in seconds:
   `python3 ~/.claude/skills/usage-review/scripts/extract.py <scratchpad>/sessions.json --since <window start>`
2. **Standard tables**:
   `python3 ~/.claude/skills/usage-review/scripts/summarize.py <scratchpad>/sessions.json --since <window start>`
   Cost by project and month, per-model usage, tools in main sessions vs subagents, Bash command
   classes and ssh failures, Agent calls by type and model, Skill invocations, error clusters,
   Workflow calls, edits by directory, hooks, prompts per month, and `/usage` checks.
3. **Dig where the tables point.** Query the JSON directly for specifics: the error lines behind
   a cluster, the sessions behind a cost spike, the scripts behind a recurring workflow shape.
   Delegate reads that would pull large transcript excerpts into context.
4. **Cluster the prompts.** Dump the window's prompts from history.jsonl and hand the clustering
   to a sonnet agent: themes with counts, phrasings repeated verbatim, reminders the user keeps
   restating. A repeated reminder is the strongest evidence for a CLAUDE.md line or a skill.
5. **Write it up** as `notes/claude_usage_review_<yyyy>_<mm>.org`, in the previous review's shape:
   context and caveats, what the data says, wins since the last review, friction with evidence,
   then recommendations as `** TODO [#A]`/`[#B]`/`[#C]` headlines ranked by evidence times
   payoff, each with its evidence line. Update the review memory to match.

## What to look for

- Where spend concentrates: projects, models, and stages (Workflow agents, 1M-context models).
- Work authored from scratch over and over: the shape of a saved workflow or a skill.
- Error clusters by command class: ssh into nushell hosts, a full temp filesystem, API rate
  limits, permission denials.
- Installed skills and plugins with no invocations. Hook-driven plugins never show up as
  invocations, so judge those by their hooks instead.
- Rituals typed verbatim many times: a project skill or a CLAUDE.md rule.
- Whether the last review's fixes held: the counts they targeted should have dropped.
