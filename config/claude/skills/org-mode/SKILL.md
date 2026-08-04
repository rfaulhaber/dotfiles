---
name: org-mode
description: Use when writing or editing Emacs Org mode content — `.org` files, fenced org code blocks, or when the user asks for documentation, notes, or task lists in org-mode format. Encodes conventions that keep org documents interactive in Emacs (cycleable TODO state, statistics cookies, agenda-visible dates, automatic completion logging), org-native markup instead of markdown, and file naming/preamble conventions.
---

# Org Mode Skill

The user is an Emacs / Org mode user. Documents you write are opened in Emacs and operated on with the standard Org keybindings (`C-c C-t` to cycle TODO state, `C-c C-c` to toggle checkboxes, `C-c #` to refresh statistics cookies, agenda views, etc.). Write org documents so those workflows work out of the box — don't produce text that *looks* like a task list but is invisible to Org's machinery.

## Rule 1: Task lists must use real TODO headlines

Whenever the document contains a list of tasks/work items, write each task as an **Org headline with a TODO keyword**, not as plain text and not as a numbered/bulleted list of strings.

```org
* TODO Set up the new build pipeline
* TODO Wire up the deploy job
* DONE Update the CI config
```

Reason: a real `* TODO ...` headline can be:

- Cycled through workflow states with `C-c C-t` (or `S-<right>` / `S-<left>`).
- Picked up by `org-agenda`, refiled (`C-c C-w`), archived (`C-c C-x C-a`).
- Scheduled, deadlined, prioritized, tagged.

A paragraph that reads `TODO: do the thing` or a list item `- TODO: do the thing` is just text — none of those keybindings or views apply to it.

### Avoid

```org
Tasks:
1. Set up the new build pipeline
2. Wire up the deploy job
```

```org
- TODO: set up the new build pipeline
- TODO: wire up the deploy job
```

Both render fine visually but Emacs cannot operate on them as TODOs.

### Checkbox lists are fine for fine-grained sub-items

`- [ ] item` checkboxes are still interactive (toggled with `C-c C-c`) and are appropriate for *sub-items inside* a TODO headline — e.g., a release checklist nested under one parent task. They are **not** a substitute for top-level TODO items; use `* TODO` for those.

### Use the full keyword set when it adds signal

This Emacs runs Doom's default keywords: `TODO PROJ LOOP STRT WAIT HOLD IDEA | DONE KILL`. `TODO`/`DONE` are always safe, but in a document tracking live work the intermediate states carry real information — `STRT` (in progress), `WAIT`/`HOLD` (blocked), `KILL` (cancelled, distinct from done). Prefer them over prose like "(in progress)" appended to a headline. They are Doom-specific: in org text destined for a vanilla-org reader, stick to `TODO`/`DONE`.

### Scheduling, deadlines, priorities

The planning line goes immediately after the headline; the priority goes in the headline after the keyword:

```org
* TODO [#A] Migrate the store pool
  SCHEDULED: <2026-08-03 Mon> DEADLINE: <2026-08-15 Sat>
```

- **Active** timestamps `<2026-08-03 Mon>` put the entry in the agenda; **inactive** ones `[2026-07-31 Fri]` are for logs and references and stay out of it. Choose deliberately — a "verified on" date should be inactive.
- Get the weekday right or omit it: `<2026-08-03>` is valid and Emacs inserts the day name on first edit, but a *wrong* day name is displayed as written and misleads until the timestamp is re-edited.

### Progress logging: when completion time matters, declare it in the file

When it's important to track *when* tasks get finished, don't hand-maintain dates — declare logging in the file and Emacs stamps state changes itself as the user cycles keywords. Marking a task DONE then appends a planning line automatically:

```org
* DONE Migrate the store pool
  CLOSED: [2026-08-04 Tue]
```

The lightest way to enable this is a startup keyword, which layers onto whatever TODO keywords are already active (the Doom set included) without redefining them:

```org
#+STARTUP: logdone
```

`logdone` records the `CLOSED:` timestamp; `lognotedone` also prompts for a note.

For per-state control, add markers to a `#+TODO:` line — `!` logs a timestamp on entering that state, `@` prompts for a timestamped note, and a marker after `/` fires on *leaving* the state (only when the target state doesn't log on its own):

```org
#+TODO: TODO(t) STRT(s) WAIT(w@/!) | DONE(d!) KILL(k@)
```

Here entering `WAIT` records why the task blocked, leaving it stamps when it unblocked, `DONE` gets a timestamp, and `KILL` records why the task was cancelled. Caution: a `#+TODO:` line *replaces* the buffer's keyword set, so declare every keyword the document needs — not just the logged ones.

A single headline can override all of this with the `:LOGGING:` property. Any non-empty value first resets logging for that entry, then applies only what's listed; `nil` turns logging off entirely:

```org
* TODO Chase the flaky migration job
  :PROPERTIES:
  :LOGGING: TODO(!) DONE(!)
  :END:
```

These in-file settings travel with the document, so completion tracking works regardless of the reader's global `org-log-done`. Reference: https://orgmode.org/guide/Progress-Logging.html (and "Tracking TODO state changes" in the full manual for the `:LOGGING:` property).

## Rule 2: Parent headings with TODO/checkbox children get a statistics cookie

When a heading has TODO sub-headings or checkbox children, append a **statistics cookie** — `[/]` (count) or `[%]` (percent) — to the parent headline. Write the cookie *empty*; Emacs fills it in whenever a child changes state, or on demand with `C-c #`. (It does not update on mere file open — a freshly opened file shows the literal `[/]`, which is fine.)

### With TODO sub-headings

```org
* Project Alpha [/]
** TODO Design the schema
** TODO Implement migrations
** DONE Write the RFC
```

Emacs renders this as `* Project Alpha [1/3]`.

### With checkbox children

```org
* Release checklist [/]
- [ ] Tag the release
- [ ] Publish to the registry
- [X] Update changelog
```

Renders as `* Release checklist [1/3]`.

### `[%]` when proportion is more meaningful than a count

```org
* Migration progress [%]
** TODO Service A
** TODO Service B
** DONE Service C
** DONE Service D
```

Renders as `* Migration progress [50%]`.

### Don't pre-fill the numbers

Write `[/]` or `[%]`, not `[0/3]` or `[0%]`. Emacs computes the values; hand-written counts go stale the moment the user toggles anything.

### Parent headings that are themselves tasks get a keyword, not a status annotation

A keyword and a cookie compose on the same headline. When a parent heading — a phase, a milestone — is a work item in its own right, give it its own TODO keyword and mark completion by cycling the keyword, exactly like any other task:

```org
* DONE Implement Phase 1 [/]
** DONE Set up scaffolding
** DONE Write fixtures
* TODO Implement Phase 2 [/]
** DONE Extract the parser
** TODO Port the CLI
```

Never encode the parent's status as text in the headline:

```org
* Implement Phase 1 [DONE 2026-08-01]
```

That annotation is inert — `C-c C-t` can't cycle it, the agenda can't see it, and it doesn't count in any ancestor's cookie. If the completion date is worth keeping, put it where org puts it: a `CLOSED: [2026-08-01 Sat]` planning line under the headline — the same line org writes automatically when progress logging is on (see *Progress logging* under Rule 1).

## Rule 3: New files — snake_case names, a preamble matched to formality

Name any `.org` file you create in lowercase snake_case: `nvme_swap_plan.org`, `atlas_migration.org` — not `NVME-SWAP-PLAN.org`, `nvme-swap-plan.org`, or `NvmeSwapPlan.org`. This matches org ecosystem convention (org-roam's default slugs join title words with underscores).

- Exception: `README.org` keeps its conventional all-caps name — that's a forge/repo convention that outranks the org one.
- The rule governs files you create; don't rename existing documents to match unless asked.

### Preamble scales with formality

- Project documentation, plans committed to a repo, anything shared or exported: full preamble — `#+TITLE:`, `#+AUTHOR:`, plus `#+STARTUP: overview` when the document is long enough that opening folded helps.
- Personal working notes: `#+TITLE:` at most. Don't front-load metadata a working document doesn't need.

## Rule 4: Org markup, not markdown

Markdown is the habit to unlearn — inside a `.org` file it is inert text at best. Translate reflexes:

| Markdown habit | Org form |
|---|---|
| `# Heading` / `## Subheading` | `* Heading` / `** Subheading` |
| `**bold**` | `*bold*` |
| `*italic*` / `_italic_` | `/italic/` |
| `` `code` `` | `~code~` for code; `=verbatim=` for filenames, keys, literal values |
| `[text](url)` | `[[url][text]]` |
| `` ```lang `` fenced block | `#+begin_src lang` … `#+end_src` |
| `> quote` | `#+begin_quote` … `#+end_quote` |

- Pasted command output belongs in `#+begin_example` … `#+end_example`, not a src block with an invented language.
- A bare `_` in prose becomes a subscript on export — one more reason filenames and identifiers always get wrapped in `=...=`.

## Quick reference

| What | Syntax | Notes |
|---|---|---|
| Open task | `* TODO Description` | Cycle state with `C-c C-t` |
| Completed task | `* DONE Description` | Same family of keywords |
| Parent counter (count) | `* Parent [/]` | Auto-fills to e.g. `[1/3]` |
| Parent counter (percent) | `* Parent [%]` | Auto-fills to e.g. `[33%]` |
| Parent that is itself a task | `* TODO Parent [/]` | Keyword and cookie compose; cycle to `DONE`, never `[DONE <date>]` in the text |
| Checkbox sub-item | `- [ ] item` / `- [X] item` | Toggle with `C-c C-c` |
| Priority | `* TODO [#A] Description` | `A`–`C`; unmarked defaults to `B` |
| Schedule / deadline | `SCHEDULED: <2026-08-03 Mon>` | Line under the headline; `DEADLINE:` may share it |
| Active timestamp | `<2026-08-03 Mon>` | The agenda sees it |
| Inactive timestamp | `[2026-07-31 Fri]` | Reference only; the agenda ignores it |
| Log completion time | `#+STARTUP: logdone` | Emacs appends `CLOSED: [...]` when a task turns DONE |
| Per-state logging | `#+TODO: ... WAIT(w@/!) \| DONE(d!)` | `!` timestamp, `@` note; after `/` fires on leaving |
| Code block | `#+begin_src nix` … `#+end_src` | Never markdown fences |

## When NOT to apply these rules

- The user explicitly asks for a flat prose list, README-style bullets, or a non-org format.
- The document is a notes/explanatory file with no task semantics — don't manufacture TODO headlines just because a sentence happens to describe future work.
- Output is going somewhere that doesn't render org (e.g., a GitHub issue body) — use the appropriate format for that destination instead.
