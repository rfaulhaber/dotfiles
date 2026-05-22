---
name: org-mode
description: Use when writing or editing Emacs Org mode content — `.org` files, fenced org code blocks, or when the user asks for documentation, notes, or task lists in org-mode format. Encodes conventions that keep org documents interactive in Emacs (cycleable TODO state, statistics cookies for parent headings, agenda-friendly structure).
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

## Rule 2: Parent headings with TODO/checkbox children get a statistics cookie

When a heading has TODO sub-headings or checkbox children, append a **statistics cookie** — `[/]` (count) or `[%]` (percent) — to the parent headline. Write the cookie *empty*; Emacs fills it in on file open and whenever a child changes state.

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

## Quick reference

| What | Syntax | Notes |
|---|---|---|
| Open task | `* TODO Description` | Cycle state with `C-c C-t` |
| Completed task | `* DONE Description` | Same family of keywords |
| Parent counter (count) | `* Parent [/]` | Auto-fills to e.g. `[1/3]` |
| Parent counter (percent) | `* Parent [%]` | Auto-fills to e.g. `[33%]` |
| Checkbox sub-item | `- [ ] item` / `- [X] item` | Toggle with `C-c C-c` |

## When NOT to apply these rules

- The user explicitly asks for a flat prose list, README-style bullets, or a non-org format.
- The document is a notes/explanatory file with no task semantics — don't manufacture TODO headlines just because a sentence happens to describe future work.
- Output is going somewhere that doesn't render org (e.g., a GitHub issue body) — use the appropriate format for that destination instead.
