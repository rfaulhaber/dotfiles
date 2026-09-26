#!/usr/bin/env nu
# Queue, run-state and report helper for the overnight skill.
#
# Two queue backends share one command surface:
#   github  a task is an issue; its state is an `<prefix>:<state>` label and
#           triage metadata lives in a marked comment on the issue
#   file    a task is a row in a TOML plan file committed to the repo
# Run state (deadline, attempt counters) and the report always live under
# .claude/overnight/ so a backend swap never loses a night's bookkeeping.
#
# stdout carries data, JSON unless a command says otherwise; stderr carries
# diagnostics. Nothing here ever prints the GitHub token.

const CONFIG_PATH = ".claude/overnight.toml"
const STATE_DIR = ".claude/overnight"
const STATES = [queued active review blocked done]
const TRIAGE_MARK = "<!-- overnight:triage -->"
const API = "https://api.github.com"

def defaults [] {
  {
    backend: "file"
    plan_file: "overnight-plan.toml"
    label_prefix: "overnight"
    base_branch: "main"
    branch_prefix: "overnight/"
    batch: 3
    max_attempts: 2
    skip_labels: ["status:in-progress" "status:on-hold" "status:waiting" "kind:decision" "kind:milestone"]
    checks: {}
    env: {}
  }
}

def load-config [] {
  if not ($CONFIG_PATH | path exists) {
    error make {msg: $"($CONFIG_PATH) not found; run `/overnight plan` first"}
  }
  let cfg = (defaults | merge (open $CONFIG_PATH))
  if $cfg.backend == "github" and ($cfg | get -o repo | default "" | is-empty) {
    error make {msg: 'backend = "github" needs repo = "owner/name" in .claude/overnight.toml'}
  }
  if $cfg.backend not-in [github file] {
    error make {msg: $"unknown backend ($cfg.backend); use github or file"}
  }
  $cfg
}

def check-state [state: string] {
  if $state not-in $STATES {
    error make {msg: $"unknown state ($state); one of ($STATES | str join ', ')"}
  }
}

def slug [text: string] {
  $text
  | str lowercase
  | str replace -ra '[^a-z0-9]+' '-'
  | str trim -c '-'
  | str substring 0..40
  | str trim -c '-'
}

def csv-list [raw] {
  $raw | default "" | split row "," | each { str trim } | where ($it | is-not-empty)
}

# ---- run state -------------------------------------------------------------

def state-path [] { $STATE_DIR | path join run.json }

def load-state [] {
  if (state-path | path exists) {
    open (state-path)
  } else {
    {attempts: {}, stopped: false}
  }
}

def save-state [st: record] {
  mkdir $STATE_DIR
  $st | to json | save -f (state-path)
}

# A bare HH:MM means the next occurrence of that wall-clock time, so an
# evening `run start --deadline 07:00` lands on tomorrow morning.
def parse-deadline [raw: string] {
  if ($raw =~ '^\d{1,2}:\d{2}$') {
    let now = (date now)
    let candidate = ($"($now | format date '%Y-%m-%d') ($raw)" | into datetime -z local)
    if $candidate > $now { $candidate } else { $candidate + 1day }
  } else {
    $raw | into datetime
  }
}

def "main run start" [
  --deadline: string  # RFC 3339 datetime, or HH:MM for the next occurrence
  --batch: int        # tasks per cycle; defaults to config `batch`
] {
  let cfg = (load-config)
  if ($deadline | default "" | is-empty) {
    error make {msg: "--deadline is required"}
  }
  let dl = (parse-deadline $deadline)
  if $dl <= (date now) {
    print -e $"warning: deadline ($dl | format date '%+') is already in the past"
  }
  let st = {
    deadline: ($dl | format date "%+")
    started_at: ((date now) | format date "%+")
    batch: ($batch | default $cfg.batch)
    attempts: {}
    stopped: false
  }
  save-state $st
  $st | to json
}

def "main run stop" [] {
  let st = (load-state)
  save-state ($st | upsert stopped true)
  $st | upsert stopped true | to json
}

def "main run status" [] {
  let st = (load-state)
  $st | merge (remaining $st) | to json
}

def remaining [st: record] {
  if ($st | get -o deadline | default "" | is-empty) {
    return {passed: true, remaining: "0", reason: "no run in progress"}
  }
  let dl = ($st.deadline | into datetime)
  let now = (date now)
  let passed = (($st | get -o stopped | default false) or ($now >= $dl))
  {
    passed: $passed
    remaining: (if $now < $dl { ($dl - $now) | into string } else { "0" })
    reason: (if ($st | get -o stopped | default false) { "stopped" } else if $now >= $dl { "deadline passed" } else { "running" })
  }
}

# {passed, deadline, remaining, reason}; passed is true once the deadline has
# gone by or the run was stopped, so a loop can key its exit on one field.
def "main deadline" [] {
  let st = (load-state)
  remaining $st | merge {deadline: ($st | get -o deadline | default "")} | to json
}

# Increment and print the attempt counter for a task.
def "main attempt" [id: any] {
  let id = ($id | into string)
  let st = (load-state)
  let n = (($st.attempts | get -o $id | default 0) + 1)
  save-state ($st | update attempts ($st.attempts | upsert $id $n))
  $n | to json
}

def attempts-for [id: string] {
  load-state | get attempts | get -o $id | default 0
}

# ---- report ----------------------------------------------------------------

def report-path [] { $STATE_DIR | path join report.md }

def "main report append" [text: string] {
  mkdir $STATE_DIR
  if not (report-path | path exists) {
    $"# Overnight report\n\nStarted ((date now) | format date '%Y-%m-%d %H:%M')\n\n" | save (report-path)
  }
  $"- ((date now) | format date '%H:%M') ($text)\n" | save --append (report-path)
}

def "main report show" [] {
  if (report-path | path exists) { open --raw (report-path) } else { "no report yet" }
}

# ---- GitHub transport ------------------------------------------------------

def gh-headers [] {
  let tok = (
    $env | get -o GITHUB_MCP_TOKEN | default ($env | get -o GITHUB_TOKEN | default ($env | get -o GH_TOKEN | default ""))
  )
  if ($tok | is-empty) {
    error make {msg: "no GitHub token in GITHUB_MCP_TOKEN, GITHUB_TOKEN or GH_TOKEN"}
  }
  [
    Authorization $"Bearer ($tok)"
    Accept "application/vnd.github+json"
    X-GitHub-Api-Version "2022-11-28"
    User-Agent "overnight-skill"
  ]
}

def gh-get [path: string] {
  http get --headers (gh-headers) $"($API)($path)"
}

# GET that tolerates a 404; returns null for one.
def gh-get-opt [path: string] {
  let r = (http get --full --allow-errors --headers (gh-headers) $"($API)($path)")
  if $r.status == 404 { null } else if $r.status >= 400 {
    error make {msg: $"GitHub GET ($path) failed: ($r.status) ($r.body | to json -r)"}
  } else { $r.body }
}

def gh-get-all [path: string] {
  let sep = (if ($path | str contains "?") { "&" } else { "?" })
  mut page = 1
  mut acc = []
  loop {
    let chunk = (gh-get $"($path)($sep)per_page=100&page=($page)")
    $acc = ($acc | append $chunk)
    if ($chunk | length) < 100 { break }
    $page += 1
  }
  $acc
}

# A fine-grained token answers a missing permission with 403 and names the
# permission it wanted in a response header; surface both so the fix is obvious.
def gh-fail [verb: string, path: string, r: record] {
  let needed = ($r.headers.response | where name == "x-accepted-github-permissions" | get -o 0.value | default "")
  let msg = ($r.body | get -o message | default ($r.body | to json -r))
  let hint = (if ($needed | is-empty) { "" } else { $" \(token needs: ($needed)\)" })
  error make {msg: $"GitHub ($verb) ($path) failed: ($r.status) ($msg)($hint)"}
}

def gh-post [path: string, body: record] {
  let r = (http post --full --allow-errors --headers (gh-headers) --content-type application/json $"($API)($path)" $body)
  if $r.status >= 400 { gh-fail "POST" $path $r }
  $r.body
}

def gh-delete [path: string] {
  let r = (http delete --full --allow-errors --headers (gh-headers) $"($API)($path)")
  if $r.status >= 400 and $r.status != 404 { gh-fail "DELETE" $path $r }
}

# ---- GitHub backend --------------------------------------------------------

def label-for [cfg, state: string] { $"($cfg.label_prefix):($state)" }

def issue-path [cfg, id: string] { $"/repos/($cfg.repo)/issues/($id)" }

def state-of [cfg, issue] {
  let names = ($issue.labels | get name)
  $STATES | where {|s| (label-for $cfg $s) in $names } | get -o 0 | default ""
}

def field-line [lines: list<string>, key: string] {
  $lines
  | where {|l| $l | str starts-with $"($key):" }
  | get -o 0
  | default ""
  | str replace $"($key):" ""
  | str trim
}

export def parse-triage [body: string] {
  let lines = ($body | lines)
  let blocked = (field-line $lines "Blocked-by" | parse -r '#(\d+)' | get -o capture0 | default [])
  let checks = (csv-list (field-line $lines "Checks"))
  let idx = ($lines | enumerate | where {|e| $e.item | str starts-with "Acceptance criteria:" } | get -o 0.index)
  let criteria = (if $idx == null { "" } else { $lines | skip ($idx + 1) | str join "\n" | str trim })
  {blocked_by: $blocked, checks: $checks, criteria: $criteria}
}

export def render-triage [criteria: string, blocked_by: list<string>, checks: list<string>] {
  let blocked = ($blocked_by | each {|b| $"#($b)" } | str join ", ")
  [
    $TRIAGE_MARK
    "**Overnight triage**"
    ""
    $"Blocked-by: ($blocked)"
    $"Checks: ($checks | str join ', ')"
    ""
    "Acceptance criteria:"
    $criteria
  ] | str join "\n"
}

# The newest triage comment wins, so re-triaging an issue is just another comment.
def gh-triage [cfg, id: string] {
  let hits = (
    gh-get-all $"(issue-path $cfg $id)/comments"
    | where {|c| $c.body | str starts-with $TRIAGE_MARK }
  )
  if ($hits | is-empty) { null } else { $hits | last }
}

def gh-task [cfg, issue] {
  let id = ($issue.number | into string)
  let triage = (gh-triage $cfg $id)
  let meta = (if $triage == null { {blocked_by: [], checks: [], criteria: ""} } else { parse-triage $triage.body })
  {
    id: $id
    title: $issue.title
    body: ($issue | get -o body | default "")
    url: $issue.html_url
    state: (state-of $cfg $issue)
    notes: ""
    pr: ""
  } | merge $meta
}

def gh-blocker-done [cfg, id: string] {
  let issue = (gh-get-opt (issue-path $cfg $id))
  $issue != null and $issue.state == "closed"
}

def gh-set-state [cfg, id: string, state: string] {
  let issue = (gh-get (issue-path $cfg $id))
  let current = ($issue.labels | get name | where {|n| $n | str starts-with $"($cfg.label_prefix):" })
  for name in $current {
    gh-delete $"(issue-path $cfg $id)/labels/($name | url encode)"
  }
  if $state != "done" {
    gh-post $"(issue-path $cfg $id)/labels" {labels: [(label-for $cfg $state)]} | ignore
  }
}

def gh-comment [cfg, id: string, body: string] {
  gh-post $"(issue-path $cfg $id)/comments" {body: $body} | ignore
}

def "main labels ensure" [] {
  let cfg = (load-config)
  if $cfg.backend != "github" { return }
  let palette = {queued: "1d76db", active: "fbca04", review: "0e8a16", blocked: "d93f0b"}
  for state in [queued active review blocked] {
    let name = (label-for $cfg $state)
    if (gh-get-opt $"/repos/($cfg.repo)/labels/($name | url encode)") == null {
      gh-post $"/repos/($cfg.repo)/labels" {
        name: $name
        color: ($palette | get $state)
        description: $"Overnight automation: ($state)"
      } | ignore
      print -e $"created label ($name)"
    }
  }
}

# ---- file backend ----------------------------------------------------------

def file-load [cfg] {
  if ($cfg.plan_file | path exists) { open $cfg.plan_file | get -o task | default [] } else { [] }
}

def file-save [cfg, tasks: list] {
  {task: $tasks} | to toml | save -f $cfg.plan_file
}

def file-find [cfg, id: string] {
  let hit = (file-load $cfg | where id == $id)
  if ($hit | is-empty) { error make {msg: $"no task ($id) in ($cfg.plan_file)"} }
  $hit | first
}

def file-update [cfg, id: string, f: closure] {
  file-find $cfg $id | ignore
  file-save $cfg (file-load $cfg | each {|t| if $t.id == $id { do $f $t } else { $t } })
}

# ---- shared surface --------------------------------------------------------

def with-runtime [cfg, task] {
  $task | merge {
    attempts: (attempts-for $task.id)
    branch: $"($cfg.branch_prefix)($task.id)-(slug $task.title)"
  }
}

# Print the resolved configuration.
def "main config" [] { load-config | to json }

# Open issues that carry no overnight label and none of `skip_labels`.
# Brief on purpose; `show <id>` fetches one issue's body and discussion.
def "main candidates" [--limit: int = 200] {
  let cfg = (load-config)
  if $cfg.backend != "github" {
    error make {msg: "candidates only applies to the github backend; edit the plan file directly"}
  }
  gh-get-all $"/repos/($cfg.repo)/issues?state=open"
  | where {|i| ($i | get -o pull_request) == null }
  | where {|i|
      let names = ($i.labels | get name)
      let ours = ($names | any {|n| $n | str starts-with $"($cfg.label_prefix):" })
      let skipped = ($names | any {|n| $n in $cfg.skip_labels })
      (not $ours) and (not $skipped)
    }
  | each {|i| {
      id: ($i.number | into string)
      title: $i.title
      labels: ($i.labels | get name)
      milestone: ($i | get -o milestone.title | default "")
      url: $i.html_url
    } }
  | sort-by -n id
  | first $limit
  | to json
}

# One task in full: body, existing triage, and the human discussion.
def "main show" [id: any] {
  let cfg = (load-config)
  let id = ($id | into string)
  if $cfg.backend == "file" {
    return (with-runtime $cfg (file-find $cfg $id) | to json)
  }
  let issue = (gh-get (issue-path $cfg $id))
  let comments = (gh-get-all $"(issue-path $cfg $id)/comments")
  with-runtime $cfg (gh-task $cfg $issue)
  | merge {
      comments: ($comments | where {|c| not ($c.body | str starts-with $TRIAGE_MARK) } | each {|c| {author: $c.user.login, body: $c.body} })
    }
  | to json
}

# Put a task in the queue with its triage metadata.
def "main queue add" [
  id: any
  --title: string        # required for the file backend; ignored for github
  --criteria: string     # acceptance criteria, the implementer's contract
  --blocked-by: string   # comma-separated task ids that must be done first
  --checks: string       # comma-separated keys into config `checks`
] {
  let cfg = (load-config)
  let id = ($id | into string)
  let blocked = (csv-list $blocked_by)
  let checks = (csv-list $checks)
  let criteria = ($criteria | default "")
  if $cfg.backend == "github" {
    gh-comment $cfg $id (render-triage $criteria $blocked $checks)
    gh-set-state $cfg $id "queued"
  } else {
    if ($title | default "" | is-empty) { error make {msg: "--title is required for the file backend"} }
    let tasks = (file-load $cfg)
    if ($tasks | any {|t| $t.id == $id }) { error make {msg: $"task ($id) already in ($cfg.plan_file)"} }
    file-save $cfg ($tasks | append {
      id: $id, title: $title, state: "queued", criteria: $criteria
      blocked_by: $blocked, checks: $checks, notes: "", pr: "", url: ""
    })
  }
}

# Take a task back out of the queue without touching anything else.
def "main queue remove" [id: any] {
  let cfg = (load-config)
  let id = ($id | into string)
  if $cfg.backend == "github" {
    gh-set-state $cfg $id "done"
  } else {
    file-find $cfg $id | ignore
    file-save $cfg (file-load $cfg | where id != $id)
  }
}

# Every task the queue knows about, in every state; the table for the plan review.
def "main queue list" [
  --table  # render as a table instead of JSON
] {
  let cfg = (load-config)
  let tasks = (if $cfg.backend == "github" {
    $STATES
    | where $it != done
    | each {|s|
        gh-get-all $"/repos/($cfg.repo)/issues?state=open&labels=(label-for $cfg $s | url encode)"
        | where {|i| ($i | get -o pull_request) == null }
        | each {|i| gh-task $cfg $i }
      }
    | flatten
  } else {
    file-load $cfg
  })
  let rows = ($tasks | sort-by -n id | each {|t| with-runtime $cfg $t })
  if $table {
    $rows | select id state title blocked_by checks attempts pr
  } else {
    $rows | to json
  }
}

# Queued tasks whose blockers are all done, oldest first, at most `limit`.
# The default limit is the batch size recorded by `run start`, else config `batch`.
def "main next" [--limit: int] {
  let cfg = (load-config)
  let limit = ($limit | default (load-state | get -o batch | default $cfg.batch))
  let ready = (if $cfg.backend == "github" {
    gh-get-all $"/repos/($cfg.repo)/issues?state=open&labels=(label-for $cfg 'queued' | url encode)"
    | where {|i| ($i | get -o pull_request) == null }
    | each {|i| gh-task $cfg $i }
    | where {|t| $t.blocked_by | all {|b| gh-blocker-done $cfg $b } }
  } else {
    let tasks = (file-load $cfg)
    let done = ($tasks | where state == done | get id)
    $tasks
    | where state == queued
    | where {|t| $t.blocked_by | all {|b|
        if $b in $done { true } else {
          if not ($b in ($tasks | get id)) { print -e $"warning: ($t.id) is blocked by unknown task ($b)" }
          false
        }
      } }
  })
  $ready | sort-by -n id | first $limit | each {|t| with-runtime $cfg $t } | to json
}

# Move a task to a new state, optionally leaving a note or PR link behind.
def "main mark" [id: any, state: string, --note: string, --pr: string] {
  let cfg = (load-config)
  let id = ($id | into string)
  check-state $state
  let note = ($note | default "")
  let pr = ($pr | default "")
  if $cfg.backend == "github" {
    gh-set-state $cfg $id $state
    if ($note | is-not-empty) or ($pr | is-not-empty) {
      let extra = ([$note (if ($pr | is-empty) { "" } else { $"PR: ($pr)" })] | where ($it | is-not-empty) | str join "\n\n")
      gh-comment $cfg $id $"**Overnight: ($state)**\n\n($extra)"
    }
  } else {
    file-update $cfg $id {|t|
      $t
      | update state $state
      | update notes (if ($note | is-empty) { $t.notes } else { [$t.notes $note] | where ($it | is-not-empty) | str join "\n" })
      | update pr (if ($pr | is-empty) { $t.pr } else { $pr })
    }
  }
}

# Open the PR for a finished branch and move the task to review.
def "main pr open" [id: any, --branch: string, --title: string, --body: string] {
  let cfg = (load-config)
  let id = ($id | into string)
  if ($cfg | get -o repo | default "" | is-empty) {
    error make {msg: "pr open needs repo = \"owner/name\" in .claude/overnight.toml"}
  }
  for f in [[branch $branch] [title $title]] {
    if ($f.1 | default "" | is-empty) { error make {msg: $"--($f.0) is required"} }
  }
  let pr = (gh-post $"/repos/($cfg.repo)/pulls" {
    title: $title
    head: $branch
    base: $cfg.base_branch
    body: ($body | default "")
  })
  main mark $id review --pr $pr.html_url
  {id: $id, pr: $pr.html_url, number: $pr.number} | to json
}

def main [] {
  print -e "overnight.nu: queue, run-state and report helper for the overnight skill"
  print -e "run `nu overnight.nu --help` for the command list"
}
