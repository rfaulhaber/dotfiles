#!/usr/bin/env nu
# Exercises overnight.nu against the file backend in a throwaway directory.
# The GitHub backend shares every code path except transport, so this is the
# regression net for queue semantics, run state, and the report.
#
#   nu test-overnight.nu
use std/assert

let script = ($env.CURRENT_FILE | path dirname | path join overnight.nu)
let tmp = (mktemp -d)
cd $tmp
mkdir .claude
"backend = \"file\"\nplan_file = \"overnight-plan.toml\"\nbatch = 2\nmax_attempts = 2\n"
| save .claude/overnight.toml

def ids [rows] { $rows | get id }

print "next on an empty plan returns no tasks"
assert equal (nu $script next | from json) []

print "queue add records tasks and next returns only unblocked ones"
nu $script queue add T1 --title one --criteria "do one"
nu $script queue add T2 --title two --criteria "do two" --blocked-by T1
nu $script queue add T3 --title three --criteria "do three" --checks "rust,web"
assert equal (ids (nu $script next --limit 5 | from json)) [T1 T3]
assert equal (ids (nu $script next --limit 1 | from json)) [T1]

print "next carries criteria and checks through"
let t3 = (nu $script next --limit 5 | from json | where id == T3 | first)
assert equal $t3.criteria "do three"
assert equal $t3.checks [rust web]

print "active and review tasks leave the queue but do not unblock dependents"
nu $script mark T1 active
assert equal (ids (nu $script next --limit 5 | from json)) [T3]
nu $script mark T1 review --pr "https://example.invalid/pr/1"
assert equal (ids (nu $script next --limit 5 | from json)) [T3]

print "done unblocks dependents"
nu $script mark T1 done
assert equal (ids (nu $script next --limit 5 | from json) | sort) [T2 T3]

print "blocked tasks leave the queue and keep the note"
nu $script mark T3 blocked --note "flaky test"
assert equal (ids (nu $script next --limit 5 | from json)) [T2]
assert ((open overnight-plan.toml | get task | where id == T3 | first | get notes) | str contains "flaky test")

print "queued re-queues a task"
nu $script mark T3 queued
assert equal (ids (nu $script next --limit 5 | from json) | sort) [T2 T3]

print "queue list shows every task with its state, blockers and pr"
let listed = (nu $script queue list | from json | select id state blocked_by pr)
assert equal $listed [
  [id state blocked_by pr];
  [T1 done [] "https://example.invalid/pr/1"]
  [T2 queued [T1] ""]
  [T3 queued [] ""]
]
assert ((nu $script queue list --table) | str contains "T2")

print "attempt counts per task and survives across calls"
assert equal (nu $script attempt T2 | from json) 1
assert equal (nu $script attempt T2 | from json) 2
assert equal (nu $script attempt T3 | from json) 1
assert equal (nu $script attempt 42 | from json) 1
assert equal (nu $script attempt 42 | from json) 2

print "marking an unknown task fails"
assert not equal (nu $script mark T9 active | complete | get exit_code) 0

print "deadline reports not passed for a future deadline"
let future = ((date now) + 1hr | format date "%+")
nu $script run start --deadline $future
let d = (nu $script deadline | from json)
assert equal $d.passed false

print "deadline reports passed for a past deadline"
let past = ((date now) - 1hr | format date "%+")
nu $script run start --deadline $past
assert equal (nu $script deadline | from json | get passed) true

print "a bare HH:MM deadline means the next occurrence of that time"
nu $script run start --deadline "23:59"
let d2 = (nu $script deadline | from json)
assert equal $d2.passed false
assert (($d2.deadline | into datetime) > (date now))
assert (($d2.deadline | into datetime) < ((date now) + 1day))

print "run start resets attempts"
assert equal (nu $script attempt T2 | from json) 1

print "run stop makes deadline report passed"
nu $script run stop
assert equal (nu $script deadline | from json | get passed) true

print "report append writes a timestamped line"
nu $script report append "hello report"
assert ((open --raw .claude/overnight/report.md) | str contains "hello report")

print "triage comment round-trips criteria, blockers and checks"
use overnight.nu [parse-triage render-triage]
let rendered = (render-triage "- returns 404 for unknown ids\n- test covers the empty case" [12 34] [rust web])
assert ($rendered | str starts-with "<!-- overnight:triage -->")
let parsed = (parse-triage $rendered)
assert equal $parsed.blocked_by ["12" "34"]
assert equal $parsed.checks [rust web]
assert equal $parsed.criteria "- returns 404 for unknown ids\n- test covers the empty case"

print "a triage comment without blockers or checks parses to empty lists"
let bare = (parse-triage (render-triage "do it" [] []))
assert equal $bare.blocked_by []
assert equal $bare.checks []
assert equal $bare.criteria "do it"

cd /
rm -rf $tmp
print "all tests passed"
