---
title: Release completed native build scratch
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:44:07.992776+03:00"
closed-at: "2026-10-20T18:19:58.312954+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Two cold builds of the identical literal app with distinct empty caches differ in19 saved DATA cells: build paths and retained process callbacks/pointers. Existing NDEF/NUNIT owner close/reset must release transient scratch and report counters after reporting, while the reusable NSTR literal-attach callback must remain typed/relocatable rather than erased. Preserve repeated builds in one process and persistent compiler identities. Cold/cold full executable equality and existing warm/fresh literal/twin E2Es are required; no generic snapshot clearing registry.
