---
title: Fix native ownership gate failures
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T13:16:56.180099+03:00"
closed-at: "2026-10-20T18:19:57.616059+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Final a5f93a78 native gate fails native-window-owner and field-proj-boundary with -8286 while compiling USE; compiler-native-host-construction reports missing implementation facts then -9440; native-defer-image prints source ok then7122. Reproduce these real paths on final source24e30532, separate shared cause from distinct boundaries, preserve retained/fresh code and storage ownership. Fix only demonstrated responsible failures, each coherent change separate; existing E2Es and independent Astra review before integration. Full captured log: carl-incremental-native.brg87d/native.stdout.log.
