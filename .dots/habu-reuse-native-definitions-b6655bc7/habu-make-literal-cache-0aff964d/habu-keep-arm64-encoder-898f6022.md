---
title: Keep ARM64 encoder operands local
status: closed
priority: 2
issue-type: task
created-at: "2026-10-17T00:09:47.150907+03:00"
closed-at: "2026-10-20T18:19:58.024073+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Fresh d2 literal E2E F18 retains ARM-RM after assembly. ENC-CSEL, ENC-FCMP and ENC-FCSEL need invocation-local operands, removing unused ARM-R3! and ARM-RM while preserving XR3/DR3/condition rejection and exact encoding. Use existing ARM/native consumers and literal image equality.
