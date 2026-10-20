---
title: Attach defer contracts before publication
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T13:34:25.193500+03:00"
closed-at: "2026-10-20T18:19:57.865826+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

The reduced DEFAULT/PUBLISH source on the matching private host reports binding-contract-unknown for PUBLISH-XT (ordinal 23556), followed by ordered-dependency store refusal. C-DEFER appends and invokes RECORD-PUBLISHED before registering its effect/deferred status, so the exact occurrence cannot acquire its checker contract. Register the unpublished record with the retained source owner and ready target before the existing single append, close/flush emitted code before checker calls, and enqueue target pretrust work only after successful publication. Preserve rollback, pretrust overflow/backstop and exact binding semantics. Extend existing defer replay behavior before code; verify contract presence, warm reuse/runtime result, replacement/effect rejection, matching native/recovery and convergence, with independent Astra review.
