---
title: Integrate FILE task ownership
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T14:47:33.477062+03:00"
closed-at: "2026-10-20T18:19:57.697515+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Reviewed peer masterbea3fdac now carries complete TASK declaration ownership for explicit FILE replay (chain and exit registrations), overlapping our producer-bound capture and dedicated TASK user-slot watermark fix. Resolve the real tools/native-unit-capture.f merge preserving both full declaration state and late/prefix-loaded TASK watermark, using existing owner API once and excluding fresh process scratch. Preserve root exact bound producer callbacks and automatic initializer semantics; verify peer legacy task/USER cases plus root automatic task-state E2E and old-cache invalidation behavior. Independent review of integration delta required before final combined gate. No API/format change unless evidence proves necessary.

On source76feed02 with matching native engine4384e857/map6ba4a675, the unchanged native-file-task-state-e2e passes both CHAIN and EXIT-HOOKS, including callbacks declared inside FILE. The unchanged native-file-types-e2e also passes cold/warm reuse, fresh downstream use, shifted-prefix imports and nominal refusal. Both exit0/test:ok with fresh external scratch/cache. The exact-prefix family correction512d68d4 retains v10 IDs while portable/pending imports still remap; no fixture changes were needed. Full combined native and generation gates remain required.
