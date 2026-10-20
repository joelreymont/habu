---
title: Keep compiler pass scratch off-image
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:37:44.284257+03:00"
closed-at: "2026-10-20T18:19:58.112250+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Warm/fresh literal E2E persists retired module IDs in NLOOP/A64SEL/A64EMIT BND-OP buffers and NFROZEN S-KEY/S-VIEW/S-READ despite every pass rebinding them. These last-pass buffers belong to compiler work lifetime, like their existing CWORK siblings. Move only these proven transient allocations to existing CWORK-BUFFER; preserve live application/compiler identity state and public behavior. Existing full-image literal/twin E2Es are the regression; focused compiler checks and a matching engine plus required shared-core gates remain necessary.
