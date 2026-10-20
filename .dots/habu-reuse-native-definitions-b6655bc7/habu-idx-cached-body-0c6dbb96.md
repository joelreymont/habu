---
title: Index cached body contexts
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T15:03:33.064419+03:00"
closed-at: "2026-10-20T18:19:57.940536+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

The frame-owned index now chains body candidates by stored context, preserving reverse row order through open, growth, store and span refresh. Full body matching, complete-reader checks, dependency preparation, pruning, disk format and source actions remain intact. A 2,048-definition edited app reduced candidate calls from 2,104,324 to 2,332 with 2,049 hits/skips, one compile and output 2069 in both versions; uninstrumented build CPU was 14.86s versus 14.53s on the busy host. Before/after history and actions passed. Both body selectors were stopped by the lead; baseline assertion F87 failed the EOF cached/fresh image comparison on donor 573305bb before stopping. Independent Astra review cleared the implementation. Final matching-engine native coverage and landing remain with the integrator.
