---
title: Cut native gate to 30s architecture
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-27T00:05:16.000712+02:00\""
---

Root cause after commit 779b32f5: full gate still passes at 2m09.41s because heavyweight phases still cold-spawn many hb processes and rebuild/verify whole programs at boundary granularity. Target: redesign the gate so the frequent full check is around 30s by keeping coverage but eliminating per-case process builds where not required, batching boundary checks inside one hb when possible, using warm images/shared artifacts for heavy tools, and measuring each change with focused timings plus documented full-gate evidence. Commit and push each significant verified batch.

Checkpoint 2026-06-27: moved build-fixpoint source-shape assertions into the
engine build slice so stdlib tail no longer runs a duplicate full
build-fixpoint fixture. Focused engine build passed in 28.64s, focused stdlib
tail passed in 32.22s, and the documented full native gate passed in 2m07.75s.
Remaining long poles: AOT negative/positive, engine repair, tool-boundary, and
engine build/REPL contention.
