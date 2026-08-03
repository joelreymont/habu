---
title: Time a real workload end to end
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-03T19:19:49.857946+02:00\""
---

The system-level proof the microbenchmarks cannot give: one process, a real workload timed with the old code, then the surveyed hot words migrated to the new chain, then the same workload re-timed - the delta is the honest 'the system is N% faster' number. Design constraints established earlier: the engine INLINES <=40-byte bodies at existing call sites, so republication does not speed up already-compiled callers - the workload must therefore COMPILE ITS CALLERS AFTER migration (e.g. evaluate a large generated file of checked definitions exercising the hot words, or re-run a compile-heavy phase) or target words too big to have been inlined. Build tools/codegen-workload.f: pick 2-3 real workloads (candidates: checking a large generated source file - the checker's own hot words TAG/PAY/SYM-FOLD-C are in the survey; a maki compute phase over the integer/float kernels), time before/after migration in one process with the harness's timing discipline (repetitions, fastest-of-N, both measured identically), report the deltas unshaded - a null result on inlined-everywhere paths is a finding about inlining, not a failure. No engine changes; the migration machinery exists (NMIGRATE staged callees, NCLOB, NINL).

Claim: agent=worklane workspace=.jj-ws/habu-time-a-real-c2ea7295
