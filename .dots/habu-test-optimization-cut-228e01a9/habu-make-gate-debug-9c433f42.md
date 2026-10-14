---
title: "Make gate-debug's prof-reset case load-proof"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-02T11:14:07.445929+02:00"
closed-at: "2026-10-15T01:26:45.188440+03:00"
close-reason: Completed or superseded on current master. 277141d097c9 prevents profiler ticks during reset; recorded stress changed from7 failures in50 runs to0 under48 busy loops. Read-only source/history evidence checked in backlog review; no new code or broader runtime claim.
---

Problem (review 287): test/gate-debug.f failed once under load with 'FAIL: prof-reset left the identity false: a tick landed between its clears' / 'skewed 1' ($HOME/.cache/tmp/kestrel-r4-rev287/ results.txt), rc 0 on an immediate rerun: the case's verdict depends on scheduling. Acceptance: find what prof-reset's identity check assumes about ticks between its clears and make the claim hold under any load (reset atomic with respect to the profiler tick, or the assertion states what reset guarantees), shown by running the case under a CPU-saturating load in a loop (e.g. 50 runs) with 0 failures, after a run that fails first under that load. Files: the profiler reset word and test/gate-debug-lib.f.
