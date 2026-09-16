---
title: Pin the compile floor in the gate
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T17:13:40.277529+03:00"
---

Problem: nothing stops the per-word compile cost from regressing: tools/tier-bench.f measures it (trivial word 28 us at tier 0, 723 us at tier 1; corpus 0.105 and 2.51 ms per word, 2026-09-16) but no suite pins it, so a lane can add a copy, an allocation or a scan to the hot path and the gate stays green. Joel: copies, allocations and scans do not belong on the compiler hot path, at either tier. Acceptance: a gate suite runs the compile-floor and corpus cases at both tiers on the engine under test and fails when a number exceeds a named budget constant by more than a stated tolerance (measure the noise on this machine across ten runs at load under 3 and set the tolerance from it, with pinned CPU affinity if the harness allows), budgets carried as named constants with the measured value and date beside them and lowered as the floor dots land; the suite prints the numbers on every run so the gate log is the history. Files: tools/tier-bench.f, test/compile-floor-gate.f (new), test/gate-stdlib-cases.f. Verify: test/run.f; a deliberate regression (an added copy in a hot word) turns the suite red. Depends: none. Ownership: gate. Claim: unassigned.
