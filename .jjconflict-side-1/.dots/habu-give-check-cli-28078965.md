---
title: Give check-cli-boundary room under its phase guard
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T04:58:36.460347+02:00"
---

Static invariant: the gate decides whether the check-cli-boundary phase passed by asking whether the phase process finished inside 120000 ms (SUITE-TIMEOUT-MS, test/gate-stdlib-lib.f:12, applied to every pooled phase through SUITE-RUN-ENV-ASYNC). Nothing ties that number to how much work the phase has, so a phase that is merely slow and a phase that is wedged are the same verdict, and the phase with the least room is the one whose own children were just re-budgeted.

Full context: measured 2026-07-30 on a 12-core machine while closing dot habu-stabilize-two-pool-763a7ec9. tools/check-test.f runs about 36 to 40 s with the machine idle. Under eight-way contention of the kind the gate pool creates - seven other gate phases busy, load average 18 to 24 - twenty consecutive runs took 100, 102, 102, 103, 104, 105, 107, 107, 107, 109, 109, 110, 110, 110, 111, 112, 112, 113, 113 and 114 s. Every one passed, but the worst was 6 s short of the phase guard and the median was 109 s. A further five runs at a slightly higher load average, 20 to 24, took 106, 108, 115, 119 and 172 s: the 172 s run is 43 percent past the phase guard and the 119 s run sat exactly on the line. Those runs were measured with no phase guard above them, so they completed and passed; inside the real gate pool the 172 s run would have been killed at 120 s and reported as a killed phase. The ceiling is therefore not merely close, it is reachable on a host this busy, and the next case added to the phase lowers the load needed to reach it. When that happens the gate reports a killed phase and none of the per-case diagnostics survive, including the named deadlock verdict that dot habu-stabilize-two-pool-763a7ec9 just added inside the fixture.

Why it is not fixed by that dot: the child budgets inside the fixture are now measured and named (WORST-CHILD-MS times HANG-MARGIN, 54 s, with a CASE-HUNG verdict that names the case and the guard). The 120 s ceiling belongs to the gate, is shared by every pooled phase, and cannot be raised for one phase without a decision about the others, so it was left alone rather than changed from inside a test fixture.

Required result: decide, with measurement, how the gate bounds a pooled phase. Either derive the per-phase budget from that phase's measured cost with stated headroom instead of one shared 120 s number, or split check-cli-boundary so no single phase carries 40 s of idle work, or both. A phase killed on the guard must report which phase and which budget, distinct from a phase that failed a case.

Also audit, on the same terms, the other unmeasured 10 s child budgets found while measuring this: GPT-TIMEOUT-MS in test/gate-pool-test.f:33 and ASM-TIMEOUT-MS in lib/ptx/toolchain.f:22. Both are the same shape - a raw wall-clock number standing in for a liveness bound - though both guard much cheaper children than the check-cli fixture did, so neither has been observed to fire.

Acceptance: the chosen budget is written as a measured quantity with its margin, not a bare literal; twenty consecutive eight-way contended runs of the phase stay green with the worst run at least a stated factor inside its budget; a phase forced past its budget prints a line naming the phase and the budget, clearly different from a case failure; suite-coverage-lint, package-diff-lint and typed-local-diff-lint exit 0 on the diff.
