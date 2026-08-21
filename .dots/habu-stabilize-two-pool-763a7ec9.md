---
title: Stabilize two pool-flaky gate fixtures
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-30T00:07:39.572131+02:00\""
---

Full context: measured repeatedly 2026-07-30 by agents storerows, hookpath, and matchdepth under parallel-lane load 18-29. Two gate-stdlib phases flake under pool load and pass alone every time: check-cli-boundary (child fixture exceeds its 10 s budget, throw -2502 E-PROC-TIMEOUT; timed 10.0-11.2 s against the 10 s budget even on an unmodified tree) and compiler-ir-id (three concurrent-allocator/task-reuse timing cases). Same defect class as habu-budget-the-standalone-92d730f2: a wall-clock budget standing in for a logical property, so slow-under-load and broken are indistinguishable. For check-cli-boundary decide what the fixture proves and give it either a load-independent probe or a measured budget with headroom plus a named timeout-vs-dead verdict; for compiler-ir-id make the three timing cases deterministic (synchronize on the actual state transition, not a sleep). A phase that reds only when the host is saturated blocks every merge gate run on a busy orchestrator machine, which is a real cost paid on every integration.

Claim: agent=poolflake workspace=.jj-ws/habu-stabilize-two-pool-763a7ec9 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

## MEASURED 2026-07-30, agent poolflake

Machine: 12-core Apple M2 Max, macOS arm64, workspace .jj-ws/habu-stabilize-two-pool-763a7ec9,
parent 5dc544b8 "Claim pool-flake stabilization dot".

### What was actually wrong

Neither phase had a concurrency defect. Both were reporting a wall-clock budget
as if it were a logical failure.

Hypothesis: the three ir-id concurrency cases fail because the parent's 2000 ms
capture budget is smaller than the child needs on a busy host, not because the
allocator barrier breaks under load. Prediction: raising only that budget, with
the load unchanged, turns every red into a pass; and the child's exit status is
the same whether the host is idle or busy. Falsification test: eight concurrent
`bin/hb --load test/compiler/ir-id.f` runs on the tree as found reported
`TFAIL assert 46/49/52` in all eight (the exact asserts this dot names); the same
eight runs against a scratch copy whose only difference was `SUBJECT-MS` 2000 to
120000 passed in all eight. Independently, the child was run on its own thirty
times across idle and loaded conditions and always exited 0 for mode 1, 76 for
mode 0 and 0 for the cleanup-reuse mode, with zero bytes on stdout and stderr in
every case. Conclusion: the child is deterministic and the stopwatch was the only
thing failing, so this is not the checker-miss-class allocator defect the dot
warned about.

Child costs, measured directly:

| child | idle | eight gate slots busy (load 16-22) |
| --- | --- | --- |
| ir-id concurrency child (fresh engine, lib/task.f + identity module) | 0.62-1.10 s | 2.34-3.00 s |
| check-cli `--load tools/check.f` child | 2.0-3.2 s (load 13) | 7.5-8.7 s |
| check-cli cleanup child | 4.7-5.0 s (load 13) | 11.2-13.4 s |

The check-cli red was reproduced the same way: one `bin/hb --load
tools/check-test.f` with eight busy slots died after 31.9 s at load average 21.9
with `hb: uncaught throw code -2502` and exit 67, on the case after
`check/file-label`, which is `check/usage-direct`. That message names no case, no
child and no budget - it is the anonymous verdict this dot describes.

### What changed

`test/compiler/ir-id.f`. `SUBJECT-MS 2000` is gone. In its place
`WORST-CHILD-MS 3000`, `HANG-MARGIN 10` and `HANG-MS` as their product, with a
comment that records the measurements above, states that the guard is a deadlock
guard rather than a performance expectation, and states the upper bound: the
product has to stay inside the gate's own 120 s per-phase guard so a real
deadlock is named here instead of arriving as a killed phase. A package-private
`CHILD-EXITED=` replaces `T-OUTCOME-EXITED=` at all nine child assertion sites;
it counts exactly one assert per call, as the shared word does, but a capture
deadline now prints `child never exited: deadlock guard expired` plus the guard
in milliseconds, and a signalled child prints its signal, instead of both
printing `expected 0 got 1`. The literal 76 in the barrier-removal case is now
`OVERLAP-RC`, named after the constant the child uses for the same value.

`tools/check-test-lib.f`. The six raw `$2710` capture budgets become one
`CHILD-HANG-MS`, again a recorded `WORST-CHILD-MS 13500` times a stated
`HANG-MARGIN 4`. The margin is 4 rather than 10 because the product is bounded
from above as well as below: a guard past the gate's 120 s phase guard could
never get to name anything. `CASE-RUN` now runs each case under `catch`; a
deadlock code is turned into `FAIL: <case> - child never exited; deadlock guard
ms: <n>` and exit `HANG-RC` (79, chosen clear of 64, 67, 70 and the 83-upward
engine failure ABI), and every other throw is re-thrown untouched.

The natural home for the named variants is `lib/test/outcome.f`, and it is
unavailable: that file defines its three assertions at global scope with no
package, so `tools/package-diff-lint.f` rejects any edit to them. Measured with a
one-character change to the timeout arm: `E-PACKAGE-OWNERSHIP
lib/test/outcome.f:9:3`. Tracked by new dot `habu-name-the-outcome-a80c2197`.

### Acceptance: 20 contended runs, zero reds

Harness: per iteration, six background loops of a real gate phase
(`test/compiler/ir-id.f`) keep all slots busy while the two phases under test run
as the seventh and eighth, so both are measured under the eight-way concurrency
the gate pool creates. Seven of the eight slots spawn their own child processes
and threads, which is at least as contended as the real pool, where most
neighbours are single-process lints. Run on the exact tree, digests
`ccd74847...` for `test/compiler/ir-id.f` and `41d4e1ff...` for
`tools/check-test-lib.f`, verified identical before and after the campaign.

| iter | ir-id rc | ir-id s | check-cli rc | check-cli s | load before | load after |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | 0 | 10 | 0 | 102 | 10.65 | 17.95 |
| 2 | 0 | 10 | 0 | 113 | 18.27 | 21.85 |
| 3 | 0 | 11 | 0 | 110 | 21.85 | 19.91 |
| 4 | 0 | 10 | 0 | 105 | 20.48 | 18.22 |
| 5 | 0 | 10 | 0 | 113 | 18.22 | 20.54 |
| 6 | 0 | 9 | 0 | 107 | 20.54 | 19.38 |
| 7 | 0 | 10 | 0 | 104 | 19.38 | 18.02 |
| 8 | 0 | 10 | 0 | 112 | 17.86 | 18.52 |
| 9 | 0 | 10 | 0 | 107 | 18.32 | 21.50 |
| 10 | 0 | 11 | 0 | 110 | 21.50 | 20.51 |
| 11 | 0 | 12 | 0 | 114 | 20.51 | 19.78 |
| 12 | 0 | 10 | 0 | 102 | 19.48 | 18.20 |
| 13 | 0 | 10 | 0 | 111 | 18.26 | 22.42 |
| 14 | 0 | 13 | 0 | 112 | 22.39 | 20.75 |
| 15 | 0 | 10 | 0 | 100 | 20.53 | 18.65 |
| 16 | 0 | 9 | 0 | 107 | 18.60 | 20.11 |
| 17 | 0 | 13 | 0 | 109 | 23.86 | 19.44 |
| 18 | 0 | 10 | 0 | 103 | 19.16 | 18.71 |
| 19 | 0 | 9 | 0 | 109 | 18.71 | 20.45 |
| 20 | 0 | 14 | 0 | 110 | 20.45 | 21.13 |

40 phase runs, 0 reds. On the tree as found, the same contention reddened
compiler-ir-id in eight of eight attempts.

After the campaign one comment sentence in `test/compiler/ir-id.f` was corrected
(it had claimed the forked subject children never reload source; the
require-replay and seal cases do). Comment text is not executed, and a five
iteration confirmation batch was run on the corrected tree, digest
`56a1c2c8...`, also with zero reds:

| iter | ir-id rc | ir-id s | check-cli rc | check-cli s | load before | load after |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | 0 | 10 | 0 | 115 | 10.23 | 19.58 |
| 2 | 0 | 12 | 0 | 172 | 19.58 | 24.09 |
| 3 | 0 | 12 | 0 | 119 | 24.09 | 23.93 |
| 4 | 0 | 11 | 0 | 106 | 23.93 | 20.21 |
| 5 | 0 | 10 | 0 | 108 | 20.28 | 19.41 |

Iteration 2 is the important one and it is not good news. At load average 24 the
check-cli-boundary phase took 172 s. It passed here because this harness runs the
phase directly, with no phase guard above it; inside the real gate pool the 120 s
`SUITE-TIMEOUT-MS` would have killed it at 120 s and reported a killed phase. So
the phase guard is not merely close to being reachable on a busy host, it is
reachable, and iteration 3 at 119 s sat exactly on the line. The child budgets
this dot owns are fixed and no longer reachable by load; the phase ceiling above
them is not, and it is the remaining way this phase can still go red under
saturation. That is dot `habu-give-check-cli-28078965`, which now carries these
numbers.

### Mutation battery, rerun on the final tree

| mutation | expected | result |
| --- | --- | --- |
| barrier never taken (`USE-BARRIER?` always false) | green case reds, mutation case reds | rc 1, asserts 46 49 52 |
| barrier always taken (`USE-BARRIER?` always true) | mutation case loses its witness | rc 1, assert 49 only |
| drop the double-`ACTIVATE` state guard in `lib/task.f` | reuse case reds | rc 1, asserts 52 54 |
| remove the compare-and-swap from `IR-ID` `TRY-SERIAL`, 6 runs | duplicate serials observed | rc 1 in 6 of 6; assert 46 or 52 varies |
| `WORST-CHILD-MS` 3000 to 1 (ir-id) | named deadlock verdict, not a wrong status | `child never exited: deadlock guard expired` / `guard ms: 10` per case |
| `WORST-CHILD-MS` 13500 to 1 (check-cli) | named deadlock verdict with the case | exit 79, `FAIL: check/usage-direct - child never exited; deadlock guard ms: 4` |
| inject a non-deadlock runtime throw into a check-cli case | propagates untouched | exit 67, `uncaught throw code -2102`, byte-identical to the same injection with `q execute` |

The barrier-removal matrix in the file therefore still bites in both directions.
The compare-and-swap mutation is caught by the file every time, though which of
asserts 46 and 52 fires varies: the rendezvous makes a collision very likely per
round, not certain. That is a pre-existing property of the fixture and is
unchanged by this work.

### Gates

`bin/hb --load test/compiler/ir-id.f` ok. `bin/hb --load tools/check-test.f` ok.
The in-process path the resident tier uses, `CHECK-CLI-GATE:RUN` from
`test/gate-stdlib-inline-lib.f`, exit 0. `package-diff-lint` and
`typed-local-diff-lint` exit 0 on `jj diff --git`, both falsified on this exact
artifact by injecting an unpackaged global and a bare local, which they reported.
`suite-coverage-lint` 173 suites 0 findings, `error-code-lint` 0 findings,
`host-lint` 0 findings, `dot-dep-lint` 0 findings.

### Left open

New dot `habu-give-check-cli-28078965`: under this contention the
check-cli-boundary phase itself takes 100-114 s against the gate's 120 s
per-phase guard, so the phase has under 10 percent room and the next case added
to it kills the phase on the outer guard, which would also swallow the named
verdict added here. New dot `habu-name-the-outcome-a80c2197`: every other caller
of `T-OUTCOME-EXITED=` still gets the anonymous line until that file is packaged.

Update 2026-07-30 (full-gate run on a quiet host, tree 75c9dd8c): the same
phase-ceiling class also reds the two heaviest build phases even with no other
load - build-fixpoint-fixtures and hb-build-fixtures both finish as
kind=TIMEOUT-UNDER-LOAD code=0 in the full pool run (the pool's own eight
slots are the load; hb-build-test alone was earlier measured at 159 s against
the 120 s phase guard). Whatever this dot decides for check-cli's ceiling must
cover these two as well: either the phase guard learns per-phase measured
budgets with the same named timeout-vs-dead verdicts, or the heavy build
phases get their own pool stage with a ceiling derived from measurement. They
are the last two non-defect reds between the current tree and a fully green
gate-stdlib.

Update 2026-07-30 (standalone measurement on the master-merged tree, fresh
engine): tools/build-fixpoint-test.f alone takes 142 s wall against the 120 s
pool phase guard, so its pool red is genuinely this ceiling class. Its CONTENT
also improved: the old "cannot map fixed code region" failure class is GONE
(the snapshot relocation landing fixed it; previously 12 such failures), and 5
failures remain with a different signature - a child exiting 134 (SIGABRT)
after mapping succeeds, consistent with the undeclared persisted-cell crash
owned by habu-fix-persisted-dangling-a520f7b4 (fix in flight). So this phase
needs BOTH the ceiling treatment here and that dot's fix to go green; neither
alone suffices.
