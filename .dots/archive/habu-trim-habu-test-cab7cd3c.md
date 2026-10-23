---
title: Trim Habu test audit rows g5
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T17:12:47.589952+03:00"
closed-at: "2026-10-23T18:00:32.010729+03:00"
close-reason: "Applied: cold-naming runs its three axiom positives in one cold boot (21.7→18.2 s); fs-lock waits on a pipe-held cat instead of sleep 1 (1.18→0.17 s). Rejected, each under ~1 s: c2-view-record-mut, combinators, trust-row-refusal, shadowed-arity-refusal, pty, xt-effect, export-keyword-package, load-argv-contract, fs-anchored, fs-mutate; outer-interpret (≤0.43 s); aot-wid-rebase/aot-data-window: the discarded emission is not measurable."
---

Problem: the Habu test audit (group g5) found gate suites whose cases repeat setup, duplicate other rows' coverage or fork more processes than their boundaries need.
Acceptance: each MERGE/SLOW/CUT row below is applied after verifying its coverage claim, or rejected with a reason recorded at closure (e.g. measured saving too small, or batching would change the path under test); every distinct assertion and boundary a row names is kept; registry entries (test/gate-*.f) match the moved/removed suites.
Verification: each changed suite passes on its own with wall time before and after; the full registry (bin/hb --load test/run.f) passes after the batch lands.

Rows:
| c2-view-record-mut | **MERGE** | Move into **c2-mut-records**’ native process. Its make/unmake positive overlaps that suite’s constructors; preserve the three distinct region/scope/pointer refusals. Both use the same SUBJECT boundary. [test/c2-view-record-mut.f:11](test/c2-view-record-mut.f:11), [test/c2-mut-records.f:35](test/c2-mut-records.f:35) |

| xt-effect | **SLOW** | Keep effect fitting and live using-import execution. Its final tier block launches three scenarios already asserted by **top-row-warn**: empty-stack execute and tier-2 execute/catch refusal, including warning counts/status. Consolidate there, preserving xt-effect’s x64-specific diagnostic assertion. [test/xt-effect-test.f:265](test/xt-effect-test.f:265), [test/xt-effect-test.f:304](test/xt-effect-test.f:304), [test/top-row-warn-test.f:196](test/top-row-warn-test.f:196) |

| outer-interpret | **SLOW** | Each `BOTH` reloads the prelude in two fresh engines. Batch the independent COMMENTS, LINE-COMMENT-AT-END, BLANK-INPUT, and TICK-AND-CHAR fixtures as separate files in one load per route: **8 launches → 2**, retaining file EOF boundaries. Keep fatal/state-corrupting cases isolated. [test/outer-interpret.f:206](test/outer-interpret.f:206), [test/outer-interpret.f:246](test/outer-interpret.f:246), [test/outer-interpret.f:486](test/outer-interpret.f:486) |

| aot-wid-rebase | **SLOW** | Keep owner-collision/rebase and forged-window boot refusals. Its two forged rewrites each emit/sign an ordinary image, then overwrite it with another emission. Apply the patch before the single final write: **two discarded emissions/signatures removed**. [test/aot-wid-suite.f:616](test/aot-wid-suite.f:616), [test/native-fixture-write-arm.f:39](test/native-fixture-write-arm.f:39), [test/aot-wid-build.f:504](test/aot-wid-build.f:504) |

| aot-data-window | **SLOW** | Keep initialized DATA, oversized-span boot refusal, and unset-defer trapping. The span rewrite reuses its capture but needlessly emits/signs an ordinary image before the forged one. Remove **one discarded emission/signature** using the same writer change as aot-wid-rebase. [test/aot-data-window-suite.f:61](test/aot-data-window-suite.f:61), [test/aot-wid-build.f:553](test/aot-wid-build.f:553) |

| combinators | **MERGE** | Load after **catch-frame** in its product-engine process. Keep transformed values, callback-row preservation and empty/nested iteration assertions. Both use disjoint packages and balanced in-process cases with independent reports. [test/combinators.f:42](test/combinators.f:42), [test/catch-frame.f:85](test/catch-frame.f:85) |

| export-keyword-package | **SLOW** | Three compatible positive stdin fixtures each exec an engine. Their names are disjoint: run them sequentially in one stdin child, retaining all outputs. **3 launches → 1**; preserve the separate load route and fatal negatives. [test/export-package.f:263](test/export-package.f:263), [test/export-package.f:293](test/export-package.f:293) |

| trust-row-refusal | **MERGE** | Move intact into **using-import**’s process. Preserve wrong-scope/private/qualified registration refusals and the surviving consumer. Its packaged helpers and TRW-prefixed subjects do not require a fresh engine. [test/trust-row-test.f:138](test/trust-row-test.f:138), [test/trust-row-test.f:189](test/trust-row-test.f:189) |

| shadowed-arity-refusal | **MERGE** | Move intact into **using-import**’s process alongside trust-row-refusal. Preserve arity refusals, executable forwarding and absence of phantom published rows; SAT-prefixed fixtures are compatible. [test/shadowed-arity-test.f:50](test/shadowed-arity-test.f:50), [test/shadowed-arity-test.f:142](test/shadowed-arity-test.f:142) |

| load-argv-contract | **SLOW** | Combine seeded-path, repeated-path and unseeded-decoy positives into one argument list ending in the probe: **3 launches → 1**. Preserve reached/decoy output and clean status/stderr. Missing path, dangling symlink and uncaught throw still need separate exits. [tools/load-argv-test.f:117](tools/load-argv-test.f:117), [tools/load-argv-test.f:149](tools/load-argv-test.f:149) |

| cold-naming | **SLOW** | The three independent axiom positives each call LOAD and cold-boot the same prefix. Put their three distinct definitions in one probe: **3 cold boots → 1**. Retain separate fatal refusals and sealed/unsealed configurations. [test/cold-naming-test.f:64](test/cold-naming-test.f:64), [test/cold-naming-test.f:83](test/cold-naming-test.f:83) |

| fs-mutate | **SLOW** | T1 forces the same A-retains-path/B-constructs-path interleaving **20 times**, creating/removing directories each time. Retain one forced handoff; leave unforced concurrency cases and the competing no-replace publishers intact. [src/lib/fs-mutate-test.f:1109](src/lib/fs-mutate-test.f:1109), [src/lib/fs-mutate-test.f:1124](src/lib/fs-mutate-test.f:1124), [src/lib/fs-rename-noreplace-test.f:124](src/lib/fs-rename-noreplace-test.f:124) |

| fs-lock | **SLOW** | Exec inheritance uses `/bin/sleep 1` and waits for its full lifetime after reacquisition checks finish. Use an exec child released by a parent-controlled pipe, preserving the “child still alive” condition and removing the fixed second. [src/lib/fs-lock-test.f:69](src/lib/fs-lock-test.f:69) |

| fs-anchored | **SLOW** | Repeats **30 identical leak-check iterations**, despite checking exact FD inventory after every operation inside each iteration. Keep one complete iteration and final inventory; preserve symlink substitution/open-anchor behavior. [src/lib/fs-anchored-test.f:149](src/lib/fs-anchored-test.f:149), [src/lib/fs-anchored-test.f:107](src/lib/fs-anchored-test.f:107) |

| pty | **MERGE** | Load before **serial** in its process. Both use disjoint packages and close ports/stop AIO; neither requires a fresh process baseline. Preserve the signal-storm quiet-read and stopped-loop assertions. [src/lib/pty-test.f:39](src/lib/pty-test.f:39), [src/lib/pty-test.f:75](src/lib/pty-test.f:75), [src/lib/serial-test.f:250](src/lib/serial-test.f:250) |
