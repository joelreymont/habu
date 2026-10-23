---
title: Trim Habu test audit rows g3a
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T17:12:47.533345+03:00"
closed-at: "2026-10-23T17:33:29.494250+03:00"
close-reason: "Rejected all 17 rows by measurement: a process start costs 0.01-0.04 s; every merge was neutral or slower (merging after '1 set-tier' compiles library requires at tier 1: catch-group 2.8-3.3 s vs 1.2 s separate); native-resource-image saves ~0.5 s CPU; plusloop DEEP cut saves <=0.34 s; rstack-input forks would change the path under test (fresh engine reading stdin); replay-binding growth sessions take 0.03/0.01 s."
---

Problem: the Habu test audit (group g3) found gate suites whose cases repeat setup, duplicate other rows' coverage or fork more processes than their boundaries need.
Acceptance: each MERGE/SLOW/CUT row below is applied after verifying its coverage claim, or rejected with a reason recorded at closure (e.g. measured saving too small, or batching would change the path under test); every distinct assertion and boundary a row names is kept; registry entries (test/gate-*.f) match the moved/removed suites.
Verification: each changed suite passes on its own with wall time before and after; the full registry (bin/hb --load test/run.f) passes after the batch lands.

Rows:
| native-quote-forward | **MERGE** | Into **compiler-native-catch**, alongside the rstack cases below. Preserve nested callback forwarding, quotation arguments through catch, and wrong-inner-effect rejection. These are distinct from simply parking a quotation; they need no separate engine. [Assertions:18](test/native-quote-forward-e2e.f:18), [target setup:2](test/compiler/native-catch.f:2). |

| compiler-native-hookless-reject | **SLOW** | Remove **tier-1 ZERO-AMBIGUOUS-TICK**: it and ZERO-AMBIGUOUS-CALL terminate at the identical preceding `W-FORGET`, then use identical assertions. Keep their tier-0 variants. The remaining 23-subject structure uses loaded-engine forks; its two expensive build probes test different boundaries, so their elimination is unproven. [Duplicate prefix/assertions:268](test/compiler/native-hookless-reject.f:268), [build probes:399](test/compiler/native-hookless-reject.f:399). |

| native-resource-image | **MERGE** | Into **process-image**’s existing saver/restore chain. Warm ZIP/F64 before capture; retain stale archive/file/input rejection and resource reacquisition after restore. Run process-image’s first-descriptor check before accessing resources. Removes the standalone save and two child executions. [Current build/relaunch:50](test/native-resource-image.f:50), [resource assertions:49](test/native-resource-image-subject.f:49), [target chain:81](test/process-image.f:81). |

| compiler-codegen-tail-probe | **MERGE** | Into **compiler-native-fold**, whose inventory tool already loads the tail probe. Preserve refusal of out-of-extent reads and detection that a tail routine must not borrow the next word’s return instruction. [Assertions:117](test/compiler/codegen-tail-probe.f:117), [shared dependency:55](tools/codegen-combine-inventory.f:55). |

| compiler-native-edge-permutation | **MERGE** | Into **compiler-native-loop**. Retain swap/rotation/sentinel assertions and freshly compiled SHA source with known digests; baked SHA would not exercise the compiler edge-copy path. No separate process boundary is asserted. [Cases:27](test/compiler/native-edge-permutation.f:27), [fresh SHA:47](test/compiler/native-edge-permutation.f:47). |

| compiler-native-do | **MERGE** | Into **compiler-native-loop**. Preserve `do` versus `?do` entry behavior, signed limits, visited indices and counters surviving calls. Both use the same compiler/inventory setup. [Setup:2](test/compiler/native-do.f:2), [entry cases:101](test/compiler/native-do.f:101). |

| compiler-native-plusloop | **SLOW** | `DEEP` runs **2²⁴ increments** to exercise 24 simultaneously live loop frames and a large stack reservation. Retain that compilation/frame pressure, but reduce outer trip counts while retaining an inner backedge. Verify the frame remains large; otherwise the cheaper case would lose its purpose. [Repeated work:96](test/compiler/native-plusloop.f:96), [existing nested-step coverage:223](test/compiler/native-plusloop.f:223). |

| compiler-native-j | **MERGE** | Into **compiler-native-loop**. Retain unequal rectangular bounds, middle-index selection in three levels, outer index across calls, and refusal to borrow caller loop frames. Same loaded compiler; preload its evaluation helper. [Behavior:99](test/compiler/native-j.f:99), [quotation refusals:155](test/compiler/native-j.f:155). |

| compiler-native-again | **MERGE** | Into **compiler-native-loop**. Retain values and calls crossing unconditional backedges, local-name binding, and the valid-repeat/invalid-again distinction. Infinite fixtures are compiled, not executed. [Cases:89](test/compiler/native-again.f:89), [compile-only fixtures:129](test/compiler/native-again.f:129). |

| compiler-native-rstack | **MERGE** | Into **compiler-native-catch**. Preserve multi-value hidden-stack transfers, real values across calls, quotation identity, and freshly compiled combinators. Catch’s single parked scalar does not cover these; its loaded compiler can run them. [Setup/replay:2](test/compiler/native-rstack.f:2), [distinct transfers:271](test/compiler/native-rstack.f:271). |

| compiler-native-rstack-input | **SLOW** | Executes a fresh engine for every source/tier. Use the existing loaded-engine subject-fork structure; retain terminating-refusal children and batch compatible admissions. **Coverage defect:** its unconditional tier-0 runs stop before their intended cases on x64. [Exec:71](test/compiler/native-rstack-input.f:71), [tier loops:103](test/compiler/native-rstack-input.f:103), [x64 restriction:3983](src/habu/kernel-x64.f:3983). |

| compiler-native-local-ambiguity | **MERGE** | Into **compiler-native-local-case**. Preserve its local winning over two imported `LAYERS` words with an existing `TARGET`; reuse the duplicated source/capture harness. [Repeated harness:13](test/compiler/native-local-ambiguity.f:13), [unique case:49](test/compiler/native-local-ambiguity.f:49). |

| compiler-native-product-locals-aot | **MERGE** | Into **compiler-native-catch**’s tier-1 process. Preserve product locals across calls/storage/scopes, mixed execute results, nested payloads and the **24-cell local wider than its source token count**. This row does not build a saved application. [Wide local:129](test/compiler/native-product-locals.f:129), [remaining consumers:140](test/compiler/native-product-locals.f:140). |

| compiler-native-word-binding | **MERGE** | Into **reopen-binding**. Keep same-file package-owned intrinsic names, mixed case and intrinsic/package/intrinsic transitions; share its harness and engine, preserving independent package scopes. [Assertions:79](test/compiler/native-word-binding.f:79), [target setup:16](test/reopen-binding.f:16). |

| compiler-native-word-binding-aot | **MERGE** | Into **reopen-binding-aot**, retaining the same assertions under the optimizing compiler. Both registry rows already preload the harness before tier selection. [Registry setup:922](test/gate-stdlib-cases.f:922), [target setup:933](test/gate-stdlib-cases.f:933). |

| reopen-binding-aot | **SLOW** | Retain the second compiler execution on ARM. On x64, default compilation is already tier 1, so this repeats the whole file for a setter that changes nothing. Timed architecture was not supplied. [Only tier-selection change:933](test/gate-stdlib-cases.f:933), [x64 restriction:3983](src/habu/kernel-x64.f:3983). |

| replay-binding | **SLOW** | Keep replay-overlay restoration and fatal live-mutation cases. Combine the two successful growth sources into one engine session: their distinct names require no restart between them. That removes **one of 17 engine executions**, not the entire replay matrix. [Repeated successful sessions:654](test/replay-binding.f:654), [growth sources:321](test/replay-binding.f:321), [rollback assertions:168](test/replay-binding.f:168). |
