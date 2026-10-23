---
title: Trim Habu test audit rows g2
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T17:12:47.521205+03:00"
closed-at: "2026-10-23T18:00:31.003637+03:00"
close-reason: "Applied engine-size-fixtures (one bin/hb measurement; 10.13→2.79 s) and folded long-string-image plus div-refusal's stripped division image into frame-slot-e2e (the three suites 9.19→3.87 s; long-string row deleted). Rejected, each saving under ~1 s: lint-intern-set 0.11 s; native-string-forms 0.33; wasm-numeric 0.32; wasm-numeric-aot 0.47; wasm-select-f64 0.15; backend-boundary 0.13; loop-frame-order 0.15; native-defer 0.21; finally-aot 0.11; engine-id-capture second boot 0.03; tool-boundary-lints ~0.3; check-repair ~0.6; div-refusal forks; wasm-kernel-words 1.19 total; loop-obligations hoist (no saving); ffi-growth (0.03 s, covers a different plain-engine path)."
---

Problem: the Habu test audit (group g2) found gate suites whose cases repeat setup, duplicate other rows' coverage or fork more processes than their boundaries need.
Acceptance: each MERGE/SLOW/CUT row below is applied after verifying its coverage claim, or rejected with a reason recorded at closure (e.g. measured saving too small, or batching would change the path under test); every distinct assertion and boundary a row names is kept; registry entries (test/gate-*.f) match the moved/removed suites.
Verification: each changed suite passes on its own with wall time before and after; the full registry (bin/hb --load test/run.f) passes after the batch lands.

Rows:
| lint-intern-set | **MERGE** | Append to **error-code-lint-fixtures**. Preserve exact-capacity and one-byte-overflow checks; both already use the intern provider and reset its state. No separate engine process is needed. [set-test.f:35](tools/lint/set-test.f:35), [receiving setup:14](tools/error-code-lint-test.f:14). |

| engine-size-fixtures | **SLOW** | Useful byte-accounting and malformed-layout checks, but repeatedly reads and fully measures unchanged `bin/hb`: eight times on macOS, seven otherwise. Group read-only checks after one measurement; restore edited fields for mutation cases. [repeated measurements:144](tools/engine-size-test.f:144), [later repetitions:262](tools/engine-size-test.f:262), [full read/walk/accounting:3241](tools/image-size-lib.f:3241). |

| tool-boundary-check-repair | **SLOW** | Preserve report-growth and repair-packet behavior. Four fixture generators perform **11,319 open/close cycles** through fragment-sized appends. Generate identical bytes in memory and write each fixture once. [generators:362](tools/check-all-errors-test.f:362), [refusal generator:626](tools/check-all-errors-test.f:626), [each append opens/closes:614](src/lib/fs.f:614). |

| tool-boundary-lints | **SLOW** | The six tools already share one registry process. The checked-boundary large fixture alone performs **2,802 open/close cycles**, including separate newline appends. Preserve its large-input case but construct it with one write. [checked-boundary-lint-test-lib.f:247](tools/checked-boundary-lint-test-lib.f:247), [append implementation:632](src/lib/fs.f:632). |

| engine-id-capture | **SLOW** | Keep the restored-DATA scan for leaked builder path/key. It then boots the same saved image again merely to resolve its pathname. Dump DATA, then resolve the pathname in that same restored child; scanning the already-written dump remains valid. [engine-id-capture-e2e.f:132](test/engine-id-capture-e2e.f:132), [second boot:142](test/engine-id-capture-e2e.f:142). |

| compiler-native-string-forms | **SLOW** | **13 forks** for 11 successful cases and two fatal overlength cases. Batch the successful printed/counted/carried-value cases into one child; retain the two fatal children. [native-string-forms.f:34](test/compiler/native-string-forms.f:34), [case list:60](test/compiler/native-string-forms.f:60), [fatal cases:95](test/compiler/native-string-forms.f:95). |

| compiler-native-long-string-image | **MERGE** | Put its existing 4096/4097/4135-byte checks into **compiler-native-frame-slot-e2e**’s application. Both use the linker-backed production `hb-build` path; string length does not require another image build. [literal checks/build:36](test/compiler/native-long-string-image.f:36), [receiving build:21](test/compiler/native-frame-slot-e2e.f:21). |

| wasm-numeric | **SLOW** | **23 forks** for returning, distinctly named programs; expected throws are caught inside the programs. Batch them into one default-tier child while retaining exact output comparisons. [numeric.f:39](test/wasm/numeric.f:39), [program contracts:8](test/wasm/numeric-rows.f:8). |

| wasm-numeric-aot | **SLOW** | Same 23-to-one batching opportunity, but retain the optimizing-compiler run separately from the default tier. Tier selection precedes subject compilation. [numeric.f:39](test/wasm/numeric.f:39), [aot-mode.f:11](test/compiler/aot-mode.f:11). |

| wasm-select-f64 | **MERGE** | Run inside **wasm-select**: both reopen `WSEL-TEST`, load the same fixture and register the same backend. Register once and run both case lists. Preserve the direct floating-constant payload case; equivalent ordinary-gate coverage was not established. [shared fixture:1](test/wasm/select-lib.f:1), [payload case:80](test/wasm/select-f64.f:80), [duplicate registration:169](test/wasm/select-f64.f:169). |

| wasm-kernel-words | **SLOW** | **62 comparisons × two children = 124 forks.** Batch 61 returning cases into one native and one provider child; keep the terminating zero-divisor pair separate: **124→4 forks**. Preserve independent native/provider output comparison. [kernel.f:47](test/wasm/kernel.f:47), [fatal case:116](test/wasm/kernel.f:116). |

| compiler-backend-boundary | **MERGE** | Move into **compiler-native-hir**’s process. Preserve HIR acceptance of A32/Thumb/C66x/big-endian bindings and ARM64 dialect refusal; the receiving suite already loads the shared HIR/ABI setup. [backend-boundary.f:72](test/compiler/backend-boundary.f:72), [receiving dependencies:11](test/compiler/native-hir.f:11). |

| compiler-native-loop-frame-order | **MERGE** | Add its existing cases to **compiler-native-frame-slot-e2e**. Preserve wide six-cell values and unchanged/permuted UNTIL carriers; the existing scalar image cases do not subsume them. They need no additional build. [native-loop-frame-order.f:33](test/compiler/native-loop-frame-order.f:33), [UNTIL carriers:119](test/compiler/native-loop-frame-order.f:119). |

| compiler-native-div-refusal | **SLOW** | Ten returning programs—eleven on ARM with the capacity case—run separately despite catching their errors. Batch them. Also put the existing stripped division subject into **compiler-native-frame-slot-e2e**’s build; both use the same linker-backed `hb-build` boundary. [per-program runs:238](test/compiler/native-div-refusal.f:238), [separate build:202](test/compiler/native-div-refusal.f:202). |

| compiler-native-defer | **MERGE** | Move into **native-defer-image**’s existing compilation process. Preserve the literal installer and forged-data/non-defer refusals; use its existing restored-image runs for relocation behavior. [native-defer.f:73](test/compiler/native-defer.f:73), [hostile targets:121](test/compiler/native-defer.f:121), [receiving capture flow:54](test/native-defer-image.f:54). |

| compiler-native-finally-aot | **MERGE** | Run its grouped-result cases in **finally**’s process, explicitly selecting tier 1 before their definitions. Preserve product/newtype results and grouped catch restoration; ordinary `finally` cases do not fully cover those shapes. [native-finally.f:18](test/compiler/native-finally.f:18), [grouped recovery:60](test/compiler/native-finally.f:60), [receiving suite:68](test/finally.f:68). |

| loop-obligations | **SLOW** | The entire runtime list repeats 128 times, before and after refusals, at both tiers: **512 passes**. Hoist identical fixed-input checks out of the loop and retain existing branch-distinct inputs and before/after recovery checks. [runtime repetition:133](test/native-window-loop-obligations.f:133), [recovery repetition:193](test/native-window-loop-obligations.f:193), [two tiers:47](test/loop-obligations.f:47). |

| ffi-growth | **CUT** | **stripped-preloaded-runtime** builds this exact file and runs its `MAIN`, asserting success and exact output. The same early/late function and library checks already execute during subject loading and again in the image. Keep the subject; delete the standalone registry row. [covering build/assertions:70](test/stripped-preloaded-runtime.f:70), [identical checks and MAIN:55](test/ffi-growth.f:55). |
