# GC Architecture Comparison

## Scope
- Primary target: map SBCL/OCaml GC techniques into concrete Habu runtime work.
- Current step: SBCL invariant extraction (`habu-sbcl-gc-doc-cc1d45a6`).

## SBCL Trigger Heuristics -> Habu Mapping

| SBCL heuristic | SBCL evidence | Habu current state | Habu action |
|---|---|---|---|
| Dynamic allocation budget is scaled from heap size with a floor. | `/tmp/sbcl/src/runtime/coreparse.c:1055-1059`, `/tmp/sbcl/src/runtime/gc-common.c:82` | Habu trigger is fixed ratio (`gc_threshold`) set at init and not retuned (`src/runtime/heap.zig:233-234`, `src/runtime/heap.zig:516`, `src/runtime/heap.zig:1072-1074`). | Add mutable per-cycle allocation budget derived from live bytes and available headroom; update after every collection in `src/runtime/gc.zig` + `src/runtime/heap.zig`. |
| Post-GC trigger is recomputed to avoid heap exhaustion, with a free-space clamp. | `/tmp/sbcl/src/runtime/gencgc.c:4025-4030` | Habu does not recompute trigger after GC; trigger remains static. | Add `recomputeGcTrigger()` in heap/gc path with clamp semantics: `bytes_used + min(target_budget, free_bytes/2)`. |
| Very large allocation requests are charged directly against trigger budget to force earlier GC. | `/tmp/sbcl/src/runtime/gencgc.c:4183-4196` | Habu allocation path does not have large-allocation trigger charging; `shouldGC()` only checks from-space fullness. | Add allocation-debt charging in alloc fast path: if allocation >= cycle budget slice, force debt/trigger update and early collection decision. |
| Promotion/raise uses policy counters, then escalates under low-space pressure and observed large-object sizes. | `/tmp/sbcl/src/runtime/gencgc.c:3942-3952` | Habu promotion policy is size-centric (`promote_threshold`) with no promotion cycle counter/space-pressure escalation (`src/runtime/gc.zig:344-350`, `src/runtime/heap.zig:303`). | Add tenuring policy inputs: promotion count, large-object pressure, free-space emergency raise gate. |
| Generation re-collection is gated by both space trigger and average age to avoid low-value collections. | `/tmp/sbcl/src/runtime/gencgc.c:3973-3980`, `/tmp/sbcl/src/runtime/gencgc-impl.h:204-214`, `/tmp/sbcl/src/runtime/alloc.c:428-436` | Habu has no age-based major scheduling yet; minor collection is primary path. | Add age/survival telemetry and gate major slice work on `(space_pressure && min_age_met)` instead of size-only triggering. |
| Per-generation trigger is reset after collection as retained-bytes + per-generation budget. | `/tmp/sbcl/src/runtime/gencgc.c:3779-3781`, `/tmp/sbcl/src/runtime/coreparse.c:1131-1134` | Habu does not keep per-generation trigger/budget state. | Introduce per-region/per-generation budget struct and reset logic in GC post-phase bookkeeping. |

## SBCL Allocation/Card Paths -> Habu Mapping

| SBCL alloc/card technique | SBCL evidence | Habu current state | Habu action |
|---|---|---|---|
| Allocation uses open regions with pointer-bump fast path; page metadata is set when region opens, then precise accounting happens when region closes. | `/tmp/sbcl/src/runtime/gencgc.c:243-273`, `/tmp/sbcl/src/runtime/gencgc.c:464-541`, `/tmp/sbcl/src/runtime/gencgc.c:639-727`, `/tmp/sbcl/src/runtime/gencgc-alloc-region.h:11-31` | Habu uses direct region pointers (`alloc_ptr`, `tenured_alloc_ptr`, `los_alloc_ptr`) and immediate accounting; no explicit open/close region model (`src/runtime/heap.zig:825-967`, `src/runtime/heap.zig:1062-1074`). | Add explicit allocation-region descriptors with open/close bookkeeping so GC scan metadata can be updated in bounded points, not on every object edge-case. |
| Small mixed objects are deliberately placed to avoid card-spanning (sub-card region with filler strategy), and oversized mixed objects fall back to normal mixed pages. | `/tmp/sbcl/src/runtime/gencgc.c:927-1050` | Habu has no anti-card-span placement policy for mixed/tagged objects; card tracking is coarse `0/1` per card (`src/runtime/heap.zig:733-823`). | Add non-spanning placement policy (or equivalent start-map) for mixed objects so card scanning can stay local and precise. |
| Root/card scan has different fast paths for spanning boxed pages, non-spanning pages (cons/vector), and small-mixed pages to reduce unnecessary traversal while preserving correctness. | `/tmp/sbcl/src/runtime/gencgc.c:2395-2531`, `/tmp/sbcl/src/runtime/gencgc.c:2571-2578` | Habu minor GC checks marked cards, then scans full remembered objects by object range (`src/runtime/gc.zig:191-203`), which can over-scan when one card is dirty. | Introduce card-granular scan routines by object-class/span behavior; avoid full-object scans when card-local scan is sufficient. |
| Write-protect + card-mark lifecycle minimizes rescans; sticky marks preserve special cases and are normalized post-GC. | `/tmp/sbcl/src/runtime/gencgc.c:2945-2966`, `/tmp/sbcl/src/runtime/gencgc.c:3998-4015` | Habu currently has binary card marks only and no sticky/normalized state transition (`src/runtime/heap.zig:733-823`). | Upgrade card state to multi-state (`clean/dirty/sticky`) and add post-collection normalization to cut repeated false-positive scans. |
| Newspace scavenge records newly allocated areas and rescans only required regions instead of always rescanning everything. | `/tmp/sbcl/src/runtime/gencgc.c:2610-2631`, `/tmp/sbcl/src/runtime/gencgc.c:2634-2667` | Habu uses one work list and scans remembered objects when cards are marked; no explicit new-area frontier lists. | Add explicit new-area frontier tracking for minor/major phases and re-scan scheduling driven by those frontiers. |
| Card table sizing is tied to heap/card geometry and codegen mask width to keep index math correct for runtime size changes. | `/tmp/sbcl/src/runtime/coreparse.c:1002-1043`, `/tmp/sbcl/src/runtime/coreparse.c:1093-1118` | Habu card table is derived from heap span but has no explicit mask-width contract exposed to JIT/runtime coordination. | Define explicit card-index contract (size/mask/shift) shared by runtime + JIT to make future heap resizing and JIT barriers stable. |

## Transfer Notes
- SBCL combines three independent signals before escalating collection:
  - allocation budget (`bytes_consed_between_gcs`),
  - free-space pressure (`dynamic_space_size - bytes_allocated`),
  - age/value signal (`generation_average_age` vs minimum age).
- Habu currently uses mainly one signal (`bytesUsed() >= gc_threshold`), so parity work should focus on multi-signal trigger control first.

## SBCL Transferable Invariants (Machine-checkable)

| Invariant | SBCL evidence | Habu enforcement target | Planned check type |
|---|---|---|---|
| Allocation regions must be closed at end of collection. | `/tmp/sbcl/src/runtime/gencgc-alloc-region.h:51-57`, `/tmp/sbcl/src/runtime/gencgc.c:3709` | `src/runtime/gc.zig` collection epilogue once regionized alloc lands | Debug assertion + regression test |
| Heap accounting must match underlying page/region usage. | `/tmp/sbcl/src/runtime/gencgc.c:4948-4953` | `src/runtime/heap.zig` (`bytesUsed`, `tenuredBytesUsed`, `losBytesUsed`) | `verifyHeapAccounting()` debug check |
| Post-GC trigger must remain ahead of current usage and clamped by available headroom. | `/tmp/sbcl/src/runtime/gencgc.c:4025-4030` | `src/runtime/heap.zig` trigger recompute path (to be added) | Runtime assert + benchmark gate |
| Per-generation trigger is reset from retained bytes + generation budget after each collection. | `/tmp/sbcl/src/runtime/gencgc.c:3779-3781` | Future Habu per-generation budget state in `src/runtime/gc.zig` | Unit test on trigger update |
| Promotion/raise policy must never raise oldest generation. | `/tmp/sbcl/src/runtime/gencgc.c:3937-3940` | Future Habu major scheduler | Scheduler invariant test |
| Card scanning for spanning boxed objects must include marked card plus spillover card logic. | `/tmp/sbcl/src/runtime/gencgc.c:2414-2417`, `/tmp/sbcl/src/runtime/gencgc.c:2440-2454` | `src/runtime/gc.zig` remembered/card scan paths | Differential scan test (spanning vs non-spanning) |
| Card state transition must preserve sticky semantics through write-protect reset and normalize after cycle end. | `/tmp/sbcl/src/runtime/gencgc.c:2947-2966`, `/tmp/sbcl/src/runtime/gencgc.c:4008-4015` | `src/runtime/heap.zig` card table state machine (to be upgraded) | State-transition unit tests |
| Newspace/frontier rescans must be bounded to known newly-allocated areas, not unbounded full-heap rescans. | `/tmp/sbcl/src/runtime/gencgc.c:2615-2631`, `/tmp/sbcl/src/runtime/gencgc.c:2634-2667` | `src/runtime/gc.zig` work frontier scheduling | Metrics invariant (`rescanned_bytes <= frontier_budget`) |
| Average-age signal must be computed from cumulative generation residency, not raw allocation volume only. | `/tmp/sbcl/src/runtime/gencgc-impl.h:204-214`, `/tmp/sbcl/src/runtime/alloc.c:429-436` | Habu survival/age telemetry and major gate control | Telemetry consistency tests |

## OCaml Pacing Heuristics -> Habu Mapping

| OCaml pacing heuristic | OCaml evidence | Habu current state | Habu action |
|---|---|---|---|
| Minor collection sets a half-heap trigger and schedules major slicing when half of minor heap is consumed. | `/tmp/ocaml/runtime/minor_gc.c:695-699` | Habu triggers by fixed from-space threshold (`src/runtime/heap.zig:1072-1074`) and does not schedule major slices from nursery pressure. | Add pre-trigger at fractional nursery occupancy that schedules incremental major work before full nursery exhaustion. |
| Domains perform opportunistic major slices while waiting at minor-GC synchronization barriers. | `/tmp/ocaml/runtime/minor_gc.c:824-833`, `/tmp/ocaml/runtime/minor_gc.c:849-859` | Habu has no opportunistic GC work scheduling during synchronization/idle waits. | Add bounded opportunistic major-slice hook for wait/spin windows. |
| Major pacing is driven by dual counters (`alloc_counter`, `work_counter`) and computes slice work from allocation, dependent allocation, and extra-heap pressure, using max of those signals. | `/tmp/ocaml/runtime/major_gc.c:899-1019` | Habu has no alloc-vs-work debt counters; major pacing is not modeled yet. | Introduce alloc/work debt counters and multi-signal slice sizing (`max(alloc_work, dependent_work, extra_work)`). |
| If GC work lags badly behind allocation, OCaml applies an explicit catch-up step to avoid runaway debt. | `/tmp/ocaml/runtime/major_gc.c:1026-1043` | Habu has no debt lag catch-up policy. | Add bounded catch-up policy when pending GC debt exceeds cycle budget multiple. |
| Major slices are chunked with explicit target/budget accounting; auto-triggered slices are interruptible and rescheduled on interrupt. | `/tmp/ocaml/runtime/major_gc.c:1103-1131`, `/tmp/ocaml/runtime/major_gc.c:2356-2368` | Habu GC execution is mostly whole-collection and not interruptible by slice contract. | Add chunked incremental major slices with interruptible mode and reschedule token. |
| Pacing reset aligns alloc/work counters after synchronous full cycles and re-seeds lower-bound work before next mark. | `/tmp/ocaml/runtime/major_gc.c:867-888` | Habu has no explicit pacing reset primitive. | Add post-full-cycle reset to clear drift and restore stable pacing baselines. |
| Runtime knobs expose GC speed/space controls (`space_overhead`, `minor_heap_size`, verbosity for slice sizing). | `/tmp/ocaml/manual/src/cmds/runtime.etex:162`, `/tmp/ocaml/manual/src/cmds/runtime.etex:176`, `/tmp/ocaml/manual/src/cmds/runtime.etex:187`, `/tmp/ocaml/runtime/gc_ctrl.c:167-179` | Habu GC knobs are mostly compile-time/config-time (`src/runtime/heap.zig:233-254`) with limited runtime tuning. | Add runtime-settable internal GC tuning knobs with telemetry-backed defaults. |
| Ramp-up/ramp-down tracks suspended/resumed allocation to keep pacing accounting accurate across deferred work windows. | `/tmp/ocaml/runtime/gc_ctrl.c:431-485` | Habu has no deferred-allocation accounting channel for pacing. | Add suspended/resumed allocation counters for phases that defer major work. |

## OCaml Shared-Heap Sweep/Compaction -> Habu Mapping

| OCaml shared-heap technique | OCaml evidence | Habu current state | Habu action |
|---|---|---|---|
| Sweeping is incremental and budgeted in words; sweep cursor advances by sizeclass and large alloc queues until budget is consumed. | `/tmp/ocaml/runtime/shared_heap.c:667-668`, `/tmp/ocaml/runtime/shared_heap.c:725-744` | Habu sweeps tenured and LOS as full passes per collection (`src/runtime/heap.zig:903-927`, `src/runtime/heap.zig:1024-1048`). | Add incremental sweep cursor and budget (`sweep_budget_words`) for tenured/LOS. |
| Heap cycle rotates color meanings and moves pools into unswept lists before the next cycle, then adopts orphan heaps. | `/tmp/ocaml/runtime/shared_heap.c:1638-1663` | Habu keeps per-object `marked` bits but no explicit unswept queues or cycle-color rotation. | Introduce unswept queues and cycle-state transitions to support incremental sweep correctness. |
| Allocation path can trigger local/global pool adoption and opportunistic sweeping to satisfy allocation without immediate heap growth. | `/tmp/ocaml/runtime/shared_heap.c:410-421`, `/tmp/ocaml/runtime/shared_heap.c:392-395` | Habu tenured alloc uses free-span reuse then bump; no sweep-on-allocation pathway (`src/runtime/heap.zig:825-853`). | Add on-demand bounded sweep step before expanding tenured/LOS footprint. |
| Pool sweep merges adjacent free runs, rebuilds freelists, and can release fully free pools back to global freelist/OS. | `/tmp/ocaml/runtime/shared_heap.c:605-619`, `/tmp/ocaml/runtime/shared_heap.c:658-663`, `/tmp/ocaml/runtime/shared_heap.c:255-277` | Habu coalesces free spans after full sweep only and does not return tenured slabs to OS. | Add in-sweep coalescing/release policy and optional page-unmap for fully free slabs. |
| Large allocations are tracked separately as unswept/swept queues with size-based sweep work accounting and custom finalizers. | `/tmp/ocaml/runtime/shared_heap.c:673-703`, `/tmp/ocaml/runtime/shared_heap.c:705-721` | Habu LOS tracks objects and frees dead entries but lacks explicit incremental queue accounting (`src/runtime/heap.zig:969-1048`). | Split LOS into unswept/swept queues and charge sweep work by object size. |
| Compaction is explicit multi-phase (evacuate, rewrite pointers, release evacuated pools, release freelist mappings) across global barriers. | `/tmp/ocaml/runtime/shared_heap.c:1160-1176`, `/tmp/ocaml/runtime/shared_heap.c:1446-1534` | Habu has no tenured compaction phase yet. | Design optional tenured compaction pipeline with forwarding pointers and phased barriers (future dot). |
| Heap/orphan statistics are moved with pool adoption/orphaning and verified against pool scans. | `/tmp/ocaml/runtime/shared_heap.c:832-857`, `/tmp/ocaml/runtime/shared_heap.c:1627-1633` | Habu tracks some GC stats but lacks deep accounting invariants for tenured/LOS fragmentation. | Add accounting invariants and verification tests for pool/span stats. |

## OCaml Transferable Invariants (Machine-checkable)

| Invariant | OCaml evidence | Habu enforcement target | Planned check type |
|---|---|---|---|
| Sweep cursor must advance monotonically and complete only when unswept queues are empty. | `/tmp/ocaml/runtime/shared_heap.c:725-739`, `/tmp/ocaml/runtime/shared_heap.c:1604-1608` | `src/runtime/heap.zig` incremental tenured/LOS sweep scheduler | Debug assertion + sweep regression test |
| Heap cycle must atomically move active pools into unswept queues before next cycle work begins. | `/tmp/ocaml/runtime/shared_heap.c:1650-1660` | Future Habu cycle transition (tenured/LOS cycle boundary) | State-transition unit test |
| Pool accounting must balance exactly (`pool_words`, `pool_live_words`, `pool_frag_words`, free words equation). | `/tmp/ocaml/runtime/shared_heap.c:1627-1633` | `src/runtime/heap.zig` tenured/LOS accounting validation | `verifyHeapAccounting()` invariant check |
| Opportunistic major slices must run only when work is available and must be bounded. | `/tmp/ocaml/runtime/minor_gc.c:849-861` | Habu opportunistic major-slice hook (future) | Runtime metric gate (`opportunistic_words > 0` only when debt > 0) |
| Slice work budget must be computed from target-vs-work counters and committed atomically as work completes. | `/tmp/ocaml/runtime/major_gc.c:1109-1113`, `/tmp/ocaml/runtime/major_gc.c:1125-1127` | Habu GC debt/slice accounting (`src/runtime/gc.zig`) | Counter-consistency tests |
| Auto-triggered major slices should be interruptible and re-request work when interrupted. | `/tmp/ocaml/runtime/major_gc.c:2356-2368` | Habu incremental major scheduler | Interruptibility integration test |
| Compaction pointer updates require forwarding-pointer contract for evacuated blocks before root/heap rewrite pass. | `/tmp/ocaml/runtime/shared_heap.c:1397-1404`, `/tmp/ocaml/runtime/shared_heap.c:1446-1453` | Future Habu tenured compactor | Compaction correctness property test |
| Compaction preconditions: no unswept adopted pools pending, empty minor space, empty mark stack at entry. | `/tmp/ocaml/runtime/shared_heap.c:1196-1212` | Habu compaction entry guard (future) | Precondition assertion set |
| Deferred-allocation ramp-up/ramp-down must preserve accounting through nested phases. | `/tmp/ocaml/runtime/gc_ctrl.c:443-471`, `/tmp/ocaml/runtime/gc_ctrl.c:483-485` | Habu deferred/debt accounting (future) | Nested-ramp accounting test |

## Habu GC Current-State Inventory

### Runtime Features (Implemented)

| Area | Current Habu behavior | Evidence |
|---|---|---|
| Layout modes | Supports `semispace` and `generational` scaffold. | `src/runtime/heap.zig:235-244` |
| Nursery collection | Copying nursery collector with root/copy/finalize phase timing. | `src/runtime/gc.zig:87-157`, `src/runtime/gc.zig:160-235` |
| Tenured/LOS regions | Separate tenured + LOS regions with own alloc pointers. | `src/runtime/heap.zig:266-313` |
| Remembered set primitive | Old->young card marking via write barrier (`card_table` byte marks). | `src/runtime/heap.zig:299-300`, `src/runtime/heap.zig:784-799` |
| Tenured/LOS sweep | Reclaims dead tenured/LOS objects and coalesces free spans. | `src/runtime/heap.zig:903-927`, `src/runtime/heap.zig:1024-1048` |
| Collector stats | Tracks gc count, bytes copied, phase timings, visited roots, promotions, barrier marks. | `src/runtime/heap.zig:382-394` |

### Telemetry/Tooling Inventory

| Metric surface | Currently emitted | Evidence |
|---|---|---|
| Runtime counters | `gc_{minor,major}_*`, pause phase timers, allocation sampling, survival/promotion histograms, `gc_root_vals`, `gc_promoted_bytes`, `wb_marks`. | `src/runtime/heap.zig` |
| GC bench JSON | Exports `p50/p95/p99`, mode averages, allocation sampling (`alloc_sample_*`), and survival/promotion histograms (`gc_survive_*`, `gc_promote_*`). | `bench/gc.zig` |
| Compare tool usage | Consumes gate schema, pause/throughput/RSS ratios, allocation hot-class summary, and survival/promotion ratios. | `tools/gc-compare` |
| Perf loop usage | Ingests `gc_compare` metrics + gate status and prints GC action context with bottleneck ranking. | `tools/perf-loop` |

### Missing Observability (Gap List)

| Missing signal | Why it blocks parity work | Primary target |
|---|---|---|
| Age distribution by survivor generation | Needed for adaptive tenuring thresholds, not just survive/promote totals. | `src/runtime/gc.zig` |
| Incremental sweep progress metrics | No per-slice progress signal for debt-based major pacing. | `src/runtime/heap.zig` |
| Fragmentation metrics by region | No visibility into tenured/LOS free-space quality vs just bytes used. | `src/runtime/heap.zig`, `bench/gc.zig` |
| Card-scan efficiency metrics | No ratio of marked cards to actually scanned/dirty cards. | `src/runtime/gc.zig`, `src/runtime/heap.zig` |
| Trigger/debt state export | No runtime export of trigger target, debt pending, catch-up events. | `src/runtime/heap.zig`, `bench/gc.zig` |
| Cross-run trend persistence | Cannot evaluate whether a GC tweak regresses over time windows. | `tools/perf-loop` |

## GC Feature Gap Matrix (SBCL vs OCaml vs Habu)

| Capability | SBCL | OCaml | Habu | Gap status | Follow-up dot |
|---|---|---|---|---|---|
| Adaptive post-GC trigger recompute | Yes (`auto_gc_trigger` clamp) | Yes (alloc/work pacing counters) | No (fixed threshold) | Missing | `habu-gc-gates-set-e17bc236` |
| Large-allocation trigger pressure | Yes (`trigger_bytes` on large allocs) | Yes (`extra_heap_resources` in slice work) | Partial (LOS threshold only) | High | `habu-debt-account-allocation-07cb1149` |
| Multi-signal pacing (alloc + age + pressure) | Yes | Yes | No | Missing | `habu-debt-integrate-trigger-c402efa2` |
| Incremental major slices | No (classic gen cycles, but structured raise policy) | Yes (slice target/budget) | No | Missing | `habu-major-gc-incremental-068b1148` |
| Opportunistic GC work during waits | Limited | Yes (`caml_do_opportunistic_major_slice`) | No | Missing | `habu-perf-loop-ingest-2b991d65` |
| Tenuring policy beyond size threshold | Yes (promotion counters + age gates) | Yes (work pacing + phase controls) | Partial (`promote_threshold`) | High | `habu-tenuring-adaptive-threshold-34c571a8` |
| Regionized allocation accounting | Yes (open/close alloc regions) | Yes (pool alloc + sweep queues) | Partial (bump + free spans, no region state) | Medium | `habu-tenured-alloc-segregated-942b726a` |
| Card-table richness | Yes (marked/sticky + WP interplay) | N/A (different major strategy) | Partial (binary card marks) | High | `habu-rset-tighten-card-ba8ce5c2` |
| Card-scan specialization by object span/type | Yes | N/A (pool/mark model) | No (coarse object-range scans) | High | `habu-rset-add-scan-13787e2c` |
| Incremental sweep with explicit budget | Partial | Yes | No | Missing | `habu-major-gc-pause-bee3923c` |
| Compaction of major/tenured space | Limited paths | Yes (explicit 4-phase compactor) | No | Missing | `habu-los-reuse-and-ca77f709` |
| Fragmentation-aware allocator policy | Yes (page accounting + thresholds) | Yes (pool stats + compaction triggers) | Partial (free-span coalescing) | Medium | `habu-tenured-alloc-fragmentation-35baabcd` |
| Runtime-tunable GC knobs | Yes | Yes | Limited init-time config | Medium | `habu-gc-gates-encode-374df105` |
| Machine-checkable parity gates | External/manual | External/manual | Partial checks in bench-check | Medium | `habu-gc-gates-wire-b71c2f49` |
| Cross-runtime parity reporting | Ad hoc | Ad hoc | Partial (`tools/gc-compare`) | Medium | `habu-bench-pack-add-cb3ac540` |

## Ranked GC Gap Backlog (Impact x Risk)

Measured baseline used for this ranking:
- `tools/gc-compare --json --iters=30 --live-mb=8 --heap-mb=64`
  - Habu avg pause `55.02 ms` vs SBCL `3.69 ms` (`14.93x` slower).
  - Habu copy phase share `99.98%` of pause (`avg_copy_ns / avg_pause_ns`).
  - Copied bytes per GC `8,396,576` (roughly full live set each cycle).
- `tools/perf-loop --json --iters=1 --scale=1`
  - Major remaining end-to-end bottlenecks are still workload dispatch/JIT paths, so GC changes must target pause/RSS first and avoid regressing CAS throughput.
- `tools/perf-loop --json --iters=1 --scale=1 --profile-mutator`
  - Enables `HABU_PROFILE_MUTATOR=1` during Maxima workload runs and emits `mutator_profile` (`wb_*`, `safepoint_*`) to isolate barrier/safepoint overhead in VM vs JIT paths.

| Rank | Gap | Measured signal | Impact | Risk | Priority | Execution dot(s) |
|---|---|---|---:|---:|---:|---|
| 1 | Adaptive nursery + trigger debt model | Full-live-set copying each GC; fixed trigger causes oversized pauses | 5 | 3 | 15 | `habu-nursery-policy-derive-d65d5879`, `habu-debt-integrate-trigger-c402efa2` |
| 2 | Incremental major slices with pause budget | No incremental major pacing; long monolithic copy dominates pause | 5 | 4 | 14 | `habu-major-gc-incremental-068b1148`, `habu-major-gc-pause-bee3923c` |
| 3 | Adaptive tenuring policy | Promotions are threshold-only, not survival-driven | 4 | 3 | 12 | `habu-tenuring-collect-age-66c01bf2`, `habu-tenuring-adaptive-threshold-34c571a8` |
| 4 | Remembered-set/card scan specialization | Coarse remembered-object scans inflate copy/scavenge work | 4 | 3 | 12 | `habu-rset-tighten-card-ba8ce5c2`, `habu-rset-add-scan-13787e2c` |
| 5 | Barrier/safepoint hot-path overhead cuts | Needed to keep mutator throughput while GC gets more adaptive | 4 | 3 | 12 | `habu-barrier-profile-mutator-812522db`, `habu-barrier-inline-hot-4222c4ad` |
| 6 | Tenured allocator + fragmentation metrics | Current free-span reuse lacks rich fragmentation visibility | 3 | 2 | 9 | `habu-tenured-alloc-segregated-942b726a`, `habu-tenured-alloc-fragmentation-35baabcd` |
| 7 | LOS policy tuning | LOS thresholds static; reuse policy can be workload-sensitive | 3 | 2 | 9 | `habu-los-threshold-auto-6d2a6cc1`, `habu-los-reuse-and-ca77f709` |
| 8 | Telemetry expansion (age/survival/hot alloc classes) | Key adaptive inputs missing for closed-loop tuning | 3 | 1 | 8 | `habu-gc-telemetry-sample-13149884`, `habu-gc-telemetry-track-230600dd` |
| 9 | CI parity gates and normalized reporting | Prevents silent regressions while high-impact GC work lands | 2 | 1 | 5 | `habu-gc-gates-wire-b71c2f49`, `habu-ci-fail-on-b00ee752` |
| 10 | Tenured compaction pipeline | Useful long-term for RSS/fragmentation, but highest complexity | 3 | 5 | 7 | `habu-los-reuse-and-ca77f709` (prereq), future compaction dot |

## GC Parity Threshold Contract

Implemented gate targets are encoded in `tools/gc-compare` and mirrored in `bench/README.md`.
Deterministic gate failure is wired via `--fail-on-gates --gate-level=<level>` in both `tools/gc-compare` and `tools/perf-loop`.

| Level | avg_pause_ratio_min (`sbcl/habu`) | p95_pause_ratio_min (`sbcl/habu`) | throughput_ratio_min (`habu/sbcl`) | rss_ratio_max (`habu/sbcl`) |
|---|---:|---:|---:|---:|
| milestone_2x_from_baseline | 0.1340 | 0.2224 | 0.1340 | 4.0 |
| competitive | 0.50 | 0.50 | 0.50 | 2.0 |
| parity | 1.00 | 1.00 | 1.00 | 1.20 |

Baseline reference used to derive the 2x milestone:
- Date: `2026-02-20`
- `avg_pause_ratio=0.0670`
- `p95_pause_ratio=0.1112`
- `throughput_ratio=0.0670`

## Adaptive Nursery Control Law (Derived)

Implemented in `src/runtime/gc.zig` as `deriveNurseryPolicy`:
- Inputs: `current_bytes`, `{min,max}_bytes`, `survive_bytes`, `copied_bytes`, `p95_pause_ns`, `target_pause_ns`.
- Derived signals:
  - `survival_ratio = survive_bytes / copied_bytes`
  - `pause_error = (p95_pause_ns - target_pause_ns) / target_pause_ns`
- Scale model:
  - shrink branch when `survival_ratio > 0.25` or `pause_error > 0`:
    - `scale = 1 - 0.60*survival_ratio - 0.35*max(pause_error, 0)`
  - grow branch otherwise:
    - `scale = 1 + 0.30*max(0.25 - survival_ratio, 0) + 0.15*max(-pause_error, 0)`
  - bounded to `[0.50, 1.50]`, with 5% deadband to avoid oscillation.
- Output: `target_bytes` aligned to heap alignment and clamped to `[min_bytes, max_bytes]`.

Runtime application status:
- Applied after each generational minor collection in `src/runtime/gc.zig` (`collectRootSet`).
- Updated live threshold is stored in `heap.gc_threshold`/`heap.nursery_target_bytes`.
- Runtime floor in `src/runtime/heap.zig` (`nurseryLiveFloor`) prevents policy shrink below current live nursery bytes plus headroom (`max(12.5% live, 64KiB)`), avoiding immediate-GC thrash after a shrink step.
- Counter deltas used by the policy are wrap-safe (`-%`) so long-running sessions cannot underflow on telemetry counter rollover.
- Telemetry exports include `gc_nursery_target`, `gc_nursery_scale`, `gc_nursery_survival`, `gc_nursery_pause_error`.
- Benchmark tuning now uses both micro GC stress and Maxima workload telemetry via `tools/gc-compare --with-maxima` (default Maxima stress config: `--maxima-scale=3 --maxima-nursery-mb=24`).

## Next Dots
- `habu-tenuring-collect-age-66c01bf2`: collect age distributions for adaptive tenuring policy.
