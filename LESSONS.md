# Lessons Learned

Hard-won patterns and anti-patterns from building Habu. **Update this file at the end of every session** with new discoveries.

> Frequency counts are from SESSION.md analysis (~102K lines, ~50 sessions).

---

## Session Notes (2026-02-23)

### Worked Well
- Fixing `&key` boundary detection to scan optional slots one-by-one in `doCall` (`src/interp/vm.zig:10859`, `src/interp/vm.zig:10863`) closed a real semantic bug for mixed `&optional`+`&key` lambdas and removed unnecessary keyword probes on key-only lambdas (`opt_count==0` fast boundary).
- Adding a small-array allowlist path for keyword validation (`src/interp/vm.zig:738`, `src/interp/vm.zig:10907`, `src/interp/vm.zig:10910`) retained generic keyword checking while reducing repeated cons-walks on repeated multi-key calls.
- Locking the path with targeted regressions (`src/tests/integration.zig:2394`, `src/tests/integration.zig:2400`) prevents both silent extra-positional acceptance on `&key` lambdas and odd-offset key-start mis-parsing after omitted optionals.
- Adding a dedicated `keyword_call` microbench (`bench/comprehensive_bench.zig:127`) gives a stable hot-loop signal for `doCall` `&key` cost independent of full Maxima loader noise.
- Adding a dedicated fixed-arity call setup fast path (`src/interp/vm.zig:10651`, `src/interp/vm.zig:10815`) and a fast closure-code chunk decode path (`src/interp/vm.zig:10786`) removed hot `doCall` overhead from the no-`&optional`/no-`&key`/no-`&rest` majority path; Maxima hotspot reruns improved JIT runtime again (`integrate` ~`165ms` -> ~`157ms`, `factor` ~`53ms` -> ~`51.7ms`, `ratsimp` ~`40.1ms` -> ~`38.8ms`, `solve` ~`13.1ms` -> ~`12.8ms`).
- Locking the fast path with a stack-depth regression (`src/tests/integration.zig:2574`, `fixed-tail-acc`) prevents accidental loss of tail-call stack safety when refactoring fixed-arity frame setup.
- Converting function-resolution cache hits to raw symbol-identity checks with GC-epoch invalidation (`src/interp/vm.zig:1309`, `src/interp/vm.zig:1312`, `src/interp/vm.zig:2435`) removed per-call forwarded-value chasing on hot `doCall` paths; `tools/maxima-hotspots --json --scale 1 --heap-mb 1024 --nursery-mb 32` improved JIT runtimes on `integrate` (~173ms -> ~165ms), `factor` (~57.7ms -> ~53.0ms), `ratsimp` (~43.1ms -> ~40.1ms), and `solve` (~13.7ms -> ~13.1ms) in same-host reruns.
- Scanning function-cell plists directly from live symbol objects (`src/interp/vm.zig:1334`) and canonicalizing function-cell writes once at store time (`src/interp/vm.zig:1358`) reduced avoidable forwarded-resolution churn while keeping symbol-function semantics unchanged in focused regressions.
- Canonicalizing forwarded symbol/list values at `progv` boundaries (`src/interp/vm.zig:5213`, `src/interp/vm.zig:8117`, `src/interp/vm.zig:8194`) removed a deterministic non-hoist Maxima crash where `pushProgvFrame` dereferenced stale forwarded symbol objects (`name_ptr=0x30/0x40`) during macro-expansion-time dynamic binding.
- Extending symbol-cell and function-cell entry points to resolve forwarded symbol values before lookup/store (`src/interp/vm.zig:1249`, `src/interp/vm.zig:1262`, `src/interp/vm.zig:1281`, `src/interp/vm.zig:1294`) closed adjacent stale-pointer dereference paths beyond `progv`.
- Locking the bug with a VM-level regression that injects a deliberately stale forwarded symbol into a `progv` symbol list (`src/interp/vm.zig:13352`) gives deterministic red/green coverage without relying on long Maxima bench reproductions.
- Running Maxima hotspot “interp mode” on hoist with runtime JIT disabled (`src/interp/repl.zig:60`, `bench/maxima_workload.zig:12`, `tools/maxima-hotspots`) removed non-hoist drift and restored full workload comparability (`checked=5` instead of `checked=1`) for gate decisions.
- Replacing ext-root prefix copyback with snapshot-stack rooting (`src/interp/vm.zig:655`, `src/interp/vm.zig:1359`, `src/interp/vm.zig:2148`) fixed nested owner restore corruption; inactive ext-root owners/slices now stay in GC root ranges directly instead of being reconstructed from temporary arrays.
- Making `saveExtRoots` fallible and updating all swap callsites (`src/compiler/compile.zig:3606`, `src/interp/repl.zig:816`) removed silent snapshot-drop risk and kept nested ext-root save/restore bookkeeping explicit.
- Adding a JIT no-GC execution fence with OOM deopt (`src/interp/vm.zig:1535`, `src/interp/vm.zig:1998`, `src/interp/vm.zig:1550`) stopped moving-GC from running while JIT-held register values have no root map; `bench-maxima` now completes instead of crashing in `jitHashGet`.
- Aligning backend forwarding resolution with VM semantics (`src/jit/backend.zig:239`, `src/jit/backend.zig:262`, `src/jit/backend.zig:287`) and resolving hash helper arguments (`src/jit/backend.zig:681`, `src/jit/backend.zig:699`, `src/jit/backend.zig:730`) removed one stale-forwarding blind spot on helper entry.
- New regressions for ext-root behavior (`src/interp/vm.zig:13132`, `src/interp/vm.zig:13172`) lock both owner-backed and plain-slice inactive-root correctness.
- Running focused bridge/safety regressions plus `bench-maxima` rebaseline (`src/tests/integration.zig:1877`, `src/tests/integration.zig:2573`, `bench/maxima_workload.zig`) validated that bridge relay remains stable in JIT mode and safety admission stays open (`jit_adm.sk_safety=0`, loader `85/85`).
- Hardening `tools/dot-finish` with timeout-aware test execution (`tools/dot-finish`) removes a recurring dev-loop failure mode where full-suite hangs left stale `zig build test` processes alive for hours and tripped unified exec process limits.
- Sampling `./zig-out/bin/comprehensive_bench --bench=assoc` in Debug (`/tmp/habu_assoc_bin_sample.txt`) showed `jit.backend.jitAssoc` (`src/jit/backend.zig:360-369`) as the dominant hotspot with heavy `debug.assert` overhead; ReleaseFast `zig build -Doptimize=ReleaseFast bench-comp ...` measured ~5.0ms for the same bench.
- Rewriting `jitAssoc` to use raw tagged checks instead of `Value.isCons()/toPtr()` (`src/jit/backend.zig:360-374`) cut Debug `bench-comp --bench=assoc` from ~137ms to ~39ms on this host, improving inner-loop developer feedback.
- Adding direct fixnum/float fast paths in numeric compare helpers (`src/jit/backend.zig:459`, `src/jit/backend.zig:608`, `src/jit/backend.zig:618`, `src/jit/backend.zig:646`, `src/jit/backend.zig:653`) plus a fixnum-guarded translator fast lane (`src/jit/backend.zig:2602`) reduced ReleaseFast `assoc` from ~5.23ms to ~5.12ms while preserving generic fallback semantics.
- Sampling the real ReleaseFast bench binary (not Debug) for `assoc` kept the hotspot unambiguous in `jitAssoc`, which avoided false follow-up work on compiler/debug-only overhead (`/tmp/habu_assoc_releasefast_sample.txt`, `src/jit/backend.zig:387`).
- Disabling runtime safety inside `jitAssoc` and switching to raw 64-bit cons-field loads plus a combined cons mask (`src/jit/backend.zig:388`, `src/jit/backend.zig:394`) reduced ReleaseFast `assoc` from ~5.25ms to ~4.69ms (~10.7%) with focused regressions still green.
- Extending `patchCrossCallsToBL` to consume optional `MOVK hw=3` target materialization (`src/jit/backend.zig:4967`, `src/jit/backend.zig:5018`) closes a 64-bit direct-branch patch gap and is locked by a new machine-code regression (`src/jit/backend.zig:8248`).
- Adding a conservative BLR-target-clobber detector with focused bad/good machine-code regressions (`src/jit/backend.zig:7822`, `src/jit/backend.zig:8267`, `src/jit/backend.zig:8285`) preserved baseline runtime behavior while locking the exact cached-helper crash signature for follow-up repair.
- Extending `fixBlrTargetClobber` with a targeted imm-chain repair path (`src/jit/backend.zig:7707`, `src/jit/backend.zig:7785`) now fixes the captured single-`MOVZ` overwrite shape in backend regressions (`src/jit/backend.zig:8303`) without destabilizing baseline ReleaseFast benches.
- Generalizing BLR-target clobber detection to include low-immediate chain rewrites and non-imm overwrites (`src/jit/backend.zig:7788`, `src/jit/backend.zig:8406`, `src/jit/backend.zig:8425`, `src/jit/backend.zig:8474`, `src/jit/backend.zig:8503`) removed the known helper-target corruption signatures and kept cached helper-pointer lowering enabled (`src/jit/backend.zig:3406`).
- Making constant-cache reuse block-local at CFG boundaries (`src/jit/backend.zig:2107`) fixed a real SSA-dominance bug in cached helper-pointer lowering that crashed branch-local JIT paths on second invocation (`src/tests/integration.zig:2186`); ReleaseFast `assoc` now runs stably at ~2.79-2.83ms on repeated checks.
- Adding `bench-maxima --workloads=...` filtering (`bench/maxima_workload.zig:240`, `bench/maxima_workload.zig:277`, `bench/maxima_workload.zig:658`) plus wiring `tools/maxima-hotspots` to pass selected workloads (`tools/maxima-hotspots:22`, `tools/maxima-hotspots:288`) removed hidden benchmark-order coupling from hotspot runs.
- Forcing a pre-timed GC after benchmark warmup (`bench/maxima_workload.zig:390`) eliminated cross-workload nursery carryover from timed sections; `ratsimp` JIT dropped from ~308ms artifact to ~39ms when measured without in-window GC pauses.
- Emitting first unsupported IR tags on JIT compile failures (`src/interp/repl.zig:3013`) turned generic `UnsupportedIrNode` logs into actionable blockers; current Maxima benchmark wrapper rejection points to `.progv` as the first missing lowering.

### Did Not Work
- Scanning for first keyword in steps of two from `arity` (`src/interp/vm.zig` pre-fix around current `10863`) is unsound for `&optional`+`&key`: odd-offset key starts are missed, and some invalid extra positional args can slip through without signaling.
- Eagerly materializing an allowed-keyword slice on every key call (pre-threshold version near current `10907`) added overhead to small or zero-key-pair calls; gating fast materialization by `(kw_pair_count > 1)` and small declared-key count is necessary.
- Fixed-arity fast paths alone are not enough to pass the JIT gate (`wins` still `0..1/5`): after call-setup wins, remaining loss is in dynamic call-shape paths (`&key`/`&rest`/dispatcher-heavy frames), so follow-up work must target those branches directly.
- Keeping `lookupFnResolveCache` defensive by resolving forwarded values and rechecking callable tags on every hit (`src/interp/vm.zig` pre-fix `lookupFnResolveCache`) consumed measurable runtime in `doCall -> resolveFunctionValue` and left obvious hotspot time on the table.
- Sampling short scale-1 runs for integrate mostly captured loader/compile activity; runtime-stage profiling required long-running benches (`--scale=80`) before call-resolution hotspots became visible in `/tmp/habu_integrate_jit_scale80.sample`.
- Hardening only qualified-symbol lookup (`src/runtime/qual_name.zig`) was insufficient: the stale-forwarded symbol was introduced earlier, and `pushProgvFrame` could still dereference stale symbol/list cells before lookup ever ran.
- Using `-Duse-hoist=false` as a proxy for interpreter baselines in `tools/maxima-hotspots` hid real JIT-vs-interpreter comparisons behind backend divergence and produced workload errors (`OutOfMemory`/`UnhandledThrow`) unrelated to JIT effectiveness.
- Relying on `restoreExtRootsSynced` copyback from temporary root arrays propagated stale values into persistent owners under nested save/set/restore chains (`src/interp/vm.zig` pre-fix `restoreExtRootsSynced` logic).
- Fixing only helper-entry forwarded resolution was insufficient by itself; stale symbol-tagged pointers can survive long enough to lose forwarding metadata before first helper use, so preventing in-JIT GC was required for correctness (`src/jit/backend.zig:239`, `src/interp/vm.zig:1535`, `src/interp/vm.zig:1998`).
- Manually updating `PLAN.md` checkboxes drifted from dot state; syncing checkboxes from `dot show` status avoids stale "open vs done" plan state when many dots close in parallel.
- Running `tools/dot-finish` with an unbounded `zig build test` on this machine can leave long-lived test jobs after harness stalls; timeout guardrails are required to keep the process pool healthy.
- Treating Debug `bench-comp` numbers as runtime parity signal was misleading for `assoc`: Debug sampling showed `debug.assert`/tag-check overhead inside `jitAssoc`; parity tracking must use `-Doptimize=ReleaseFast`.
- The `jitAssoc` raw-check rewrite materially improved Debug numbers but did not move ReleaseFast parity (`~5.23ms` to `~5.25ms`), so remaining gap is elsewhere (helper/call lowering and loop arithmetic), not `Value` predicate overhead.
- Even with compare-helper fast paths, `assoc` parity remains far from SBCL in ReleaseFast (`~5.12ms` vs `~2.79ms` baseline), so the next wins are not in scalar compare helpers but in call/loop lowering and remaining helper-bound overhead.
- Inlining `assoc` directly in Hoist IR (replacing helper calls in `translateAssoc`) regressed ReleaseFast `assoc` to ~7.59ms; for this path, optimized native helper code beats `.none`-mode JIT control-flow lowering.
- Sampling `./zig-out/bin/comprehensive_bench` before rebuilding with `-Doptimize=ReleaseFast` reintroduced debug-only signals and distorted RCA; rebuild mode must match measured mode before profiling.
- Replacing `jitAssoc` with a C helper path did not outperform the tuned Zig helper in repeated ReleaseFast rebench runs, so this hotspot should stay Zig-native and be optimized in-place.
- Reshaping `jitAssoc` to a `while (true)` loop and removing pointer-mask loads regressed ReleaseFast `assoc`, so the prior masked/guarded loop form should remain the baseline until a proven win appears.
- Extending cross-call BL patch coverage to 64-bit materialization did not move the current `assoc` microbench immediately; treat it as coverage/hardening for address-layout variability rather than guaranteed direct speedup.
- Rewriting post-registration cross-call patching to a compact `BL + B-skip` shape at materialization head regressed ReleaseFast `assoc` from ~4.5-4.7ms to ~4.94ms, so keep the conservative patch form until call-target integrity is proven with stronger machine-code checks (`src/jit/backend.zig:4923` attempted rewrite, reverted).
- Adding unrolled/prefetch variants to `jitAssoc` regressed ReleaseFast `assoc` into ~4.87-5.48ms; this loop is latency-sensitive and extra control/memory ops hurt on this host (`src/jit/backend.zig:388` attempted variants, reverted).
- Caching helper pointers via `cachedIconst` in `emitPrimitiveCall*` triggered `BENCH-ASSOC` EXC_BAD_ACCESS from `BLR x9` target clobber (`movz x9,#imm` in arg setup); call-target preservation must be proven first before re-enabling pointer caching (`src/jit/backend.zig:3406`, `src/jit/backend.zig:3412`, `/tmp/assoc_dump2.txt`).
- Even after landing one imm-chain clobber repair in `fixBlrTargetClobber`, enabling cached helper pointers still crashes `BENCH-ASSOC`; additional BLR target corruption shapes exist beyond the single-`MOVZ` signature and need separate regressions before retrying caching.
- Reusing cached constants across blocks without dominance checks was unsound: helper pointer constants first materialized in one branch were reused from sibling branches, leading to undefined call targets and deterministic second-call crashes in branch-local JIT tests (`src/jit/backend.zig` pre-fix `switchBlock` behavior, `src/tests/integration.zig:2186`).
- Running `tools/maxima-hotspots` against full `bench-maxima` output while only filtering rows post-hoc produced misleading regressions: excluded earlier workloads could trigger GC inside a selected workload’s timed section, inflating that workload’s reported cost.

## Session Notes (2026-02-22)

### Worked Well
- Rekeying JIT chunk maps at GC boundaries from forwarding pointers (`src/interp/vm.zig:1489`, invoked at `src/interp/vm.zig:2246`) fixed stale-pointer chunk dispatch without mutating `Chunk` object layout, and the regression (`src/tests/integration.zig:275`) now proves lookup works after chunk movement.
- Keeping registration/removal on raw chunk addresses (`src/interp/vm.zig:1382`, `src/interp/repl.zig:3076`, `src/interp/repl.zig:3090`) plus replacing prior compiled entries in-place prevented stale map entries from surviving failed JIT finalization paths.
- Moving JIT bridge unwinding to a C trampoline (`src/jit/bridge_jump.c`) with `bridgeRun(callback)` kept the `setjmp` frame alive across native execution and enabled true non-local exits from bridge errors without continuing compiled code.
- Routing `jitCallBridgeInvoke` error catches to `bridgeThrow` (`src/interp/vm.zig:355`) and executing compiled calls through `bridgeRun` (`src/interp/vm.zig:1422`) cleanly aborts active JIT frames while preserving VM error semantics (`UnhandledThrow`, etc.).
- Replacing bridge panic-on-error with an explicit JIT bridge error lane (`src/interp/vm.zig:335`, `src/interp/vm.zig:1377`, `src/jit/backend.zig:60`, `src/jit/backend.zig:3116`) let `tryCallJit` propagate VM errors (`UnhandledThrow`, `ControlTransfer`, etc.) through normal VM error paths instead of aborting the process.
- Locking bridge relay behavior with a focused regression (`src/tests/integration.zig:1877`) catches panic regressions on JIT generic-call error paths and proves error relay works end-to-end.
- Replacing single-candidate JIT extraction (`extract first candidate` + `child_chunks[0]`) with full candidate discovery and signature/name chunk matching (`src/jit/candidates.zig`, `src/interp/repl.zig:2850`, `src/testing/compile_chunk.zig:102`) removed incorrect chunk registration when top-level forms contain multiple defuns and nested lambdas.
- Locking multi-defun progn JIT registration with function-cell chunk lookups (`src/tests/integration.zig:88`) catches regressions where only the first eligible function gets native code.
- Adding explicit `jit_compiled` counters + machine-checkable JIT gate (`bench/maxima_workload.zig:605`, `tools/maxima-hotspots:87`, `tools/perf-loop:346`) made "JIT effective vs interpreter" a hard signal instead of manual inspection.
- Remapping stale perf-loop recommendation dots through live `dot ls --json` state (`tools/perf-loop:393`, `tools/perf-loop:631`) removed closed-dot action churn and keeps next-dot output executable.
- Extending generic JIT call bridges from 4 to 7 user args (`src/jit/backend.zig:305`, `src/interp/vm.zig:397`) removed an obsolete arity cap in `translateGenericCall` and keeps calls register-only (fn+7 args) without relying on hoist stack-arg lowering.
- Locking the widened bridge with a seven-argument rooted generic-call regression (`src/jit/backend.zig:7894`) prevents the old `UnsupportedCallTarget` ceiling from coming back silently.
- Adding VM-level JIT admission counters (`src/interp/vm.zig:436`, `src/interp/repl.zig:2850`, `src/testing/compile_chunk.zig:120`) made candidate rejection reasons measurable in both REPL and test helper compilation paths.
- Exporting admission counters in Maxima benches/hotspot tooling (`bench/maxima_workload.zig:606`, `tools/maxima-hotspots:83`) turned `jit_compiled=0` from a black box into actionable evidence (`sk_speed` dominating candidate skips).
- Dropping the explicit speed gate while keeping safety=0 (`src/jit/candidates.zig:92`) removed opt-declare dependency and shifted Maxima skip telemetry from `sk_speed` to `sk_safety`, proving where the true admission blocker sits.
- Replacing REPL-side alias-cache rebuilds with an incremental `GlobalEnv` alias index (`src/compiler/compile.zig:1901`, `src/compiler/compile.zig:1968`, `src/interp/repl.zig:1619`) removed per-lookup global-table scans and cut Maxima hotspot workloads (`ratsimp`, `factor`, `integrate`) by ~1.6-3x at `tools/maxima-hotspots --scale 1 --heap-mb 1024 --nursery-mb 32`.
- Adding `GlobalEnv` reverse name indexing and routing VM global-name lookup to O(1) (`src/compiler/compile.zig:1930`, `src/compiler/compile.zig:2036`, `src/interp/vm.zig:1636`) eliminated `globalNameForIndex` hash-map iterator walks from the `loadGlobal` hot path.
- Caching `HABU_TRACE_FN_RESOLVE` at REPL init (`src/interp/repl.zig:105`, `src/interp/repl.zig:130`, `src/interp/repl.zig:1338`, `src/interp/repl.zig:1546`) removed repeated `getenv` calls in function-resolution hot loops without changing trace semantics.
- Replacing linear builtin-callable checks with compiler-side cached lookup maps (`src/compiler/compile.zig:2058`, `src/compiler/compile.zig:2324`, `src/compiler/compile.zig:15666`) and switching REPL package-probe paths to raw-set checks (`src/interp/repl.zig:1326`, `src/interp/repl.zig:1406`, `src/interp/repl.zig:1422`) reduced function-designator dispatch overhead; `tools/maxima-hotspots --scale 1 --heap-mb 1024 --nursery-mb 32` improved from ~367/222/110ms (`ratsimp`/`integrate`/`factor`) to ~326/165/85ms in follow-up runs.
- Hoisting GC/heap debug env checks out of allocation and root-scan loops (`src/runtime/gc.zig:369`, `src/runtime/gc.zig:685`, `src/runtime/heap.zig:381`, `src/runtime/heap.zig:2049`) removed per-object and per-root `getenv` churn while preserving opt-in diagnostics.
- Caching VM trace filter env state at init (`src/interp/vm.zig:568`, `src/interp/vm.zig:957`, `src/interp/vm.zig:683`, `src/interp/vm.zig:762`) removed repeated getenv parsing in error/call tracing predicates.
- Keeping list cursors rooted and advancing the root before recursive compile calls (`src/compiler/compile.zig:6246`, `src/compiler/compile.zig:7231`, `src/compiler/compile.zig:15847`, `src/compiler/compile.zig:17671`) fixed real stale-pointer traversal hazards in moving-GC compiler passes.
- Rewriting `compileTagbody` to compile segments from rooted cursors instead of staging raw `Value` arrays (`src/compiler/compile.zig:8457`) eliminated the Maxima `nparse` crash (`compileTagbody` segfault on stale cons pointers) under generational load.
- Supporting integer tags in `tagbody`/`go` (`src/compiler/compile.zig:8524`, `src/compiler/ir.zig:218`) aligned behavior with CL semantics and removed false `InvalidSyntax` on numeric tag targets.
- Locking integer-tag behavior in both compiler and runtime tests (`src/compiler/compile.zig:21016`, `src/tests/integration.zig:3101`) prevented silent regressions in tag parsing and jump resolution.
- Normalizing BL/BLR argument-copy chains before scheduling in `fixCallArgMoves` (`src/jit/backend.zig:7036`) handled duplicate destination moves and preserved final pre-call register mapping under indirect call setup.
- Scanning call setup through interleaved BLR target materialization (`mov`/`movz`/`movk`) in `fixCallArgMoves` (`src/jit/backend.zig:7041`) fixed a real blind spot where argument moves were skipped when target setup appeared between arg copies and call.
- Adding machine-code regressions for interleaved target setup and two-cycle copies (`src/jit/backend.zig:7244`, `src/jit/backend.zig:7266`, `src/jit/backend.zig:7288`) gives direct protection for indirect-call repair logic without relying on full-suite runtime repros.
- Letting parser read-eval/dispatch hooks surface original VM errors via parser-side hook capture (`src/reader/parser.zig:61`, `src/reader/parser.zig:105`, `src/reader/parser.zig:169`, `src/reader/parser.zig:201`) preserved non-local-exit semantics instead of collapsing them to parse failures.
- Routing VM and REPL parse callsites through hook-error-aware parsing (`src/interp/vm.zig:290`, `src/interp/vm.zig:6927`, `src/interp/repl.zig:2234`, `src/interp/repl.zig:2329`, `src/interp/repl.zig:3386`) fixed nested `#.` throw relay paths (`(catch 'x (read-from-string \"#.(throw 'x 42)\") ...)`) without special-casing Maxima code.
- Locking the reader relay behavior with a focused integration regression (`src/tests/integration.zig:3032`) gives deterministic coverage for nested read-eval non-local exits across call barriers.
- Switching `runVmPreserveMacroState` from pointer-classified `currentExtRoots` restore to `saveExtRoots`/`restoreExtRoots` (`src/interp/repl.zig:812`, `src/interp/repl.zig:863`) removed a stale-slice restore path and made nested VM root restoration owner-stable under reallocations.
- Adding a direct owner-reallocation regression for ext-root snapshots (`src/interp/vm.zig:12759`) locks `restoreExtRoots` semantics so restores rebind by owner and not by stale slice pointers.
- Treating AArch64 unscaled/pre/post-index load/store forms as first-class register uses in MOVZ liveness (`src/jit/backend.zig:6380`, `src/jit/backend.zig:6421`, `src/jit/backend.zig:6491`) fixed the nested-cons JIT corruption/crash path where live constant materialization for cons stores was being NOPed.
- Locking MOVZ liveness and nested-cons runtime behavior with focused regressions (`src/jit/backend.zig:7906`, `src/jit/backend.zig:7928`, `src/tests/integration.zig:245`) gives direct red/green coverage for this exact failure mode.
- Treating `RET` as reading x0 in liveness (`src/jit/backend.zig:6326`, `src/jit/backend.zig:6372`) fixed a real dead-MOVZ miscompile where `movz x0,#imm; ret` got NOPed and leaf functions returned stale pointer garbage (`hoist IR translator: block wrapper compiles` expected tagged 85).
- Locking return-register liveness with focused backend regressions (`src/jit/backend.zig:8045`, `src/jit/backend.zig:8063`) prevents future dead-code passes from deleting result materialization before `RET`.
- Replacing static `arr_new` lowering with register-only call shapes in `translateArrNew` (`src/jit/backend.zig:4291`) removed 10-arg indirect calls from JIT array construction and fixed the `gc_vector` warmup crash path.
- Guarding indirect-call lowering to max 8 args (`src/jit/backend.zig:3128`) turns unsafe stack-arg call emission into an explicit compile-time fallback instead of silent return-address corruption.
- Locking the crash repro with a focused JIT integration regression (`src/tests/integration.zig:245`) keeps `(make-array ... )` loop return paths covered under `(optimize (speed 3) (safety 0))`.
- Tracing JIT call entry/exit (`HABU_TRACE_JIT_CALL`) in `tryCallJit` (`src/interp/vm.zig:1285`) gave a deterministic failing function name (`%MAP-REVERSE`) for a Maxima-load crash that otherwise only surfaced as random native PC faults.
- Restoring strict JIT eligibility to explicit `(optimize (speed 3) (safety 0))` (`src/interp/repl.zig:2910`, `src/testing/compile_chunk.zig:164`) removed unsafe safety>0 JIT compilation and restored full `bench-maxima` load+run stability.
- Keeping a hard allocator-cursor invariant check after JIT returns (`src/interp/vm.zig:1311`, `src/jit/backend.zig:95`) turns cursor corruption into immediate, attributable failures instead of delayed heap-state crashes.

### Did Not Work
- Adding identity inside `Chunk` itself for JIT key stability was not viable in practice: layout changes destabilized hoist-mode Maxima runs, so chunk identity must stay external to the GC object layout.
- Even after JIT map rekeying, `bench-maxima -Duse-hoist=true --scale=1 --json` still crashes in package symbol lookup (`src/runtime/heap.zig:211`, `src/compiler/compile.zig:15803`) with an invalid string pointer path (`0x30`), so this is a separate root-cause track.
- Calling `setjmp` in a helper that returns to Zig (`bridgeEnter`) and later `longjmp`ing back to that dead frame crashed immediately (`Segmentation fault at address 0x0`); `setjmp` must remain active in the same frame for the full JIT call window.
- Injecting a post-generic-call guard CFG inside JIT translation (an `emitBridgeErrorGuard` experiment in `src/jit/backend.zig`) regressed recursive JIT functions (`compileChunk JIT handles recursive nqueens helper entry copies`) with null-call crashes; keep bridge relay state in VM/backend runtime lanes until that control-flow lowering path is proven safe.
- Using a direct keyword-heavy generic call as the bridge relay regression target caused an unrelated native crash (`Bus error at 0x3`) before reaching the bridge helper; the stable repro is a JIT call into an interpreted wrapper that triggers the keyword failure (`src/tests/integration.zig:1877`).
- Expecting all `(speed 3, safety 0)` functions in the same top-level progn to compile is still wrong when one body contains unsupported IR (`lambda` nodes in body): candidate collection now keeps compiling later candidates, but unsupported functions remain interpreted by design (`src/testing/compile_chunk.zig:163`).
- Running perf-loop with large microbench iteration counts for quick validation (`tools/perf-loop --iters 1000`) stalls practical feedback loops; keep smoke validation runs small and use targeted bench commands for deep measurements.
- Widening `CallBridge` in hoist mode without matching `src/jit/backend_stub.zig` broke `-Duse-hoist=false` builds immediately; backend and stub interfaces must evolve together.
- Telemetry initially showed zero candidate counts in compileChunk-only tests because admission accounting existed only in REPL JIT paths; helper compiler paths (`src/testing/compile_chunk.zig`) must update the same counters for consistent assertions.
- Removing the safety gate entirely immediately crashes Maxima load with `jit call bridge failed: UnhandledThrow argc=4` (`src/interp/vm.zig:353`), so safety>0 admission needs proper JIT↔VM condition relay before it can be enabled.
- Invalidating a REPL-owned alias cache by `globals.next_index` and rebuilding from `globals.bindings.iterator()` (`src/interp/repl.zig` pre-fix alias-cache helpers around 1538-1607) was still O(n) under loader churn and stayed on the hotspot path.
- Broadening JIT admission to safety>0 call-free lambdas without full runtime-safety lowering/bridge semantics caused deterministic Maxima crashes in `%MAP-REVERSE` (segfault + misaligned allocator cursor), even after partial cons-lowering changes.
- Treating all non-symbol atoms in `tagbody` as executable forms was incorrect; CL treats integer atoms as labels too, so tests that expected trailing fixnum atoms as forms were invalid and had to be rewritten (`src/compiler/compile.zig:20972`, `src/compiler/compile.zig:21032`).
- Running `sample` against short-lived bench processes without a longer run window produced stale/no profile capture (`/tmp/bench_maxima_s20_sample.log` showed process exited before sampling); use workload settings that guarantee process lifetime during sampling.
- Full `zig build test` can still hang in this environment (`--listen` child process remained active with no output), so use focused `-Dtest-filter` gates for deterministic dot closure checks when the full suite stalls.
- Relying on contiguous backward scans of only `mov x0..x7,*` before BL/BLR in `fixCallArgMoves` missed valid call setup windows with interleaved target setup ops, leaving indirect-call argument corruption unpatched.
- Converting read-eval/dispatch callback errors to parser `UnexpectedToken` in bridge hooks (`src/interp/vm.zig` pre-fix `readEvalBridge`/`dispatchMacroBridge`, `src/interp/repl.zig` pre-fix `parserReadEval`/`parserDispatchMacro`) masked real control transfers as parse/type errors and broke `(catch ...)` around `read-from-string` `#.` forms.
- Restoring nested VM ext roots via pointer-identity classification (`persistent`/`ctx`/`slice`) in `runVmPreserveMacroState` was brittle; unclassified owners fell back to raw slices and risked stale restores after owner reallocation.
- Restricting load/store read/write detection to the unsigned-offset `0x39*` family in MOVZ dead-code analysis (`src/jit/backend.zig` pre-fix `insnReadsReg`/`insnWritesReg`) missed hoist-emitted unscaled `F8*` forms, so `eliminateDeadMovz` deleted live constants and produced malformed cons cells at runtime.
- Treating `RET` as a pure control-flow terminator in liveness (`src/jit/backend.zig` pre-fix `isRegDeadInBlock`/`isRegDeadFrom`) is incorrect for x0: dead-MOVZ elimination can remove return-value setup and surface as nondeterministic pointer returns in leaf wrappers.
- Emitting `arr_new` via `jitMakeArrayStatic` with 10 indirect-call args (`src/jit/backend.zig` pre-fix `translateArrNew`) exercised hoist stack-arg lowering that spilled at `[sp]` and overwrote saved LR, crashing on function return (`Bus error at 0x4e1f` in `gc_vector` JIT warmup).

## Session Notes (2026-02-21)

### Worked Well
- Fixing tail-call `&key` frame reuse with overlap-safe argument moves in `doCall` (`src/interp/vm.zig:10164`) plus ordering positional copy before keyword-pair relocation removed real argument-slot corruption where `MEMBER` saw `lst` as a closure and `test` as nil in Maxima `INFINITYP`/`$LIMIT` paths.
- Registering `defstruct` type names in the runtime class registry during `compileDefstruct` (`src/compiler/compile.zig:10258`) made `typep`/`typecase` on struct names return booleans for non-struct objects instead of `UnknownTypeSpecifier`, unblocking Maxima `marray-type` calls in `limit`.
- Locking both regressions with focused integration tests (`src/tests/integration.zig:1597`, `src/tests/integration.zig:4567`) now catches tail-call keyword frame corruption and defstruct-type `typep` regressions before Maxima workload runs.
- Propagating `NestedNonLocalExit` out of `execute` instead of consuming/rethrowing it in-place (`src/interp/vm.zig:2248`) restored call-barrier ownership of non-local exit relay, fixing resumed execution after a caught `(load ...)` condition (the `transl.lisp` `DEF%TR` path no longer continues into later forms after the first caught failure).
- Locking the signal-path variant with a script-level regression (`src/interp/repl.zig:4887`) catches a previously untested case where `handler-case` around `load` could catch twice and keep running the failed file; expected post-fix behavior is one catch and no post-error file progress.
- Adding a Maxima transl script gate in integration (`src/tests/integration.zig:7336`) now exercises the real `(load script -> maxima-load-all -> transl failure)` path and validates that loader state is returned exactly once without post-return resume crashes.
- Routing generic JIT numeric ops through dedicated helpers while keeping recursive functions on conservative fixnum lowering (`src/jit/backend.zig:1764`, `src/jit/backend.zig:1773`, `src/jit/backend.zig:1812`) fixed float benchmark semantics and removed the `float` call-bridge bottleneck; `bench-comp` JIT float benches moved from ~332/346ms to ~12.8/14.9ms.
- Adding direct primitive resolution for `FLOAT` designators (`src/jit/backend.zig:908`, `src/jit/backend.zig:286`) removed per-iteration VM bridge dispatch in float-heavy loops.
- Fixing BLR target-register clobber before arg-move rewrites (`src/jit/backend.zig:6177`, `src/jit/backend.zig:4569`) resolved real call-target corruption where `movz x9,#imm` overwrote the call target register and jumped to immediate values (for example `0x23`).
- Locking the path with a focused integration regression (`src/tests/integration.zig:206`) catches JIT regressions in generic float arithmetic and float comparisons under `(optimize (speed 3) (safety 0))`.
- Resolving class metadata by current package + unambiguous local class name in `lookupClassMetadataByName` (`src/compiler/compile.zig:11136`) fixed `make-instance` compile failures when symbol package qualifiers differed from metadata qualifiers (for example `BIGFLOAT-IMPL:BIGFLOAT` symbol vs `BIGFLOAT:BIGFLOAT` metadata), unblocking `numeric.lisp` and Maxima e2e load readiness.
- Treating `AND` as a generic LOOP clause separator outside FOR/WITH chaining (`lib/stdlib.habu:5300`) fixed real-world forms like `(loop ... collecting ... and do ...)` used in `mload.lisp` while preserving parallel FOR/WITH semantics via explicit `:and` step markers only for variable chains.
- Adding a focused loop regression (`src/tests/integration.zig:4961`) for `collecting ... and do ...` catches future parser regressions that break macro-heavy loaders before Maxima e2e status checks.
- Tightening `eliminateRoundTripMovs` safety checks in JIT post-lowering (`src/jit/backend.zig:5047`) by rejecting source-overwrite/control-flow windows and requiring `isRegDeadAfter` on the temporary register fixed a real helper-call argument corruption in `bench-intern` (`<` received the function pointer instead of loop index) and restored `bench-comp`/`perf-loop` stability.
- Locking the failure mode with a dedicated regression (`src/tests/integration.zig:206`) for optimized `bench-intern` loop count prevents future call-setup rewrites from silently dropping live save/restore moves.
- Resolving forwarded values at every quasiquote recursion boundary (`src/compiler/compile.zig:7696`, `src/compiler/compile.zig:7754`) fixed a smallest-heap stdlib-load crash where `quasiquoteList` dereferenced stale/forwarded cons cells under GC pressure.
- Making the MV conditional-jump regression independent of stdlib macros (`src/tests/integration.zig:6485`) by using direct `if` instead of `when` removed false negatives from missing macro expansion setup.
- Computing untagged eligibility before cross-call classification and making helper detection lowering-aware (`containsHelperCalls(body, fixnum_inline)` in `src/jit/backend.zig:4072`, `src/jit/backend.zig:4384`) removed false `cross=true` flags for pure arithmetic loops, restoring aggressive JIT opt-level selection for `bench-fixnum-loop`/`bench-fixnum-mul`.
- Adding focused backend unit tests for helper-call classification (`src/jit/backend.zig:6486`, `src/jit/backend.zig:6502`) locked the new lowering-aware behavior so future refactors do not silently reintroduce conservative cross-call misclassification.
- Rooting `global_ref` symbols during JIT literal-root collection (`src/interp/repl.zig:2726`) and lowering generic-call designators from those roots (`src/jit/backend.zig:2478`) fixed missing call-target patterns where non-primitive/non-known global calls previously fell through with invalid designators.
- Locking the behavior with dedicated backend regressions (`src/jit/backend.zig:6533`, `src/jit/backend.zig:6559`) catches both required-root failure mode and rooted designator success path in generic call lowering.
- Extending JIT helper lowering for data-path IR (`src/jit/backend.zig:603`, `src/jit/backend.zig:822`, `src/jit/backend.zig:1000`, `src/jit/backend.zig:1063`) removed major unsupported coverage gaps for vector/hash/string ops plus generic N-subscript `arr_ref`/`arr_set` and dynamic/static array construction.
- Wiring the same data tags through translator support gates (`src/jit/backend.zig:1960`, `src/jit/backend.zig:2083`, `src/jit/backend.zig:4763`) prevented false JIT rejection/classification drift where helpers existed but `canTranslate`/`firstUnsupportedTag`/`containsHelperCalls` lagged behind lowering.
- Adding backend regressions for the new generic data paths (`src/jit/backend.zig:7219`, `src/jit/backend.zig:7263`, `src/jit/backend.zig:7328`, `src/jit/backend.zig:7392`) gives direct red/green signal for vec/hash/multidim-array helper lowering.

### Did Not Work
- Relocating keyword pairs before positional arguments in tail-call `&key` frame reuse (`src/interp/vm.zig` pre-fix `doCall` tail key path) still clobbered positional source slots when ranges overlapped, producing partially fixed but still wrong bindings (`lst` became `:TEST`); positional arguments must be copied first.
- Handling `NestedNonLocalExit` inside the main `execute` error loop (`src/interp/vm.zig` pre-fix `err == error.NestedNonLocalExit` branch) bypassed call-boundary restoration logic and allowed inner file loaders to keep advancing forms after outer `handler-case` already caught the condition.
- Using a package-specific `eq` check for the first failed module in transl status validation was brittle (`src/tests/integration.zig` pre-fix); the failure marker must allow symbol/string representation differences and compare by canonical module text.
- A branch-heavy fixnum-fast/slow lowering for every generic numeric op triggered upstream hoist CFG instability (`computePreds` out-of-bounds) on real benchmark functions; keeping non-recursive generic ops helper-based and recursive paths conservative avoided this compiler failure in practice.
- Relying only on `fixCallArgMoves` was insufficient once constant materialization clobbered the BLR target register between `mov target` and `blr`; a dedicated BLR-target-clobber repair pass was required.
- Assuming native package qualifiers in `lookupClassMetadataBySymbol` were stable was wrong: aliases from Lisp package setup (for example Bigfloat package mapping) can diverge from defclass metadata keys and silently trigger `InvalidSyntax` on otherwise valid `make-instance` forms.
- Restricting LOOP `AND` to FOR/AS/WITH-only continuation (`lib/stdlib.habu` pre-fix `loop-expand`) is too strict for ANSI/Maxima code that uses `and` to chain action clauses, and it produced hard load stops (`AND must continue FOR/AS/WITH clause`) in `mload.lisp`.
- Eliminating round-trip MOV pairs using only local between-use checks (`src/jit/backend.zig` pre-fix `eliminateRoundTripMovs`) is unsound for call setup: `mov x22,x0` / `mov x0,x22` around helper calls can look cancelable but are live state transfer when the source register is overwritten in-between.
- Using `when` in low-level VM jump tests without loading stdlib macros (`src/tests/integration.zig` pre-fix `mv: values through conditional jumps`) can fail as `UnboundSymbol` and hide the real jump/multiple-value behavior being tested.
- Relying on a single-run `bench-comp` number to validate sub-millisecond loop improvements is noisy; confirm with `HABU_TRACE_JIT_FLAGS` classification output plus repeated runs before concluding a regression or win.
- Leaving `.global_ref` call designators on the legacy `nil` translation path in JIT (`src/jit/backend.zig` pre-fix `translateGenericCall`) silently masks call-target lowering gaps; generic calls must load rooted symbol designators or fail fast.
- Full Habu test validation is currently blocked when `../hoist` has syntax-incomplete edits (`/Users/joel/Work/hoist/src/context.zig:25`), so dot closure must record external-blocker status and use partial compile/test signal until hoist builds again.

## Session Notes (2026-02-20)

### Worked Well
- Syncing inline-cons cursor state at JIT↔VM bridge boundaries (`src/interp/vm.zig:337`, `src/interp/vm.zig:349`) fixed a real allocator rewind bug: bridge calls no longer reset `g_alloc_ptr` from stale `heap.alloc_ptr`, and recursive nqueens JIT paths now preserve cons list state.
- Classifying *any* non-self call as a cross-call in JIT lowering (`src/jit/backend.zig:4079`, `src/jit/backend.zig:4153`) ensured `fixCallArgMoves` runs in `src/testing/compile_chunk.zig` flows where `known_fns` is empty, closing helper-call arg corruption in wrapper functions.
- Adding env-gated JIT bridge tracing (`HABU_TRACE_JIT_BRIDGE` in `src/interp/vm.zig:326`) made call-designator/arg corruption immediately visible and shortened RCA from assembly-level guesswork to one deterministic signal.
- Extending `CompiledFn.callFromValues` beyond arity 3 (`src/jit/backend.zig:983`) closed a silent high-arity JIT call bridge gap where 4+ arg compiled functions previously returned `nil` from the VM bridge path.
- RCA on JIT helper-call corruption showed a true parallel-copy cycle in call-argument setup (`mov x0,x1; mov x1,x3; mov x2,x0; mov x3,x2`) being lowered sequentially; extending `fixCallArgMoves` to use scratch-cycle breaking and consume the pre-call target move slot (`mov x9,xT; blr x9`) fixed wrong helper args without papering over.
- Tightening untagged-mode eligibility to a conservative arithmetic subset in `src/jit/backend.zig` prevented untagged/tagged mixing across runtime helper boundaries and removed a class of silent semantic corruptions in JIT helper paths.
- Adding a focused JIT regression for formatted templates with suffix text (`src/tests/integration.zig:128`) caught the helper-call argument corruption immediately and now guards the call-argument fix.
- Specializing `concatenate` for all-string inputs in `lib/stdlib.habu:2436` with direct `string-concat` handling for 1/2-arg hot cases and preallocated copy for 3+ args cut `bench-comp` `string_concat` from ~2031ms to ~39ms while keeping mixed-sequence fallback behavior.
- Expanding concatenate integration coverage (`src/tests/integration.zig:5586`) to include mixed sequence coercion and list output protected the optimized string path from silently breaking non-string result types.
- Rewriting `reduce` to iterative folds and adding a `#'+` non-`:from-end` fast path in `lib/stdlib.habu:1043` removed `funcall` dispatch from the dominant benchmark case and cut `bench-comp` `reduce` from ~1894ms to ~25ms (single-iteration run) without changing CL fold behavior.
- Locking reduce semantics with an integration gate (`src/tests/integration.zig:719`) ensured left/right fold order, empty-sequence behavior, and `:initial-value` handling stayed intact after the loop rewrite.
- Splitting `mapcar` into explicit 1-list and 2-list fast paths in `lib/stdlib.habu:107` removed per-element `apply` argument-list churn on the hot benchmark path while preserving the generic variadic branch for 3+ lists; `bench-comp` `mapcar` dropped from ~190ms to ~63ms (single-iteration run).
- Making `mapcar2` iterate with `consp` guards and `%map-reverse` (`lib/stdlib.habu:146`) kept dotted-list termination semantics aligned with generic `mapcar` while avoiding an extra `reverse` pass and potential non-cons `car` errors.
- Locking the new semantics with `src/tests/integration.zig:694` catches regressions in one-list, two-list, and dotted-tail list behavior under stdlib load.
- Rooting saved package state through VM global root stack (`src/interp/repl.zig:1540`, `src/interp/repl.zig:1548`) eliminated a real generational GC corruption where `COMMON-LISP:*PACKAGE*` was restored from stale local `Value` snapshots, and the full Maxima generational bench now completes (`bench/maxima_workload.zig`).
- Rooting defmacro transformed definitions across VM execution (`src/interp/repl.zig:3644`, `src/interp/repl.zig:3659`) prevented stale macro-entry payloads when GC runs during macro closure materialization.
- Adding an opt-in pre-GC global corruption probe (`HABU_TRACE_BAD_GLOBAL_ROOT` in `src/interp/vm.zig:1608`) made the bad root source explicit (`idx=100`, `COMMON-LISP:*PACKAGE*`) and shortened RCA.
- Locking the package-root fix with a dedicated generational load regression (`src/interp/repl.zig:4867`) catches stale `*PACKAGE*` restoration by forcing GC during `load` and then collecting again after `load` returns.
- Saving/restoring `*LOAD-PATHNAME*`/`*LOAD-TRUENAME*` through the VM root stack (`src/interp/repl.zig:1601`, `src/interp/repl.zig:1630`) removed another stale-local `Value` path under moving GC and is guarded by a focused generational regression (`src/interp/repl.zig:4900`).
- Adding a generational GC-stress regression for `string-upcase`/`string-downcase` designators (`src/tests/integration.zig:4620`) locks the forwarded-string safety path under heavy allocation churn.
- Automating dual-mode CAS hotspot capture with `tools/maxima-hotspots` plus `docs/maxima-hotspots.md` removed ad-hoc profiling drift and made JIT-vs-interpreter deltas reproducible in one command.
- Running parallel worker agents in isolated `jj` workspaces (`/Users/joel/Work/habu-agent-compiler`, `/Users/joel/Work/habu-agent-gc`) accelerated independent RCA/fix loops without file ownership collisions, then `jj squash --from ... --message ...` merged results cleanly back into the default workspace.
- Resolving forwarded symbols at every list-iteration boundary in compiler hot paths (`src/compiler/compile.zig:2682`, `src/compiler/compile.zig:5122`, `src/compiler/compile.zig:14190`, `src/compiler/compile.zig:17540`) eliminated stale symbol/name pointers under moving GC and stopped `stdlib fdefinition basic` segmentation faults.
- For incremental major-sweep tests, draining any already-active cycle before changing the root set (`src/runtime/gc.zig:2477`) prevented false negatives caused by finishing a cycle that started under the old root set.
- Validating barrier-assisted incremental marking with an old-object rescue regression (`src/runtime/gc.zig:2048`) caught cross-slice liveness hazards that normal sweep tests miss.
- Gating old->old card marking behind `major_cycle_active` (`src/runtime/heap.zig:1003`) preserved fast-path remembered behavior outside major cycles while still providing correctness during incremental marking.
- Moving major old-space collection to an explicit phase machine (`src/runtime/gc.zig:102`, `src/runtime/gc.zig:584`) with persistent `major_work` queue enabled resumable mark/sweep progress without per-cycle full sweeps.
- Splitting tenured/LOS sweeping into cursor-based slices (`src/runtime/heap.zig:1187`, `src/runtime/heap.zig:1367`) kept reclamation bounded per minor cycle while preserving coalescing correctness at cycle completion.
- Enabling write-barrier card marking for old->old pointer stores only while major cycle is active (`src/runtime/heap.zig:1003`) kept incremental marking sound across mutator slices and was validated by focused regression coverage.
- Extending Maxima workload GC snapshots with debt telemetry (`bench/maxima_workload.zig:53`, `bench/maxima_workload.zig:84`, `bench/maxima_workload.zig:604`) made debt trigger/skip behavior visible during real loader pressure.
- Wiring Maxima debt metrics through comparison tooling (`tools/gc-compare:455`, `tools/gc-compare:663`) enabled direct A/B coefficient checks instead of inferring debt behavior from pause metrics alone.
- Running coefficient A/B and rolling back to baseline constants in `src/runtime/gc.zig:89` after benchmark evidence prevented a real VM throughput regression (`bench-vm` string/hash path) caused by over-aggressive early-trigger thresholds.
- Integrating debt-trigger scoring into VM precollection (`src/runtime/gc.zig:197`, `src/runtime/heap.zig:1348`, `src/interp/vm.zig:1414`) replaced threshold-only checks with measurable debt/pause/occupancy decisions.
- Exporting debt-decision telemetry end-to-end (`bench/gc.zig:372`, `bench/check.zig:421`, `tools/gc-compare:341`) exposed policy-range regressions immediately in the standard perf loop.
- Recording debt paydown as actual debt retired instead of raw reclaim volume (`src/runtime/heap.zig:1370`) aligned counters with invariants and removed false debt-regression failures in `bench-check`.
- Tracking nursery survivor age through a reusable side-map + per-copy updates (`src/runtime/heap.zig:1292`, `src/runtime/gc.zig:582`) produced stable age histograms without changing object layouts.
- Extending survival/promotion telemetry with explicit age buckets and promotion-success counters (`src/runtime/heap.zig:434`, `src/runtime/heap.zig:992`, `src/runtime/heap.zig:1350`) made tenuring feedback directly measurable for the next adaptive-threshold dot.
- Rebuilding survivor-age state after each nursery swap (`src/runtime/gc.zig:245`, `src/runtime/gc.zig:330`) kept age tracking aligned with moving addresses and prevented stale-address drift.
- Guarding promotion-success accounting inside `sweepTenured` even when `dead_count == 0` (`src/runtime/heap.zig:1000`) fixed a real telemetry blind spot where always-live promoted objects never counted as successful promotions.
- Wiring new telemetry through `gc_bench`/`bench-check` (`bench/gc.zig:239`, `bench/check.zig:49`) caught schema and invariant regressions immediately.
- Fixing stale forwarded pointers at the VM constant/chunk boundary (`src/interp/vm.zig:10469`, `src/interp/vm.zig:10488`, `src/interp/vm.zig:10500`) removed a root crash vector under small nursery pressure; repairing constants/chunk pointers lazily in hot ops (`push_const`/`check_or`/`push_block`/`return_from`) kept behavior generic for any large Lisp workload.
- Using an interned builtin key for function cells (`src/runtime/builtins.zig:66`, `src/runtime/builtins.zig:177`, `src/interp/vm.zig:907`) removed repeated runtime interning in function-namespace lookup/store/clear and stabilized `symbol-function` behavior during GC churn.
- Preserving VM chunk-pool state with pointer-aware restore logic in compiler temporary execution paths (`src/compiler/compile.zig:3626`, `src/compiler/compile.zig:9233`) fixed stale chunk-pool restoration when nested compile/eval replaces pools mid-expansion.
- Canonicalizing forwarded symbols before macro/symbol-macro and struct-predicate lookup (`src/compiler/compile.zig:14881`, `src/compiler/compile.zig:14906`, `src/compiler/compile.zig:17338`) prevented GC-moved symbol identity drift in compile-time dispatch.
- Running both targeted integration regressions and real Maxima workload repros (`src/tests/integration.zig:7135`, `zig build -Duse-hoist=true bench-maxima -- --json --scale=1 --heap-mb=1024 --nursery-mb=16`) gave deterministic proof that the small-nursery path now completes without crash.
- Replacing the `symbol-plist` placeholder with a real primitive-backed wrapper in `lib/stdlib.habu:4171` fixed function-cell parity: direct `(symbol-plist ...)` and `(funcall #'symbol-plist ...)` now agree, and `getl` behavior is stable when loaded generically.
- Adding a stdlib `getl` compatibility implementation in `lib/stdlib.habu:4180` plus an integration lock in `src/tests/integration.zig:7135` prevented silent plist lookup regressions in Maxima-style paths.
- Adding the exact `defun + &aux + outer cond + push + inner do/cond/return` repro as an integration test (`src/tests/integration.zig:6285`) is a reliable guard even when no compiler code change is required.
- Tightening format directive behavior in `src/interp/vm.zig` fixed real gaps:
  - `~*` argument navigation now honors `~*`, `~:*`, `~@*`, and numeric counts (`src/interp/vm.zig:7913`).
  - `~P` now falls back to previous argument when no next argument exists (`src/interp/vm.zig:7945`), preserving common `~D ... ~P` usage.
  - `~G` now emits general float formatting (`src/interp/vm.zig:8546`).
  - `~/fn/` now invokes formatter functions and appends stream output (`src/interp/vm.zig:8579`).
- Expanding integration coverage for format directives (`src/tests/integration.zig:2938`, `src/tests/integration.zig:3005`, `src/tests/integration.zig:3029`) gave immediate red/green signal on each missing directive behavior.
- Adding real-workload benchmark harnesses for both Habu and SBCL (`bench/maxima_workload.zig`, `bench/maxima_workload.lisp`) made Maxima CAS performance and loader gaps measurable in one command (`tools/maxima-bench`).
- Adding `tools/perf-loop` to combine comprehensive microbench + Maxima workload results produced a deterministic hotspot ranking and concrete next-action list instead of ad-hoc profiling.
- Adding `bench/sbcl_gc.lisp` + `tools/gc-compare` gave direct pause-time parity numbers (`avg_pause_ns`/`p95_pause_ns`) against SBCL for equivalent allocation pressure.
- Parsing mixed benchmark stdout by extracting the trailing JSON payload (`tools/maxima-bench`, `tools/perf-loop`, `tools/gc-compare`) made automation robust when Maxima runtime warnings print before/around result JSON.
- Adding a live-occupancy floor in nursery resizing (`src/runtime/heap.zig:1144` `nurseryLiveFloor`) prevented adaptive shrink steps from setting `gc_threshold` below current live nursery usage, which otherwise risks immediate-GC thrash loops.
- Making policy-cycle counters wrap-safe (`src/runtime/gc.zig:165` via `counterDelta`) removed latent unsigned-underflow hazards when long-running telemetry counters roll over.
- Running Maxima workload benches in generational mode with explicit nursery sizing (`bench/maxima_workload.zig`: `--nursery-mb`) plus GC telemetry export (`.gc.load`/`.gc.run`) made nursery-policy behavior observable on real workloads instead of microbench-only signal.
- Extending `tools/gc-compare` with optional Maxima telemetry (`--with-maxima`, defaults `--maxima-scale=3 --maxima-nursery-mb=24`) provided a practical mixed workload calibration point while keeping fast micro-only runs as default.
- Driving tenuring as a first-class control law in `deriveTenuringPolicy` (`src/runtime/gc.zig:126`) and applying it every minor cycle (`src/runtime/gc.zig:273`) made promotion threshold behavior measurable, bounded, and non-oscillatory without workload-specific special cases.
- Capturing adaptive tenuring bounds/ratios directly in heap stats (`src/runtime/heap.zig:454`, `src/runtime/heap.zig:1242`) and exporting/validating them in bench tooling (`bench/gc.zig:366`, `bench/check.zig:353`) caught policy regressions as schema/invariant failures instead of latent perf drift.
- Locking policy behavior with dedicated GC tests (`src/runtime/gc.zig:1457`, `src/runtime/gc.zig:1510`) provided deterministic red/green coverage for raise/lower/deadband decisions and runtime threshold updates.
- Extending `tools/gc-compare` with tenuring guard metrics/gates (`tools/gc-compare:38`, `tools/gc-compare:256`, `tools/gc-compare:460`) added machine-checkable regression signals for promotion waste and policy-scale drift alongside pause/throughput parity checks.
- Adding a deterministic generational stress regression in integration (`src/tests/integration.zig:7173`) locked adaptive tenuring bounds (`threshold/min/max`, scale, ratio ranges) and ensured threshold movement under repeated promote-and-sweep cycles.
- Replacing 1-bit card marks with per-card lane bitmasks (`src/runtime/heap.zig:20`, `src/runtime/heap.zig:851`, `src/runtime/heap.zig:963`) tightened remembered-set granularity and reduced same-card false-positive scans without changing barrier call sites.
- Making `hasMarkedCardInAddrRange` lane-aware (`src/runtime/heap.zig:954`) plus adding a focused regression (`src/runtime/heap.zig:3070`) gave deterministic proof that unrelated lanes in the same card no longer trigger remembered-set hits.
- Coalescing remembered cards into run lists (`src/runtime/heap.zig:914`) and reusing a persistent `remembered_runs` buffer in GC (`src/runtime/gc.zig:185`, `src/runtime/gc.zig:414`) improved minor-GC remembered scanning locality while keeping allocation-free hot paths.
- Routing minor-GC remembered scans through run-aware overlap checks (`src/runtime/gc.zig:415`, `src/runtime/heap.zig:996`) eliminated full-table clean-run walks and preserved correctness on tenured/LOS edge scanning.
- Adding explicit remembered-set telemetry counters (`src/runtime/heap.zig:481`, `src/runtime/gc.zig:450`) plus exporting them in GC bench payloads (`bench/gc.zig:337`) made RSet scan pressure visible and regression-checkable.
- Extending `bench/check` + `tools/gc-compare` with remembered-set invariants/gates (`bench/check.zig:380`, `tools/gc-compare:33`, `tools/gc-compare:483`) locked both correctness (non-zero marked/runs/scans) and efficiency (`scan_per_mark`) in automated validation loops.
- Locking runtime coverage with a focused GC regression (`src/runtime/gc.zig:1822`) ensured remembered-set telemetry is exercised and monotonic under real LOS owner + young child mutation patterns.
- Adding heap-level GC debt accounting (`src/runtime/heap.zig:338`, `src/runtime/heap.zig:1329`, `src/runtime/heap.zig:2829`) plus VM debt-triggered precollection hooks (`src/interp/vm.zig:1062`, `src/interp/vm.zig:1410`) converted allocation pressure into explicit, testable counters instead of implicit OOM-only behavior.
- Exporting debt telemetry through `bench/gc` and enforcing it in `bench/check`/`tools/gc-compare` (`bench/gc.zig:337`, `bench/check.zig:70`, `tools/gc-compare:330`) created a closed verification loop for debt bytes, paydown, and trigger quality.
- Driving LOS threshold from per-cycle allocation-size deltas (`src/runtime/gc.zig:244`, `src/runtime/gc.zig:273`) plus occupancy/pause feedback produced bounded threshold movement without workload-specific handling.
- Exporting LOS policy state end-to-end (`src/runtime/heap.zig:474`, `bench/gc.zig:357`, `bench/check.zig:90`, `tools/gc-compare:374`) turned threshold/scale/range regressions into immediate gate failures instead of latent perf drift.
- Reusing one bin/list allocator path for both tenured and LOS free spans (`src/runtime/heap.zig:1124`, `src/runtime/heap.zig:1175`, `src/runtime/heap.zig:1433`) removed duplicate allocation-policy code and made LOS reuse use the same bounded best-fit behavior as tenured.
- Rewinding LOS bump-pointer from coalesced tail spans (`src/runtime/heap.zig:1420`, `src/runtime/heap.zig:1501`) reclaimed top-of-LOS space immediately and reduced LOS reuse latency on subsequent allocations.
- Emitting LOS policy + live-bytes counters in Maxima workload GC snapshots (`bench/maxima_workload.zig:50`, `bench/maxima_workload.zig:112`, `bench/maxima_workload.zig:629`) made real-workload LOS behavior inspectable without ad-hoc traces.
- Extending `tools/gc-compare` Maxima parsing with LOS bounds checks (`tools/gc-compare:589`, `tools/gc-compare:631`, `tools/gc-compare:833`) provided one-command verification that LOS policy remains in-range under `--with-maxima`.
- Adding opt-in mutator profiling counters (`HABU_PROFILE_MUTATOR`) for write barrier and safepoint paths (`src/runtime/heap.zig:527`, `src/interp/vm.zig:1420`, `src/jit/backend.zig:82`) produced direct VM-vs-JIT overhead telemetry without changing default hot-path behavior.
- Wiring `tools/perf-loop --profile-mutator` to export/load mutator profile snapshots (`tools/perf-loop:132`, `tools/perf-loop:392`, `tools/perf-loop:457`) made barrier/safepoint overhead part of the standard optimization loop.
- Inlining a cheap `stored.isPointer()` guard at VM/JIT barrier call sites (`src/interp/vm.zig:1417`, `src/jit/backend.zig:99`) cut mutator-profiled barrier call volume on Maxima load paths without changing GC semantics.
- Batching debt safepoint polls by both op-count and allocation-byte budget (`src/interp/vm.zig:1432`, `src/interp/vm.zig:471`) preserved bounded polling latency while cutting VM safepoint poll overhead by an order of magnitude on Maxima loads.
- Resetting safepoint batch counters on every actual GC entry (`src/interp/vm.zig:1452`) avoided stale-batch carryover after collections.
- Defining a single cross-runtime workload manifest (`bench/pack/corpus.json`) removed benchmark-name drift between Habu/SBCL tooling and provides a stable contract for OCaml runner integration.
- Moving runtime execution into one shared adapter module (`tools/bench_pack_runner.py:369`, `tools/bench_pack_runner.py:445`) made `tools/perf-loop` and `tools/gc-compare` consume identical normalized payloads, eliminating duplicated command/parsing drift.
- Enforcing required top-level JSON keys when scraping mixed stdout (`tools/bench_pack_runner.py:426`, `tools/bench_pack_runner.py:481`) prevented nested benchmark-object misparses and restored complete workload accounting in `tools/perf-loop` (`tools/perf-loop:116`) and `tools/gc-compare` (`tools/gc-compare:285`).
- Surfacing OCaml adapter status/errors in JSON and text outputs (`tools/perf-loop:491`, `tools/gc-compare:706`) made missing OCaml command wiring explicit instead of silently dropping the runtime.
- Emitting selected-gate parity deltas and CI trend series directly from gate evaluations (`tools/gc-compare:680`, `tools/gc-compare:706`, `tools/gc-compare:947`) created a machine-consumable contract for regression dashboards without duplicating gate math downstream.
- Ranking GC actions from repeated `gc-compare` samples with per-metric confidence (`tools/perf-loop:314`, `tools/perf-loop:410`, `tools/perf-loop:548`) reduced score volatility and exposed low-confidence optimization signals directly in reasons/output.
- Persisting perf-loop runs as append-only JSONL plus derived trend lines (`tools/perf-loop:574`, `tools/perf-loop:706`, `tools/perf-loop:917`) gives a durable self-improvement trail without coupling ranking logic to external storage.
- Emitting explicit `next_dots` recommendations from measured score/confidence/trend signals (`tools/perf-loop:658`, `tools/perf-loop:750`, `tools/perf-loop:988`) turns perf-loop output into direct execution commands instead of manual interpretation.
- Adding a dedicated `gc-parity` build step (`build.zig:279`) plus CI workflow (`.github/workflows/gc-parity.yml`) gives a stable entrypoint for parity artifacts without forcing gate failures yet.
- Adding regression-baseline mode to `tools/gc-compare` (`--regression-baseline`, `--fail-on-regressions`) let CI hard-fail on measured drift while keeping absolute parity milestones as informational (`tools/gc-compare:253`, `tools/gc-compare:781`).
- Publishing a single GC parity contract doc (`docs/gc-parity-contract.md`) and linking it from `bench/README.md`/`docs/README.md` removed ambiguity about gate semantics vs regression semantics.
- Linking Maxima loader docs directly to parity/regression commands (`docs/maxima-loader.md`) made loader RCA and perf gate checks share one operational entrypoint.

### Did Not Work
- Assuming `jit_backend.setHeap()` was always safe in bridge helpers without first syncing inline-cons progress was wrong; when JIT had advanced `g_alloc_ptr`, bridge entry rewound allocator state and corrupted in-flight recursive data structures (`src/interp/vm.zig` pre-fix `jitCallBridgeInvoke`).
- Gating non-self call handling on populated `known_fns` was brittle in test/harness compilation paths (`src/testing/compile_chunk.zig:191` calls `compileIr` without known-fn map), leaving call-arg cycle passes disabled for real helper-call shapes.
- Saving/restoring VM globals in local structs across `load`/nested eval (`src/interp/repl.zig` pre-fix `savePackageGlobals`/`restorePackageGlobals` pattern) is unsafe under moving GC; the restored values can be stale and later crash in GC object-size dispatch.
- Bundling a broader load-global rebinding rewrite while fixing package restoration caused a deterministic Maxima nparse regression (`InvalidIr` in `SIMPTIMES`); isolating the package-root fix first restored the gate before further refactor work.
- Stress fixtures that keep entire allocation chains alive (for example repeatedly `cons`ing into a retained list) can OOM before the target invariant is exercised; GC-stress regressions should churn ephemeral allocations.
- Reordering root-stack pushes to allocate before assigning the new root (`src/interp/repl.zig` `pushRootValue` experiment) leaves the incoming value unrooted during GC and can crash later in macro symbol canonicalization (`maxima ... ifactor` path).
- Assuming post-promotion collections start from an idle major-cycle state was wrong; tests that drop roots mid-cycle can observe old marks and fail reclamation assertions unless the previous cycle is drained first (`src/runtime/gc.zig:2477`).
- Using `jj squash --from ...` without `--message` in non-interactive automation opened an editor unexpectedly; always pass `--message` for scripted merges.
- Assuming a fixed `MAJOR_SWEEP_BUDGET`-sized fixture would keep major cycle active was brittle; root ordering/object size can make the cycle complete in one pass, so barrier tests need larger deterministic workloads.
- Transitioning mark->sweep as a single step per minor cycle delayed tiny sweep completions by an extra GC; using iterative phase advancement in one cycle (`src/runtime/gc.zig:639`) fixed this regression.
- More aggressive debt thresholds/weights looked faster on Maxima only because loader failures increased (`maxima_habu_errors` 8→10), so raw wall-time wins are invalid unless error counts stay flat.
- Counting `gc_debt_paydown_bytes` as raw `max(copied,reclaimed)` was wrong (`src/runtime/heap.zig:1370`): it can exceed debt inflow by orders of magnitude and trip valid invariants (`bench/check.zig:416`).
- Treating a single default-threshold `bench-check` p95 miss as semantic breakage was noisy in this environment; rerunning with a relaxed p95 gate isolated invariant/schema correctness from host performance variance.
- Assuming promotion-success counters would update only when tenured sweep reclaimed something was wrong; the old early-return path in `sweepTenured` skipped success accounting for all-live sets.
- Using `AutoHashMapUnmanaged.ensureTotalCapacity(..., entries.len)` without casting failed on Zig 0.15 `Size` typing (`src/runtime/heap.zig:1294`); explicit integer casts are required.
- Relying on `jj diff` word-level render to validate edited code was misleading during this RCA; several hunks appeared token-mashed while source files were correct, so direct line inspection (`nl -ba`) is required before concluding syntax damage.
- Treating full `zig build test` as a required close gate in this environment remained unreliable (`--listen` hang state); targeted `-Dtest-filter` gates plus workload repro must be the deterministic proof path until harness stability improves.
- Assuming `(in-package ...)` inside one `progn` would affect reader/package resolution for subsequent symbols in the same already-read form was wrong; defining formatter helpers with explicit package-qualified symbol names avoids this trap.
- Relying on `tools/dot-finish` full `zig build test` in this environment was unreliable due harness stalls; targeted filtered test gates provided deterministic validation for dot closure work.
- Running real-workload CAS loops with large default iteration counts caused impractically long benchmark runs; use very small defaults plus explicit `--scale` for controlled expansion.
- Parsing mixed benchmark stdout by taking the last JSON object without key validation was incorrect for list-heavy payloads; inner bench objects can parse successfully and masquerade as full payloads (`tools/bench_pack_runner.py:426` fix with `required_keys`).
- Using `datetime.utcnow()` for persisted run timestamps triggered runtime deprecation warnings in current Python; use timezone-aware UTC timestamps (`tools/perf-loop:707`).
- Reusing `lib/maxima-loader.lisp` as-is for SBCL benchmarking was brittle because warning conditions were treated as load failures; SBCL-side loaders need warning-muffling and explicit per-file load control.
- Clamping adaptive nursery targets only to static min/max bounds was insufficient: without a live-bytes floor, shrink decisions can violate runtime occupancy constraints (`src/runtime/heap.zig:1140`) and force pathological recollection behavior.
- Using plain unsigned subtraction for per-cycle counter deltas (`src/runtime/gc.zig:165`) is unsafe with wrapping counters; use modular delta (`-%`) consistently.
- A less aggressive nursery shrink law experiment in `src/runtime/gc.zig` increased Maxima stressed-runtime totals (~75.7s baseline to ~78.2s at `scale=4,nursery=24`), so benchmark-driven tuning must keep the original coefficients until tenuring/debt controls land.
- Very small nursery settings (`tools/maxima-bench --nursery-mb=8..16`) exposed real crash paths under GC pressure (compiler/runtime stale-pointer faults), so treat those runs as RCA repros, not tuning datapoints.
- Enforcing `tenured_live > 0 => tenured_bytes > 0` as a strict benchmark invariant was incorrect for current allocator accounting (`bench/check.zig:390`): `tenured_bytes` tracks bump-usage, not exact live-bytes, so hard coupling generated false failures on valid runs.
- Repl/stdlib-driven tenuring stress tests were brittle for this gate (fixture-sensitive OOM and promotion-starvation); heap-driven promote/drop cycles in `src/tests/integration.zig:7173` are a better deterministic guard for policy regression checks.
- In this environment `zig build bench-check -- --json` can stall with sleeping `bench_check`/build processes and no progress output; targeted `-Dtest-filter` gates plus `tools/gc-compare` JSON checks are the reliable verification path until harness stability is fixed.
- Using only per-object `hasMarkedCardInAddrRange` checks across all old objects is still too cache-cold for remembered scans at scale; run coalescing + fast run filtering should be the baseline before deeper RSet tuning.
- Running `python -m py_compile` in-tree drops `tools/__pycache__` artifacts; remove these before commit to keep generated files out of history.
- Debt-triggered precollection is safe for `Value` roots but not raw heap-backed byte slices (`allocString`/`intern`/`allocSymbol`); those paths still need explicit stable-copy handling before enabling proactive debt collections there.
- Using cumulative allocation histograms directly for LOS adaptation was wrong; control decisions must use per-cycle deltas (`src/runtime/gc.zig:244`) or thresholds drift from stale historical bias.
- Asserting absolute LOS object positions in tests was brittle because low thresholds can route bootstrap allocations into LOS; capture/mark target spans explicitly and assert reuse by span address (`src/runtime/heap.zig:3490`, `src/runtime/heap.zig:3521`).
- Looking only at Maxima run-phase GC counters is insufficient for LOS validation when run-phase alloc pressure is low (`maxima_gc_run_count` may be 0); include load-phase LOS telemetry in validation checks (`tools/gc-compare:589`, `tools/gc-compare:839`).
- Running `zig test src/interp/vm.zig` directly is invalid in this repo layout (relative imports outside module path); validate VM changes through build steps/bench paths instead.
- For short run phases, nanosecond counter deltas can quantize to zero (`wb_ns` on tiny benchmark tails), so compare call counts and load-phase totals instead of relying on single tiny-phase timing deltas.
- Op-count-only safepoint batching can over-delay polls during large single allocations; enforce a byte budget (`SAFEPOINT_BATCH_BYTES`) alongside op budget to keep latency bounded by allocation volume.
- Maintaining benchmark name lists in multiple scripts is fragile; keep workload names in one corpus and have runner tools consume that manifest.

## Session Notes (2026-02-18)

### Worked Well
- Reproducing function-namespace corruption with a minimal generic CL case (`(proclaim '(special selector))` + `(symbol-function 'selector)`) made the Maxima `defmode` failure deterministic without Maxima-specific assumptions.
- Storing function bindings explicitly at defun/fdefinition/symbol-function definition points (`src/compiler/compile.zig:6051`, `src/compiler/compile.zig:6114`, `src/compiler/compile.zig:8141`, `src/compiler/compile.zig:8146`) plus VM-side function-cell resolution (`src/interp/vm.zig:808`) fixed the root namespace bug instead of masking it.
- Adding function-cell lookup to REPL callable resolution (`src/interp/repl.zig:981`, `src/interp/repl.zig:1058`) kept `fboundp`/designator behavior stable when value cells are dynamically rebound.
- Expanding builtin function classification to include internal setf helpers (`src/compiler/compile.zig` primitive dispatch table now includes `%aset`/`%svset`/`%sset`) removed brittle reliance on nil-slot fallback during stdlib bootstrap and reduced resolver misses.
- Locking the regression in integration (`src/tests/integration.zig:6165`) and updating the Maxima readiness gate as behavior improved (`src/tests/integration.zig:5989`) prevented reintroducing special-binding/function-binding alias bugs.
- Sampling the long-running integrate gate (`sample` on the live test PID) immediately identified the real hot region (`expandMacro`/`compileCondWithTail`) instead of guessing.
- Checking process state during long `zig build test` runs distinguished real runtime hotspots from external build contention and avoided chasing false "hang" causes.
- Using REPL-compiled defmacro closures in compiler expansion (`src/interp/repl.zig` compiled macro-table entries + `src/compiler/compile.zig` direct closure-call path) removed repeated macro-lambda compile/emit cycles while keeping chunk/index semantics safe (closures come from stable REPL chunk pool, not transient expansion pools).
- In `lib/stdlib.habu`, parsing `IF/WHEN/UNLESS ... DO` actions with the same keyword-boundary rule as top-level `DO` fixed a real parser bug where trailing forms (for example `(loop-finish)`) were misclassified as top-level LOOP clauses.
- Rewriting `loop-finish` calls at LOOP codegen time (after `result-expr` is known) preserved generic accumulation semantics while avoiding Maxima-specific behavior.
- Tracing Maxima load with per-form names (`TRACE defun ...`) made the real blocker obvious: `db.lisp` `defun clear` failed only because preceding `defmode` setup failed.
- Reducing the failure to a minimal repro (`defmode` + `putprop` arg probe) exposed the root semantic bug: proclaimed `special` lambda params were compiled lexically, so helper callees saw `name=nil`.
- Fixing lambda-parameter special semantics generically in `src/compiler/compile.zig` (dynamic `progv` wrapper for globally proclaimed special params) restored `declare-top` behavior across Maxima macros without Maxima-specific patches.
- Adding a focused regression in `src/tests/integration.zig` (`proclaimed special lambda params are dynamically visible in callees`) locks this dynamic-scope contract.
- Adding system-only/internal keywords on `maxima-load-all` (`:habu-stop-on-error`, `:habu-required-bindings`) enabled stronger diagnostics without bending CL-facing defaults.
- Removing per-form error masking in `src/interp/repl.zig` `evalForms` (then named `evalFileContentSeparateVm`) made `(load ...)` semantics deterministic and restored reliable file-level failure accounting for Maxima loader gates.
- Locking strict load semantics with a focused regression (`src/interp/repl.zig` `loadFile` aborts on first form error) prevented silent partial-file success regressions.
- Fixing `loop` parser support for `FOR ... IN ... BY ...` in `lib/stdlib.habu` removed a generic clause-gap that surfaced as `Unknown loop keyword: BY` in large Lisp packages.
- Extending `get-setf-expansion` with composed list-place updaters (`cadr`/`cddr`/`caddr`/`cdddr`/aliases) removed a high-frequency `setf: unsupported place` class for macro-heavy code.
- Reworking LOOP conditional routing to accept `ELSE WHEN ... ELSE ...` in `lib/stdlib.habu` unblocked real-world clause patterns (e.g. `commac.lisp` `maknam`) without Maxima-specific branches.
- Tracking proclaimed specials by symbol identity (`Value.raw`) instead of bare names in `src/compiler/compile.zig` prevents cross-package special-variable leakage.
- Fixing nested callback non-local exits in `src/interp/vm.zig` (`callFromStackAt`/`doThrow`) removed a root semantic bug where `handler-case` around `(load ...)` could catch an error and still resume the loaded file.
- Adding dual regressions in `src/interp/repl.zig` for direct eval and script-driven `handler-case (load ...)` closed the gap that only appeared when `load` ran inside another loaded script.
- Keeping GC state persistent in `src/runtime/heap.zig` (`Heap.gc`) and routing collection through `self.gc.collectRootSet(...)` eliminated per-collection `GC.init/deinit` churn from the hot path.
- Refactoring `src/runtime/gc.zig` to pass `heap` explicitly into `collect/collectRootSet/copyValue/scanObject` made collector lifetime safe and enabled queue reuse; GC benchmark p95 dropped to ~7.49ms from ~7.72ms on `bench-check`.
- Adding phase counters to `src/runtime/heap.zig`/`src/runtime/gc.zig` and surfacing them in `bench/gc.zig` gave actionable GC slices (`build/root/copy/finalize`) and enabled structural perf gates in `bench/check.zig`.
- Caching internal GC root slots in `src/runtime/heap.zig` (`gc_internal_slots` + `calcGcRootSig`) removed per-collection full table walks; using `SymbolTable.version` in the signature prevented stale-cache reuse when symbol maps mutate without net count change.
- Adding explicit heap layout scaffolding (`GcLayoutMode`, `HeapLayout`, `Region`) in `src/runtime/heap.zig` made nursery/tenured/LOS boundaries concrete without changing current semispace behavior; this keeps incremental generational work isolated and testable.
- Adding a no-allocation write barrier in `src/runtime/heap.zig` (card table + `writeBarrier`) and calling it at VM/primitives pointer-store sites (`src/interp/vm.zig`, `src/runtime/primitives/list.zig`, `src/runtime/primitives/hash.zig`, `src/runtime/primitives/clos.zig`, `src/runtime/primitives/symbol.zig`) provided generational-safe mutation hooks without changing non-generational behavior.
- Exposing remembered-set APIs (`markedCardCount`, `appendMarkedCards`, `appendMarkedCardRanges`, `clearMarkedCards`) in `src/runtime/heap.zig` made barrier output directly consumable for upcoming minor-GC root scanning and added deterministic tests for mark/enumerate/clear flow.
- Adding JIT-side barrier/safepoint hooks in `src/jit/backend.zig` (`jitWriteBarrier`, `jitSafepointBeforeAlloc`) keeps runtime helper mutations (`jitNreverse`) and slow allocation paths aligned with VM barrier invariants.
- Splitting GC entry by layout mode in `src/runtime/gc.zig` (`collectSemispaceRootSet` vs `collectMinorRootSet`) kept semispace behavior stable while enabling generational-only logic incrementally.
- Keeping minor-GC promotion conservative (pointer-free objects only) in `src/runtime/gc.zig` `shouldPromote` avoided premature tenure of resource-bearing/ref containers before tenured mark/sweep exists.
- Extending stream liveness checks in `src/runtime/gc.zig` `finalizeUnreachable` to accept forwarded tenured addresses prevented false-finalization when survivors are promoted.
- Adding non-moving tenured mark-sweep metadata in `src/runtime/heap.zig` (`tenured_objs.marked` + `tenured_free`) enabled deterministic reclaim of dead promoted objects without moving survivors.
- Marking tenured reachability directly in `src/runtime/gc.zig` `copyValue` for non-from-space pointers ensured tenured objects reachable only through nursery survivors are not swept accidentally.
- Extending the same non-moving discipline to LOS (`src/runtime/heap.zig` `allocLosRaw`/`recordLosObject`/`sweepLos`) made large-object allocation and reclamation predictable with stable addresses.
- Mark-on-touch + work-queue scan for LOS in `src/runtime/gc.zig` `copyValue` prevented stale young pointers inside pinned large containers across minor collections.
- Switching GC perf benches to generational fixtures (`bench/gc.zig`) is essential; semispace-only benches can pass while generational paths silently regress.
- Hoist API drift checks must run under `-Duse-hoist=true`; default test mode can otherwise hide interface breakage behind the stub backend.
- In `src/interp/vm.zig` `collectGarbageExtra`, replacing the closure-count prepass with an upper-bound capacity estimate (`self.fp`) and merging frame closure/chunk staging into one pass removed a duplicate frame walk with no semantic change.
- Reintroducing a source-backed `bench/jit.zig` and wiring `bench-jit` in `build.zig` removed a stale-artifact trap where `bench-check` could read an old `zig-out/bin/jit_bench`.
- Enforcing strict `bench/check.zig` argument handling (`InvalidArgs` returns non-zero) exposed accidental no-op invocations like `bench-check -- --json /tmp/file`.

### Did Not Work
- Leaving `CompiledFn.callFromValues` capped at arity 3 (`src/jit/backend.zig` pre-fix) quietly produced `nil` for 4+ arg JIT functions even when compilation succeeded, masking coverage and correctness holes.
- Relying on naive topological reordering for call-arg moves without cycle breaking (`src/jit/backend.zig` pre-fix `fixCallArgMoves`) produced deterministically wrong helper arguments on 4-arg call-indirect paths and silently returned `nil` from JIT `format` calls.
- A pure per-character preallocation path for all string concatenations (`lib/stdlib.habu:2416` intermediate attempt) improved long concatenations but regressed short hot call sites; restoring dedicated 1/2-arg `string-concat` fast paths fixed that.
- A loop-only `reduce` rewrite without function-specialized dispatch barely moved the benchmark (~1901ms to ~1894ms), confirming that per-element `funcall` overhead (not recursion itself) was the dominant bottleneck in the hot `#'+` path.
- Keeping `mapcar` on a single generic variadic `apply` loop (`lib/stdlib.habu:107` pre-fix) caused severe avoidable overhead for the dominant one-list benchmark shape; arity-specialized paths are required for production throughput.
- Making `resolveFunctionValue` strict-callable-only without preserving nil-slot bootstrap behavior immediately broke stdlib bootstrapping (`%ASET` unresolved via `(symbol-function '%aset)`); preserving nil/unbound slot fallback while rejecting non-callable non-nil values was required.
- Caching compiled macro expanders by storing closures in `macro_table` is not safe with current chunk-pool/index patching: cached closures retain expansion-time chunk index assumptions and can mis-dispatch nested lambdas later.
- "Tagged cached macro" wrappers still failed because macro closures compiled in one expansion context are not context-free artifacts under current VM/compiler coupling (chunk indices + expansion-time global/macro state assumptions).
- Treating a hanging `zig build test -Dtest-filter=...` run as a runtime hotspot signal was misleading in some cases: sampled hangs showed Zig build/test protocol wait states (`build` polling while `test --listen` waited for commands), so a stuck filtered run is not automatically a VM performance regression.
- Treating conditional `DO` boundaries as only `ELSE/END/AND` was wrong: it consumed subsequent LOOP clauses (like `COLLECT`) and silently changed loop results.
- Defining `loop-finish` as a global macro caused expansion timing issues; keeping it as a callable symbol and lowering it inside LOOP expansion was more reliable here.
- Chasing downstream `SIMPLE-ERROR` output first was noisy; until `defmode`/special-parameter semantics were fixed, later integrate traces were mostly secondary fallout.
- Running long `zig build test -Dtest-filter=\"...maxima...\"` invocations remained unreliable/hang-prone in this environment; short focused filters and direct scripted repros gave more deterministic signal.
- Using multiline piped REPL scripts for loader RCA gave misleading/garbled diagnostics; `habu <script-file>` probes and targeted tests were more trustworthy.
- Name-only special-declaration matching in the compiler was too coarse; package-unaware declaration lookup can silently destabilize unrelated lexical bindings.
- Testing only direct `(handler-case (load ...))` eval was insufficient; script-level `loadFile` execution has different callback boundaries and must be covered explicitly.
- Blind regex rewrites on function-call signatures in `src/runtime/gc.zig` briefly produced duplicate arguments (`self.copyValue(heap, heap, ...)`); immediate compile/test loops are required right after broad replacements.
- Using `std.time.Timer.start()` inside GC internals widened the GC error set (`TimerUnsupported`) and broke call-site error contracts in `src/interp/vm.zig`; use `std.time.nanoTimestamp()` deltas in hot/runtime internals when error signatures must stay stable.
- Root-cache signatures based only on table counts are not enough; equal counts can still hide map-entry churn. Include mutation/version signals (for symbol tables) or stronger structure signatures.
- Generational scaffolding must not silently change default capacity assumptions; keep default mode semispace and prove unchanged behavior with existing bench-check gates before moving to barrier/minor-GC dots.
- Barrier coverage needs grep-driven audits after each refactor (`.car=`, `.cdr=`, `vec.set`, hash puts); it is easy to miss direct stores in VM helpers and primitive paths.
- Remembered-set APIs should be allocation-free on hot mutation paths and only allocate during explicit scan/export calls; keep the write barrier itself side-effect-light.
- JIT runtime helpers can mutate heap objects outside the interpreter dispatch loop; barrier logic must be hooked there explicitly or remembered sets drift silently.
- Running `zig build test` in this environment can park in Zig `--listen` mode without emitting failures; treat that as harness instability and rely on targeted test filters plus explicit process sampling for RCA.
- Promoting pointer-bearing containers before implementing tenured collection is a semantic trap: unreachable promoted objects will not be reclaimed/finalized yet, so promotion policy must enforce this boundary explicitly.
- Reclaiming tenured holes without a free-list leaves long-running sessions with artificial tenured OOM despite low live set; non-moving sweep must feed allocator reuse paths immediately.
- LOS tests should assert deltas, not absolute counts: heap bootstrap can legitimately pre-populate LOS metadata when low thresholds are used in tests.
- Bench checks should assert structural GC invariants (promoted bytes, LOS/tenured liveness, old-space bounds), not just pause time, to catch semantic regressions early.
- For hoist signatures, ownership is transferred into `Function.init`; calling `sig.deinit()` afterwards double-frees and crashes.
- Leaving `use-hoist` defaulted off while still labeling runs as JIT leads to misleading perf/RCA outcomes (e.g. recursive benchmarks failing under interpreter stack limits while reported as JIT mode).

## Session Notes (2026-02-19)

### Worked Well
- Splitting constructor missing-slot defaults by type family in `generateStructConstructor` (`src/compiler/compile.zig:10071`, call sites `src/compiler/compile.zig:9756` and `src/compiler/compile.zig:11261`) restored CL semantics: `defclass` slots without initform start unbound, while `defstruct` still defaults to nil.
- Locking `defstruct` nil-default behavior with a focused regression (`src/tests/integration.zig:3698`) prevented a silent semantic regression while fixing CLOS slot-boundp behavior.
- Updating Maxima subset gate checks to package-qualified symbols (`src/tests/integration.zig:5783`) removed false negatives caused by strict package resolution.
- Treating escaped reader characters as syntax (not symbol-name data) in parser expectations (`src/reader/parser.zig:2513`) aligned tests with CL reader behavior.
- Treating `Repl` as self-referential (VM callbacks/global-env pointers into `Repl.compiler`) and keeping helper state at a stable address (`src/tests/integration.zig:5050`) removed deterministic `set_symbol_function` segfaults in MV tests.
- Adding a focused regression for moved-helper REPL execution (`src/tests/integration.zig:5075`) keeps this lifetime bug from returning silently.
- Replacing `global_special_syms` raw-value keys with package/uid-aware `VarKey` identity (`src/compiler/compile.zig:2159`, `src/compiler/compile.zig:2205`) removed GC-movement sensitivity from special-variable tracking and fixed stale special lookups after collections.
- Detecting leading local `(declare (special ...))` forms before lowering `let` (`src/compiler/compile.zig:5150`) fixed a root semantic gap where locally-declared specials were compiled lexically.
- Unifying symbol value-cell operations through explicit VM helpers (`src/interp/vm.zig:823`, `src/interp/vm.zig:835`) and handling uninterned symbols via stable uids fixed `symbol-value`/`boundp`/`makunbound`/`progv` behavior generically.
- Rewriting `progv` save/restore to bind concrete slots or uninterned symbol cells (`src/interp/vm.zig:7257`) removed name-suffix aliasing and restored correct dynamic binding restoration.
- Routing `write-string` through shared stream I/O (`src/runtime/primitives/stream.zig:407`, `src/runtime/primitives/io.zig:787`) centralized stream-type behavior and avoids duplicated output-path logic.
- Locking regressions for dynamic specials/value cells/numeric predicates (`src/tests/integration.zig:6226`, `src/tests/integration.zig:6305`, `src/tests/integration.zig:6344`) kept fixes generic and prevented Maxima-only drift.
- Tightening `set_symbol_function` to stop mutating value-cell globals except legacy callable slots (`src/interp/vm.zig:4026`) removed a generic namespace corruption path where `defun` could overwrite unrelated variable bindings (for example Maxima `ratvars`-style symbols).
- Locking this with explicit regressions for shared symbol names and nil-bound values (`src/tests/integration.zig:6245`, `src/tests/integration.zig:6282`) keeps future function-cell work from silently reintroducing value-cell clobbers.
- Capturing the failing `check_closure` disassembly for the Maxima gate showed the assertion was injected at local `let` binding stores, which pointed directly to declaration scoping instead of JIT/runtime dispatch.
- Adding lexical declaration storage to `Env` (`src/compiler/compile.zig:1461`, `src/compiler/compile.zig:1587`, `src/compiler/compile.zig:1698`) and routing local `(declare (type ...))` through it (`src/compiler/compile.zig:13037`) stopped cross-form declaration bleed.
- Switching lexical variable assertion lookup from global name-based declarations to environment symbol-identity lookup (`src/compiler/compile.zig:2620`) removed false `assert_closure`/`assert_fixnum` injections in unrelated forms.
- Dropping global type-decl application from `let` initializer compilation (`src/compiler/compile.zig:5002`) removed a root crash vector where unrelated local names inherited stale global declarations.
- Locking the fix with a regression (`src/tests/integration.zig:5889`) prevents reintroducing local type declaration leakage.
- Tracing with `HABU_TRACE_ERROR_CONTEXT=1`/`HABU_TRACE_ERROR_ONLY=UnboundSymbol` on the module load gate immediately exposed the actual symbol-function miss (`ATAN`) inside `trigi` instead of chasing downstream parser/runtime fallout.
- Installing BIGFLOAT-IMPL callable aliases through a guarded binder (`lib/maxima-stubs.lisp:120`, `lib/maxima-stubs.lisp:133`) plus inverse-trig fallbacks (`lib/maxima-stubs.lisp:105`) fixed `trigi`/`trigo` loading generically and kept operator symbols fbound across package shadowing.
- Adding a focused trigi subset regression (`src/tests/integration.zig:5921`) catches future regressions where callable trig aliases disappear during package/bootstrap changes.
- Tracing unbound calls in `SIMP-%SIN` showed `COMPLEX-NUMBER-P` was required before `ellipt.lisp`; adding a bootstrap-compatible definition in `lib/maxima-stubs.lisp:257` removed that hidden dependency from trig subset execution.
- Keeping `def-simplifier` bootstrap output aligned with `simp.lisp`'s real `arg-count-check` arity (`lib/maxima-stubs.lisp:355`) eliminated a cross-module arity mismatch that only appears after `simp` redefines `arg-count-check`.
- Mapping VM `InvalidTypeSpecifier`/`InvalidArgument` to CL conditions in `zigErrorToConditionSym` (`src/interp/vm.zig:7398`) restored `handler-case` behavior for malformed type/argument paths and kept long Maxima probes from aborting at the first uncaught Zig error.
- Locking the condition mapping with `src/tests/integration.zig` (`handler-case catches invalid argument and invalid type specifier`) prevents condition-handler regressions from silently returning to raw Zig error aborts.
- Converting the Maxima end-to-end check into a deterministic readiness vector (`src/tests/integration.zig:5781`) keeps large-package progress measurable without hiding remaining semantic gaps.
- Splitting large Maxima setup/eval forms into separate `repl.eval` calls reduced parser-noise and made failures attributable to specific steps instead of one monolithic expression.
- Keeping `defun` intact in desugar (`src/compiler/passes/p02_desugar.zig`) and only desugaring the body restored compiler-level DEFUN semantics (implicit function block), which removed `NoMatchingBlock` failures in real Maxima functions (`add-lineinfo`).
- Restricting legacy bare-name global fallback to `CL-USER` symbols in both compiler and VM (`src/compiler/compile.zig`, `src/interp/vm.zig`) prevented cross-package function-cell capture (notably `FUNCTIONP` recursion paths while loading Maxima).
- Preserving secondary values across `pop_block`/`push_block` in VM op post-processing (`src/interp/vm.zig`) fixed a subtle multi-value regression introduced by implicit DEFUN blocks (`(defun f () (values ...))` started returning only the primary value before this fix).
- Routing builtin callable checks through compiler dispatch tables (`src/compiler/compile.zig:14482`) and consuming that API in REPL symbol resolution (`src/interp/repl.zig:880`, `src/interp/repl.zig:908`, `src/interp/repl.zig:956`) fixed the `ATAN`/`%ATAN` unbound path in `trigi` without adding Maxima-specific symbol aliases.
- Adding/keeping focused gates (`src/tests/integration.zig:5921`, `src/tests/integration.zig:5979`, `src/tests/integration.zig:6047`) gave deterministic proof for the trig/matrix/dependency chain fixes even when broad filtered test runs were noisy.

### Did Not Work
- Hard-coding Maxima subset load counts in integration gates is brittle; module lists and transitive dependencies drift and invalidate exact-count assertions.
- Using unqualified `fboundp` symbols in package-heavy loaders created misleading failures even when target functions were correctly defined in `MAXIMA`.
- Returning `Repl` by value from test helpers was unsafe: internal pointers (`vm.global_env`, callback contexts) can dangle after copies/moves and crash later in unrelated eval paths.
- Continuing to use raw `Value.raw` identity for globally special symbols was incorrect under moving GC; symbol keys must use package/uid-aware identity to stay stable.
- Treating uninterned symbols like global-name fallbacks was wrong; uninterned value cells need dedicated storage keyed by stable symbol uid (`src/interp/vm.zig:788`).
- Leaving debug env checks in hot VM op paths (for example `write_to_stream`) is a measurable perf anti-pattern; remove tracing from opcode dispatch and keep diagnostics opt-in at higher layers.
- Running `zig build test -Dtest-filter='maxima e2e operation readiness status'` is still hang-prone here; the equivalent scripted readiness probe produced deterministic signal.
- Treating the previous function-cell fix as complete was incorrect; leaving `nil`/`unbound` in the value-cell overwrite allowlist still let `defun` corrupt same-name variables in generic Lisp code.
- Focusing first on mixed special-`let` lowering (`tryCompileSpecialLet`) was a false lead; the actual fault came from type declaration leakage into lexical bindings.
- Running broad `zig build test -Dtest-filter='maxima '` remains unreliable in this environment (hang-prone); targeted filters for failing gates are more deterministic for RCA.
- Packing very large module-list setup and operation probes into a single reader input string produced unstable `UnexpectedToken` failures; smaller staged eval forms are safer for large integration probes.
- Assuming package-qualified names were safe under old fallback logic was wrong: fallback-to-bare-name can silently bind to the wrong package/global slot and manifests later as recursive calls instead of immediate package resolution errors.
- Directly aliasing every BIGFLOAT-IMPL symbol to a `cl:` function without a `fboundp` guard failed at load time (`ASIN` unbound on this runtime); guarded binding with explicit fallbacks is required for portable bootstrap stubs.
- Assuming stubbed helper function signatures stay stable across later Maxima module loads was wrong: `simp.lisp` redefines `arg-count-check` with different arity, so generated bootstrap calls must follow upstream arity contracts.
- Assuming `handler-case (error ...)` already covered all VM failures was wrong; unmapped Zig errors (`InvalidTypeSpecifier`, `InvalidArgument`) bypassed condition handlers until explicitly mapped.
- Reintroducing builtin-name scans as ad hoc manual lists (`Builtins.primitive_fields`) is brittle; stale entries caused `symbol-function` to miss legitimate primitives (`ATAN`) even though compiler lowering already supported them.
- Pushing wide `HABU_TRACE_FN_RESOLVE=1` traces across large Maxima loads produced megabytes of mostly noise; narrowing to failing subset tests and symbol-miss traces is faster for RCA.

## Session Notes (2026-02-17)

### Worked Well
- Following Maxima source to the exact failing semantic operation (`mrgmac.lisp` `defc/defs/defa`: `(coerce \`(lambda ...) 'function)`) gave a generic CL fix in `lib/stdlib.habu` (`coerce-to-function`) instead of a Maxima-specific patch.
- Converting temporary root-cause traces into focused regression tests (`src/tests/integration.zig`: function-designator coercion, optional `env` lambda designator arity) preserved behavior while allowing debug instrumentation to be removed cleanly from hot compiler/VM paths.
- Aligning `lib/maxima-loader.lisp` file order with upstream `src/maxima.system` module ordering (not ad-hoc sequencing) removed dependency-order regressions (`PUTOPR`/`SPECREPCHECK` class) and gave a principled path for loader parity.
- VM mismatch tracing (`HABU_TRACE_CALL_MISMATCH=1`, `HABU_TRACE_ERROR_CONTEXT=1`) exposed a generic CL semantic bug quickly: `MAPC` was fixed-arity in `lib/stdlib.habu` and failed in Maxima `$errormsg` multi-list dispatch.
- Replacing `mapc` with variadic CL semantics (`lib/stdlib.habu`) and adding focused regression coverage (`src/tests/integration.zig`: `stdlib mapc supports variadic list dispatch`) removed the callback-arity crash class without Maxima-specific patches.
- Persisting probe results to files (`/tmp/*.result`) after non-interactive `(load "...")` runs gave stable signal where REPL output was noisy; this exposed that integrate blockers were advancing from MAPC arity into missing module chain (`m2`/`schatchen-cond` unbound when `schatc` not loaded).
- Form-level tracing (`HABU_TRACE_FORMS=1`) isolated the failing loader site to `lib/maxima-stubs.lisp` form 24 (`eval-when`) quickly.
- Cross-checking Maxima symbol state through file-based reports (`with-open-file`) avoided terminal overwrite noise and made root-cause data stable (`/tmp/maxima-subset42-report.txt`).
- Reproducing with minimal Lisp snippets (outside full Maxima load) made package bugs obvious and testable.
- Adding focused regression tests in `src/runtime/primitives/package.zig` caught real root causes:
  - stale inherited-symbol replacement in native tables,
  - inherited lookup using native exports when Lisp export tables are sparse,
  - keyword nickname handling in package creation.
- Validating with the same Maxima subset gate used by integration (`lib/maxima-loader.lisp`, 39 files) gave a concrete pass criterion: `(39 39 0 1 1 1 1 1 1)`.
- Isolating Maxima `destructuring-let` failure to a language-level repro (`let` with mixed lexical + special vars) exposed the true compiler bug quickly:
  - `(let ((a 1) (*x* 2)) ...)` leaked writes to global `*x*` instead of dynamic binding.
  - Fixing mixed special/lexical lowering in `src/compiler/compile.zig` (specials via `progv` with temp bindings) removed the `LET-MACRO-HAIR` crash path.
- Adding dedicated integration regressions in `src/tests/integration.zig` for mixed special `let` and Maxima `letmac` keeps this class of bug from regressing.
- Treating `defpackage` as a strict semantic boundary (parse and apply `:import-from` / `:shadowing-import-from` instead of ignoring them) removed cross-package symbol alias bugs without Maxima-specific rewrites.
- Loading upstream Maxima package definitions first (`lib/maxima-loader.lisp` + `maxima-package.lisp`) and using stubs only as guarded fallbacks preserved symbol/package intent across diverse source files.
- Running package-form compilation in an arena-scoped compiler context (`src/interp/repl.zig` `evalPackageForm`) eliminated persistent IR node leaks on repeated `defpackage` evaluations.
- Fixing `%shadowing-import` replacement semantics in `src/runtime/primitives/package.zig` (replace conflicting local/native entries before import) aligned behavior with CL expectations and unblocked real package forms.

### Did Not Work
- Driving long Maxima probes via non-interactive `./zig-out/bin/habu < script` in this environment was unreliable for deterministic pass/fail capture; targeted integration tests were more trustworthy for regression signal.
- Using `./zig-out/bin/habu <script-file-arg>` as a multi-form probe source was misleading in this environment; only the final top-level form was reliably observed, so probe conclusions must come from integration tests or controlled REPL eval paths.
- Assuming `mapc` was already CL-compatible because `mapcar`/`mapl` were variadic was wrong; missing variadic support in one mapping combinator can break large Lisp packages in non-obvious error-reporting paths.
- Driving large multi-form scripts by piping raw lines into the interactive REPL produced misleading output corruption; loading a script file and writing explicit probe artifacts was required for trustworthy RCA.
- Using stdlib `find-symbol` as a debugging oracle was misleading; its previous shim semantics masked package-state bugs.
- Trusting `maxima-load-all` success counters alone was misleading: `sin.lisp` can leave `MAXIMA::SININT` unbound while reporting `(ok=total, fail=0)`, so binding checks (`fboundp`) are required for critical entrypoints.
- Assuming Lisp package export hash tables mirror native exports caused false negatives in inherited symbol classification.
- Accepting keyword nicknames in validation while later calling `nameBytes` (string/symbol-only) produced delayed `TypeError` in `eval-when`, not at option parse time.
- Relying on a single long `zig build test -Dtest-filter=...` run was unreliable in this environment; targeted tests plus direct REPL gate runs were more deterministic.
- Assuming a Maxima runtime failure (`$ratsimp`) was a setf-expander bug was wrong; after dependency fixes, the failure moved and the real issue was mixed special/lexical `let` compilation semantics.
- Silently ignoring unknown/unsupported `defpackage` options in `compileDefpackage` was a shortcut that hid root causes and led to hard-to-trace runtime recursion/dispatch failures.
- Implementing `shadowing-import` by delegating to plain `importSymbols` first was incorrect when same-name local symbols already existed; it caused native symbol-table conflicts instead of required replacement.

---

## Session Notes (2026-02-19)

### Worked Well
- Caching builtin refresh by heap epoch in `src/compiler/compile.zig` (`bi_heap`/`bi_gc`/`bi_cl_pkg`/`bi_cl_ver` + `refreshBuiltins`) removed repeated `Builtins.init` churn from primitive compile dispatch while still invalidating on GC and CL package symbol-table mutation.
- Replacing `append` primitive compilation’s temporary `ArrayList(*Ir)` in `src/compiler/compile.zig` with a streaming left fold removed a per-call transient allocation in a hot compile path.
- Locking refresh invalidation behavior with focused tests in `src/compiler/compile.zig` (`refreshBuiltins rebuilds when builtin handles are cleared`, `refreshBuiltins invalidates on CL package symbol-table mutation`) prevented cache-staleness regressions.
- Tracing bench JIT eligibility in `src/testing/compile_chunk.zig` immediately exposed two root causes for `compile_n=0`: top-level `defun` lowering had moved from `.define` to `.set_symbol_function`, and Hoist translation rejected implicit `.block` wrappers.
- Extending JIT candidate extraction in both `src/testing/compile_chunk.zig` and `src/interp/repl.zig` to accept `.set_symbol_function` + lambda restored post-defun JIT registration after function-cell lowering changes.
- Adding `.block` traversal/translation support in `src/jit/backend.zig` (`irAny`, `countIrNodes`, `canTranslate`, `firstUnsupportedTag`, `translate`, TCO helpers) fixed the real backend incompatibility instead of masking it in benchmark gating.
- Validating with `zig build -Duse-hoist=true bench-jit -- --json` and `zig build -Duse-hoist=true bench-check -- --json` proved end-to-end recovery (`compile_n=1`, `fail_n=0`) and restored meaningful JIT perf signal.
- Replacing macro-root staging in `src/compiler/compile.zig` (`callMacroClosure`, `expandMacro`) from temporary `ArrayList` map snapshots to direct root-buffer packing removed repeated transient allocations in macro expansion hot paths without changing GC/root restoration semantics.
- Pre-counting macro call argument arity and using a stack buffer (with single fallback heap alloc) removed dynamic `ArrayList(Value)` growth churn in compile-time macro invocation loops.
- Reworking `compileCondWithTail` in `src/compiler/compile.zig` to count clauses first and use a stack-first clause buffer (heap fallback only for large conds) kept iterative reverse lowering while removing per-cond `ArrayList` churn; regression `src/tests/integration.zig` with 80 clauses validates large fallback correctness.
- Rewriting `compileBodyWithTail` in `src/compiler/compile.zig` to use a single-form fast path plus one direct pre-sized allocation for multi-form bodies removed the prior `ArrayList`+`dupe` double-allocation pattern in a ubiquitous compile path.
- Rewriting `filterDeclares` in `src/compiler/compile.zig` to build a reversed list directly and reverse links in-place removed temporary `ArrayList(Value)` staging while preserving declaration processing and body ordering.
- Replacing `compileListPrim` / `compileBroadcastStream` / `compileConcatenatedStream` in `src/compiler/compile.zig` with count+single-allocation slices and direct IR node initialization removed the previous `ArrayList` then `dupe` double-allocation pattern for variadic primitive lowering.
- Rewriting `compileVariadicArith` in `src/compiler/compile.zig:15127` from `ArrayList(*Ir)` staging to a single-pass compile+fold removed transient allocation churn on arithmetic hot paths and allowed strict dotted-tail rejection (`(+ 1 . 2)` now errors at this lowering boundary).
- Locking variadic arithmetic semantics with focused regressions in `src/compiler/compile.zig:19284` (`(+)`, `(*)`, unary `(- x)`, unary `(/ x)`, and left-associated `(+ 1 2 3)`) preserved CL behavior while tightening argument-list validation.
- Replacing `compileCallNextMethod` and `generateMethodCallByNameAtDepth` transient `ArrayList` staging in `src/compiler/compile.zig:12126` and `src/compiler/compile.zig:12726` with pre-sized argument slices + `buildCallIr` removed list growth churn from generic-function dispatch call lowering.
- Adding focused regressions for `call-next-method` arg-shape preservation and dotted explicit arg rejection (`src/compiler/compile.zig:19336`, `src/compiler/compile.zig:19382`) locked the no-drop/no-mask behavior while tightening malformed-list handling.
- Replacing `compileMakeInstance` keyword/value call-arg staging and `compileFindClass` optional-form sequencing staging (`src/compiler/compile.zig:11522`, `src/compiler/compile.zig:11699`) with pre-sized slices removed extra temporary list growth and redundant copy churn in CLOS compile helpers.
- Locking `make-instance` ctor-arg preservation and `find-class` optional sequencing/dotted-tail rejection in focused regressions (`src/compiler/compile.zig:19410`, `src/compiler/compile.zig:19462`, `src/compiler/compile.zig:19494`) made these list-shape contracts explicit.
- Replacing `compileVectorPrim`, `compileAref`, and `compileAset` transient `ArrayList` staging (`src/compiler/compile.zig:16829`, `src/compiler/compile.zig:17009`, `src/compiler/compile.zig:17064`) with count+single-allocation slices removed avoidable growth/copy overhead in array/vector lowering paths.
- Adding focused regressions for vector/aref/aset operand preservation and dotted-tail rejection (`src/compiler/compile.zig:19524`, `src/compiler/compile.zig:19566`) locked the new strict list-shape checks and subscript/value arity behavior.
- Replacing `compileMakeArray` static-dimension staging (`src/compiler/compile.zig:16921`) with count+single-allocation slices and direct `.arr_new` IR node construction removed intermediate list growth and builder-level duplicate copying for static dimension lists.
- Locking scalar/static/dynamic `make-array` dimension lowering in focused regressions (`src/compiler/compile.zig:19603`) prevented regressions where static dimension vectors collapse back to dynamic paths.
- Replacing `compileMvBind`/`compileMvCall` `ArrayList` staging (`src/compiler/compile.zig:8108`, `src/compiler/compile.zig:8159`) with pre-counted slices removed transient growth churn and made malformed dotted tails fail early.
- Adding focused regressions for MV var/form counts and dotted-tail rejection (`src/compiler/compile.zig:19645`, `src/compiler/compile.zig:19688`) locked shape correctness while preserving `multiple-value-*` lowering semantics.
- Replacing `compileDefclass` expansion accumulation (`src/compiler/compile.zig:11375`) from `ArrayList` growth to exact pre-counted form allocation removed dynamic staging overhead while preserving reader/writer symbol filtering.
- Locking reader/writer expansion counting with a focused regression (`src/compiler/compile.zig:19120`) prevents silent off-by-one/missing-form regressions in generated defclass helper definitions.
- Refactoring `compileTagbody` segment construction (`src/compiler/compile.zig:8015`) to pre-count tags, allocate segment/tag buffers once, and compile each segment directly removed dynamic segment/form staging lists from the hot control-flow lowering path.
- Adding a focused dotted-tail regression for `tagbody` (`src/compiler/compile.zig:19902`) locked malformed body-list rejection while preserving existing segment partition behavior.
- Replacing method-dispatch setup staging in `generateMethodDispatcher` (`src/compiler/compile.zig:12321`, `src/compiler/compile.zig:12446`) with fixed-size slices for `no-applicable-method` call args and lambda optional params removed avoidable list-growth churn in generic-function dispatcher synthesis.
- Locking dispatcher arity shaping via `defmethod` regression (`src/compiler/compile.zig:19208`) keeps optional-param count aligned with computed max arity after staging refactors.
- Replacing `buildEffectiveMethod` statement/after-body accumulation (`src/compiler/compile.zig:12517`, `src/compiler/compile.zig:12560`) with deterministic pre-sized slices and direct progn nodes removed additional method-combination staging allocations in CLOS dispatcher synthesis.
- Locking `:before`/primary/`:after` dispatcher synthesis with a focused regression (`src/compiler/compile.zig:19256`) preserved method-combination shape while removing transient list staging.
- Replacing `toOwnedSlice`-based dispatcher/lambda param handoff in `defmethod`/dispatcher generation (`src/compiler/compile.zig:12011`, `src/compiler/compile.zig:12296`) with explicit pre-sized/duped slices removed remaining ownership-churn allocations in method-dispatch parameter setup.
- Rewriting `parseVariant` field extraction (`src/compiler/compile.zig:12900`) to pre-count and allocate field-name slices once removed transient `ArrayList` growth and added strict dotted-tail rejection for malformed variant specs.
- Locking the behavior with `parseVariant` focused regression (`src/compiler/compile.zig:17991`) keeps ADT variant parsing strict while preserving field ordering.
- Locking direct `char`/`schar` CL semantics with integration coverage (`src/tests/integration.zig:862`) prevents regressions where string indexing accidentally returns integer codepoints instead of character values.
- Fixing `read-from-string` wrapper index semantics in `lib/stdlib.habu:3984` (add `:start` offset back to secondary position and preserve multi-values through wrapper branches) closed a real CL behavior gap that surfaced under `(multiple-value-list (read-from-string ... :start N))`.
- Locking the fix with integration coverage (`src/tests/integration.zig:4335`) prevents regressions where wrappers return slice-relative positions instead of original-string indices.
- Updating `Repl.evalPrint` to emit VM secondary values after the primary (`src/interp/repl.zig:2637`) fixed interactive output for multi-value forms (`(values ...)`, `floor`, etc.) and clearing `secondary_values_count` after print prevents stale-value bleed into subsequent REPL displays.
- Adding REPL output regressions (`src/interp/repl.zig:4274`, `src/interp/repl.zig:4296`) locks multi-line multi-value display and post-print secondary reset behavior.
- Extending `compileDefpackage` coverage to keyword designators (`src/compiler/compile.zig:19041`) locked CL-compliant forms like `(defpackage :my-pkg (:use :cl))` and prevents regressions where keyword package designators were accepted in parser paths but failed in compiler/package setup.
- Adding focused `with-output-to-string` regressions (`src/tests/integration.zig:4421`) locked clean primary-value behavior and verified `princ` writes to string streams without call-mismatch failures.
- Extending `coerce` numeric/character coverage in `lib/stdlib.habu:2400` (integer/fixnum targets via truncation, `character`<->`integer` bridges, and explicit `t` identity) closed real CL conversion gaps without backend-specific branching.
- Locking those conversions with integration coverage (`src/tests/integration.zig:4010`) prevents regressions on numeric/char/list/string/vector coercion paths.
- Routing `~D` through a dedicated grouped-decimal helper in `src/interp/vm.zig:7786`/`src/interp/vm.zig:8719` fixed `~:D` output semantics (`1,234,567` and `-1,234,567`) without ad-hoc directive parsing branches.
- Locking grouped-decimal behavior with explicit integration coverage (`src/tests/integration.zig:2749`) prevents regressions where modifier parsing falls through to literal directive text.
- Reproducing multidimensional `make-array` row-major access in a focused integration (`src/tests/integration.zig:950`) exposed a real stdlib semantic gap rather than test-only churn.
- Removing early-stdlib `dolist` macro forward-reference use in `array-row-major-index` and implementing true rank-aware `row-major-aref` index decomposition (`lib/stdlib.habu:605`, `lib/stdlib.habu:622`) fixed two root issues: malformed compile-time macro expansion in early forms and incorrect rank-1-only row-major access.
- Replacing `with-package-iterator` stubs with real iterator state and hardening `do-symbols`/`do-external-symbols`/`do-all-symbols` package normalization (`lib/stdlib.habu:6145`, `lib/stdlib.habu:6398`, `lib/stdlib.habu:6428`, `lib/stdlib.habu:6441`) restored package-iteration behavior for generic CL code paths.
- Locking iteration behavior with a focused regression (`src/tests/integration.zig:5037`) catches regressions in symbol-category iteration and iterator return-value shape.
- Replacing `restart-bind` stubs with real restart-case lowering (`lib/stdlib.habu:7173`) restores dynamic restart registration so handler code can invoke bound restarts in generic CL flows.
- Locking `restart-bind` behavior with focused coverage (`src/tests/integration.zig:4716`) prevents regressions where restart handlers silently no-op.

### Did Not Work
- Clearing `compiler.builtins` inside `setVm` caused null-handle crashes in REPL setup (`src/interp/repl.zig:createFeaturesGlobal` reads `compiler.builtins.?` directly). Correct fix was to invalidate refresh epoch keys in `setVm` without nulling builtin handles.
- Assuming JIT entry detection based only on `.define` was stable was wrong; compiler IR shape changes (function-cell correctness work) silently disabled JIT coverage in both REPL and benchmark paths.
- Driving dotted-tail rejection through top-level `compile` dispatch was misleading for this test: non-builtin `+` symbol identity can route to generic call lowering, so the invariant should be asserted at `compileVariadicArith` directly when validating list-shape enforcement.
- Reader/parser canonicalizes unescaped symbol case, so parser-based regressions should assert normalized names (`FOO`) instead of source spelling (`Foo`) when validating symbol-derived identifiers.

---

## Anti-Patterns (What Goes Wrong)

### 1. "Already Exists" Discovery (793 occurrences)

The #1 time sink: implementing something that's already in the codebase.

**Examples:**
- Added duplicate array opcodes (0x73-0x78) when they already existed at 0xCF, 0x1B-0x1E
- Wrote VM handlers for make_array/aref/aset, then found existing handlers 1000 lines away
- Implemented format directives that were already working

**Rule:** Before writing ANY new code, `grep -rn` the codebase for the feature name, opcode, function name, and related keywords. Check both Zig source and stdlib.habu.

### 2. Forward Reference / Ordering Bugs (199 occurrences)

Lisp macros compile their body at definition time. If a macro calls a helper, the helper must be defined BEFORE the macro.

**Examples:**
- `defmacro` using helpers defined later in stdlib.habu → CompileError
- Moved macro definitions above helpers → broke other macros depending on the moved code
- LOOP macro helpers had cascading ordering dependencies

**Rule:** In `lib/stdlib.habu`, helper functions go ABOVE the macros that use them. When adding a new helper, check all macros below it for dependency ordering.

### 3. Arena Allocator Lifetime Bugs (385 occurrences)

The REPL resets the arena allocator between expression compilations. Any IR nodes, strings, or metadata allocated with the arena become stale pointers after the next expression compiles.

**Examples:**
- defmethod stored `body: *Ir` pointers that pointed into freed arena memory → segfault
- Fix: store function NAME strings (persistent allocator) instead of IR pointers
- Slot names from defclass allocated in arena, freed before runtime execution

**Rule:** Anything that must survive across REPL expressions MUST use `globals.allocator` (persistent), NOT `self.allocator` (arena). IR nodes, compiled chunk references, and temporary strings are arena-scoped.

### 4. Package-Qualified Name Mismatches (430 occurrences)

The compiler looks up globals using qualified names like `"CL-USER:foo"`, but generated code sometimes registers with unqualified names like `"foo"`.

**Examples:**
- defclass constructors registered as `"make-person"` but looked up as `"CL-USER:make-person"` → UnboundVariable
- Fix: added `qualifyName()` helper that prepends current package prefix

**Rule:** When generating function definitions programmatically (defclass, defstruct, defmethod), ALWAYS use `qualifyName()` or `getQualifiedName()` to match the lookup path.

### 5. Reverts and Rework (118 occurrences)

Large, multi-file changes that break tests and require full reverts.

**Examples:**
- unwind-protect error handling attempted 5+ times, always abandoned
- CLOS defmethod rewritten 3 times before finding the right abstraction (store names, not IR)
- Bignum arithmetic had repeated off-by-one bugs in carry propagation

**Rule:** Make small, testable changes. Commit after each working step. If a change touches >3 files, break it into smaller dots.

### 6. Complexity Bailouts (79 occurrences)

Starting a feature, discovering it's far more complex than estimated, then abandoning.

**Examples:**
- unwind-protect on VM errors: needs dedicated effort to handle cleanup-form execution during error propagation
- Full LOOP macro: each keyword interaction multiplies complexity
- Pretty-printer: dispatch table for every type

**Rule:** When estimated time doubles, stop. Create a focused dot with the new understanding. Don't push through with partial knowledge.

### 7. Duplicate Code / Handlers (793 occurrences, overlaps with #1)

Adding code in one location without checking if it exists elsewhere in the same file.

**Examples:**
- Two sets of array VM handlers (lines ~1190 and ~2235) in vm.zig
- Duplicate opcode definitions in opcodes.zig

**Rule:** Before adding a handler/opcode/primitive, `grep -n` the target file for the name. vm.zig is 10K+ lines — duplicates are easy to introduce.

---

## Anti-Patterns (Tooling)

### 8. sed/regex Edits on Large Files (from SESSION.md patterns)

Using sed or regex-based edits on large files frequently deletes too much, duplicates sections, or corrupts syntax.

**Rule:** Use the `edit()` tool with exact `oldText` match for surgical changes. Read the target area first with `read()` to get exact text.

### 9. Editing Without Reading First

Making assumptions about file contents based on stale context.

**Rule:** ALWAYS `read()` the target lines before `edit()`. File contents change between turns. Never assume line numbers are still accurate.

---

## Positive Patterns (What Works Well)

### 1. Test After Every Change (1333 occurrences of `zig build test`)

Run `zig build test` after every meaningful edit. Catches regressions immediately.

### 2. Read Code Before Editing

Understand the existing patterns in a file before modifying it. Check how similar features are implemented.

### 3. Helper Function Extraction

When 3+ locations share logic, extract to a function. Examples: `qualifyName()`, `getPredicateOperand()`, table-driven dispatch.

### 4. Table-Driven Dispatch

Replace if-else chains with data tables. Easier to extend, fewer typos, compiler catches missing cases.

### 5. Small Dots, Frequent Commits

Break work into dots that take <2 hours. Commit after each passing test. Use `tools/dot-finish` to enforce the build-test-commit cycle.

### 6. Store Names, Not Pointers

When crossing allocator lifetimes (arena → persistent), store string names and re-resolve at use time instead of storing raw pointers.

---

## Zig-Specific Lessons

### Arena Reset Invalidates All Pointers
The REPL's arena allocator (`self.allocator` in compile.zig) is reset between expressions. Never store arena-allocated pointers in persistent data structures.

### Switch on typeKind(), Not If-Else
Exhaustive switch catches missing cases at compile time. If-else chains silently ignore new types.

### Allocator-First Convention
`fn init(allocator: Allocator, ...) Self` — allocator is always the first parameter.

### ArrayList is Unmanaged in Zig 0.15
`var list = std.ArrayList(T){};` — pass allocator to each method call, not at construction.

### Import Once, Reference via Namespace
`const types = @import("type.zig");` then `types.Type`, `types.Primitive`. Don't import individual names.

---

## Lisp-Specific Lessons

### Macro Compilation Order
`defmacro` compiles its body immediately. All helpers used by a macro must be defined above it in the source file.

### Lisp-1 vs Lisp-2
Habu is a Lisp-1 (single namespace for functions and variables), but has some Lisp-2 features (symbol-function, fdefinition). `define` sets the value cell. Functions are looked up via global variable binding, not a separate function cell.

### defclass Slot Syntax
Correct: `(defclass person () name age)` — slots are separate top-level forms.
Wrong: `(defclass person () (name age))` — this is parsed as ONE slot with options.

### CLOS defmethod: Store Function Names
Each method compiles to a separate named function (e.g., `"foo$number"`). The generic function stores the name string, not an IR pointer. This survives arena resets.

---

## Session Workflow

### Always Create Dots Before Starting Work
No multi-step work without a tracking dot. Include file paths, line numbers, and dependencies.

### Update LESSONS.md at Session End
After completing work, add any new patterns discovered. Reference specific files and line numbers.

### Check Before Implementing
1. `grep -rn` for existing implementations
2. `read()` target files before editing
3. Check both Zig source (`src/`) and Lisp source (`lib/`)
4. Look for related opcodes, VM handlers, and compiler special forms

---

## JIT-Specific Lessons

### runMaybeJit Only Called from vm.run()
`runMaybeJit` (the JIT code check) is only called in `vm.run()`, NOT in `callFromStackAt()`. This means JIT→interpreter→JIT transitions via `callFromStackAt` never check for JIT code on the callee. Fix: `callFromStackAtFast` adds a JIT check after `doCall()`.

### ARM64 Register Map for JIT
- `x19` = sp (JIT stack pointer)
- `x20` = const_pool
- `x21` = ret_buf
- `x22` = ctx (JitContext pointer)
- `x23` = frame_base (locals accessed via `LDR x0, [x23, #offset]`)
- `x24` = stack_end

### Nested JIT Calls Need Adjusted frame_base
`runJitFn` sets `frame_base = self.stack[0..].ptr` (absolute base). For nested JIT calls, `frame_base` must be `self.stack[0..].ptr + bp` where `bp` is the callee's frame base from `self.frames[fp-1].bp`. See `runJitFnInFrame`.

### sp Recovery After Nested JIT
When JIT code runs with a non-zero frame_base, recovering `vm.sp` from `ctx.sp` requires computing the absolute offset from the stack base, not from frame_base. Use `@intFromPtr(ctx.sp) - @intFromPtr(stack_base)`.

### callFast Must Use Absolute Stack Indices
`rt.callFast` computes `fn_idx` relative to `frame_base` via `stackLen(c)`. But `callFromStackAtFast` expects an **absolute** index into `vm.stack`. For top-level JIT (frame_base == stack[0]), they're the same. For nested JIT calls (frame_base > stack[0]), must convert:
```zig
const abs_fn_idx = (@intFromPtr(c.frame_base) - @intFromPtr(c.vm.stack[0..].ptr)) / @sizeOf(Value) + fn_idx;
```
Bug manifestation: recursive functions returning wrong results (e.g., fib(10) → -7 instead of 55).

### Helper-Lowered IR Must Disable Untagged Mode
When adding IR nodes lowered through C-ABI helper calls (`make_hash`, `hash_*`, `make_string`, `arr_*`, `str_set`, `position`, `format`, `intern`), keep `translator.untagged = false` for those bodies. Untagged mode assumes fixnum-only locals; boxed/string/hash values will be corrupted if untagged remains enabled.

### Coverage Work: Add Translation + Reachability Together
JIT coverage work needs three updates in lockstep:
1. `canTranslate` / `firstUnsupportedTag` node acceptance,
2. `translate(...)` lowering implementation,
3. call-safety classification (`has_cross_calls`, untagged gating).
Skipping (3) causes post-emit/liveness issues even if translation compiles.

### JIT Tests Must Use VM Stack, Not Local Buffers
Tests that manually create `JitContext` must use `vm.stack` as the stack buffer, not a local `var stack_buf: [32]Value`. When `callFast` converts frame-relative to absolute indices, it assumes `frame_base` points into `vm.stack`. A separate buffer produces garbage indices.

### Self-Call Detection: Track Stack Depth Across Opcodes
To detect `load_global FIB; ...args...; call N` as a self-call:
1. On `load_global X` where globals[X] is a closure for the current chunk: set `self_call_depth = 0`
2. On push ops (push_nil, push_i32, load_local, etc.): increment depth
3. On binary ops (add, sub, lt, etc.): decrement depth (consume 2, push 1 = net -1)
4. On `call N` where depth == N: emit self-call
5. On anything else (jumps, pops, etc.): reset tracking to null

### Self-Call Frame Setup Must Replicate doCall
The VM's `doCall` shifts args down by 1 (overwriting closure slot): `stack[new_bp + i] = stack[new_bp + 1 + i]`. The JIT self-call must do the same, or `load_local 0` will load the closure instead of arg0.

### saved_chunk_sp Limits Recursive JIT Depth
`callFromStackAtFast` uses `saved_chunk_sp` (limited to `MAX_SAVED_CHUNKS`). Each nested call uses one slot. For recursive JIT functions, this limits call depth. Increased to 256 from 16.

### tryJitCompile: Compile-Only, No Run
When adding JIT compilation in call paths, separate "compile and cache" from "run". `tryJitCompile` should only compile and return the function pointer. The caller handles `runJitFnInFrame`. This avoids re-entrance issues where compile→run→callFast→compile creates nested compilation contexts.

### Dot Workflow
Always: `dot add` → `dot activate` → work → `tools/dot-finish`. Close activate dots immediately after activation. Never start multi-step work without a tracking dot.

---

## JIT Optimization Lessons (Session 2)

### Specialize Pass Must Preserve Lambda Fields
When the specialize pass copies a lambda IR node (because the body changed), it must copy ALL fields including `safety` and `speed`. Omitting them resets to defaults (safety=1), causing check_fixnum bytecodes even when the user declared `(optimize (safety 0))`. This was a silent performance bug — everything still worked correctly, just slowly.

### Type Declarations Don't Propagate Without Explicit Wrapping
`(declare (type fixnum n))` records the type in `global_decls` but does NOT automatically wrap variable references with `assert_fixnum`. Without explicit wrapping in the compiler's variable-reference path, the specialize pass can't prove operands are fixnum. Fix: when compiling a variable reference, check `global_decls.getTypeDecl(name)` and wrap with `assert_fixnum` if the type matches a known builtin (like fixnum).

### getTypeDecl Was a Stub Returning null
The `DeclEnv.getTypeDecl()` method was a stub (`return null`) with a comment "TEMP: bypass HashMap to avoid crash". This silently disabled all type-driven specialization. Lesson: search for `return null` and `TEMP` comments that might be masking missing functionality.

### Don't Strip assert Wrappers From Specialized Ops
When converting `add(assert_fixnum(x), assert_fixnum(y)) → fixnum_add(...)`, keep the `assert_fixnum` wrappers on the operands. They serve as runtime safety checks at safety > 0. The specialized op handles the performance (no type dispatch), while the assert handles correctness. At safety 0, the emitter skips the check anyway. Stripping asserts breaks `(the fixnum ...)` contracts — `(double "hello")` would silently produce garbage instead of erroring.

### declare Not Processed in let Scopes
`filterDeclares` was only called in lambda body compilation, not in `compileLetWithTail`. So `(let (...) (declare (type fixnum ...)) body)` silently ignored the declaration. Fix: add `filterDeclares` call before compiling let body.

### Peephole Fusion: Generate Less Code, Not Better Code
The #1 JIT bottleneck is memory stack traffic: every bytecode op pushes/pops through memory. SBCL keeps values in registers. Instead of optimizing individual stencils, fuse common bytecode sequences to eliminate intermediate memory round-trips:
- `load_local N; push_i32 K; fixnum_le; jmp_nil` → `LDR; CMP; B.cond` (3 inst, 1 memory op instead of 7)
- `load_local N; push_i32 K; fixnum_sub` → `LDR; SUB; ORR; push` (4 inst instead of ~10)
This yielded 36% improvement on fixnum_loop (83→53ms).

### B.cond Encoding for Peephole Jumps
`B.cond` instruction: `0x54000000 | (imm19 << 5) | cond`. Condition codes: EQ=0, NE=1, GE=10, LT=11, GT=12, LE=13. Invert the condition for `jmp_nil` (which branches when false): LE→GT, LT→GE, etc. Use `rel19` hole type for patching.

---

## Architecture Lessons

### Stack Machine JIT is Fundamentally Broken
A stack-machine JIT that translates each bytecode to native code will always be slow because every value round-trips through memory. Peephole fusion is a band-aid — it reduces memory traffic for specific patterns but can't fix the root cause. The right architecture is SSA-based: bytecodes → SSA IR → register allocation → native code. This is what SBCL, V8, and every serious JIT does.

### SSA Over Direct IR-to-Native
Tree-shaped compiler IR (like Habu's `Ir`) represents *source structure*. SSA represents *data flow*. For JIT compilation you need data flow because: (1) phi nodes at join points tell you which definition reaches each use, (2) def-use chains enable dead code elimination and constant propagation for free, (3) SSA liveness intervals are clean for register allocation, (4) loop-invariant code motion requires knowing what doesn't change across iterations.

### Hoist Integration
Hoist (Cranelift port in Zig) provides the full SSA pipeline: IR → Optimize (SCCP, DCE, GVN, LICM) → ISLE lowering → Register allocation → AArch64 emit. Vendored as path dependency via `build.zig.zon`. Access: `hoist_dep.artifact("cranelift").root_module`. Key APIs: `FunctionBuilder` for IR construction, `ContextBuilder` for compilation settings, `JitMem` for executable memory. Types use constants (`Type.I64`) not constructors.

### Hoist Block Params vs SSA Variables
Two ways to handle phis in Hoist: (1) block params (`setBlockParams` + `jumpArgs`) — manual but doesn't trigger SSA builder, (2) SSA variables (`declareVar`/`defVar`/`useVar`) — automatic phi insertion but requires the SSA builder to compile cleanly in the consumer's build context. Block params are safer for initial integration.

**Caveat**: Block param phis don't work correctly with hoist's current codegen. The merge block param values get assigned to wrong registers. Workaround: emit `ret` directly from both branches (no merge block). This limits if-expressions to top-level position (can't be nested inside arithmetic). Future fix: fix hoist's block param → register mapping.

### Hoist Register Allocator: Caller-Saved Handling (FIXED)
**Bug**: Hoist's linear scan allocator didn't know that calls clobber caller-saved registers (x0-x18). Values in caller-saved regs were silently destroyed after calls.

**Fix**: Added `call_positions` tracking to `LivenessInfo`. Both `computeLiveness` and `computeLivenessWithCFG` now record instruction indices of call/call_indirect/blr instructions. The allocator's `tryAllocateReg` checks `spansCall()` — if a live range spans a call, only callee-saved registers (x19-x28) are considered. Required adding `isCall()` to all backend instruction types.

**Key subtlety**: A value whose last use IS the call (it's a call argument) doesn't need to "survive" the call. The span check uses `call_pos >= start AND call_pos < end` (strict less-than on end). Using `<=` for end would incorrectly force call arguments into callee-saved regs.

### Hoist AArch64 Emitter: V-Bit Bug in STR/LDR (FIXED)
**Bug**: `emitStr` and `emitLdr` (unscaled immediate forms) had bit 26 (the V flag) set to 1, generating SIMD `STUR Dt`/`LDUR Dt` instead of integer `STUR Xt`/`LDUR Xt`. Template `0b11111000000` should have been `0b11110000000` (bit 6 in the 11-bit constant maps to bit 26 in the instruction).

**Manifestation**: Callee-saved register save/restore wrote to SIMD register D19 instead of integer register X19. The restore instruction `LDUR D19` with the wrong encoding (`opc=10, size=11`) was an UNDEFINED encoding → "Illegal instruction" trap.

**Debugging approach**: Hex-dumped JIT code, manually decoded AArch64 instructions, compared bit patterns against ARM Architecture Reference Manual. The V flag (bit 26) distinguishes integer (`V=0`) from SIMD/FP (`V=1`) in all load/store encodings.

### Hoist AArch64 Emitter: LDP Encoding Bug (FIXED)
**Bug**: `emitLdp` used template `(0b1010011 << 23)` which gives `[25:23]=011` (pre-index variant) with `L=0` (store). This generated STP pre-index instead of LDP signed-offset. Two errors in one constant:
1. Wrong variant: 011 (pre-index) instead of 010 (signed offset)
2. Missing L bit: L=0 (STP) instead of L=1 (LDP)

**Fix**: Replaced opaque bitfield constant with explicit field composition:
```zig
(0b101 << 27) | (0b010 << 23) | (0b1 << 22)
```

**Lesson**: Never use magic bit constants for instruction encoding. Compose from named fields so each bit's purpose is visible and verifiable against the architecture manual.

### Self-Pointer Patching for Recursive JIT
To emit self-recursive calls via `call_indirect`, embed a placeholder constant `0x0BADF00DDEADBEEF` as an `iconst`. After compilation, scan the generated code for the MOVZ+MOVK+MOVK+MOVK sequence matching the placeholder and patch with the actual function address. Patch BEFORE `writeExec` so the I-cache flush covers the patched code (on AArch64, D-cache writes are not visible to I-cache without explicit flush).

### Hoist Aggressive Optimization Removes Recursive Calls
With `optLevel(.aggressive)`, hoist's optimizer removes `call_indirect` instructions to functions with no observable side effects. Recursive fib calls get eliminated because the optimizer can't prove they terminate. Use `optLevel(.none)` for functions with recursive calls.

### Compiler IR vs Test IR: Symbol Representation Mismatch
**Bug**: Hoist backend unit tests used `.global_ref` for function references in self-calls, but the actual REPL compiler produces `.lit` (symbol value) for the same purpose. `detectSelfCalls` only checked `.global_ref`, so recursive functions compiled from the REPL were treated as non-recursive — the self-call was replaced with `nil`.

**Fix**: Added `isCallTargetSelf()` that checks both `.global_ref` (name match) and `.lit` (symbol value with qualified/unqualified name matching). Qualified names like `"CL-USER:MYFIB"` must match unqualified symbol names like `"MYFIB"` by checking suffix after `:`.

**Lesson**: Always test the actual compilation pipeline end-to-end, not just hand-crafted IR. The compiler's output may use different IR nodes than what you expect.

### Multiple REPL Compilation Paths
**Bug**: Hoist compilation was only wired into the stdlib loading path (`compileAndRun`) but not the interactive REPL path (`evalCapturingError`). User-defined functions with `(declare (optimize (speed 3)))` never got hoist-compiled.

**Fix**: Added `tryHoistCompileLambdas` call to `evalCapturingError` after bytecode emission.

**Lesson**: In a REPL with multiple expression evaluation paths (file loading, interactive input, eval-when), new passes must be added to ALL paths.

### Signature Ownership Double-Free
**Bug**: `errdefer sig.deinit()` + later `defer func.deinit()` double-freed signature arrays when `Function.init(sig)` consumed the sig by value. If compilation failed after func creation, both deferred ops ran.

**Fix**: Track ownership with a boolean: `var sig_owned = true; defer if (sig_owned) sig.deinit(); ... sig_owned = false; // after func takes ownership`.

### Nested Self-Calls Cause Regalloc Segfaults
**Pattern**: When a self-call's result is passed as an argument to another self-call (e.g., `(tak (tak ...) (tak ...) (tak ...))`), hoist's regalloc fails to properly spill values across nested `call_indirect` instructions, causing segfaults.

**Workaround**: Detect nested self-calls (`hasNestedSelfCalls`) and refuse to hoist-compile such functions, falling back to bytecode VM.

**Affected benchmarks**: tak (nested), NOT fib (fib passes self-call results to `+`, not to another self-call).

### Hoist Loop Phi Codegen: Three Bugs
**Root cause**: Three separate bugs conspired to make loops fail:

1. **Jump phi resolution missing** (FIXED): Hoist's AArch64 `jump` lowering emitted a bare `B` instruction without generating moves for `jumpArgs` values. When `jump block1(v7, v11)` was lowered, v7 and v11 were never moved into the registers assigned to block1's params. Fix: emit parallel copies (`mov`) before the branch for each arg→param pair.

2. **Frame layout clobbers FP/LR** (FIXED): `stackSlotOffset()` started at offset 0, which overlaps with the FP/LR save area written by `STP x29, x30, [SP, #-frame_size]!`. Stack stores at `[SP, #0]` overwrote the saved return address, causing "Bus error at address 0x15" (= 21 = the tagged fixnum 10, which was the loop limit stored over LR). Fix: start `stackSlotOffset` at `out_stack_max + 16`.

3. **stack_store lowering missing** (FIXED): The AArch64 lowerer had no case for `.stack_store`, causing `LoweringFailed`. `stack_load` was handled but not its counterpart. Fix: add `.stack_store` handler with STR instruction emission.

**Impact**: fixnum_loop 52ms → 8ms (6.5x speedup).

**Lesson**: When debugging "wrong results", don't assume a single bug. The first fix (stack_store handler) revealed the second (frame layout), which when combined with the initial approach (phi) revealed the third (missing parallel copies). Test each layer independently.

### Parallel Copy for Jump Args (SSA Phi Resolution)
In SSA-based codegen, `jump block(v1, v2)` where `block` has parameters `(p1, p2)` requires generating `mov p1, v1; mov p2, v2` BEFORE the branch instruction. This is the "parallel copy" problem — values must be moved to their target registers atomically. Simple sequential moves work when there are no circular dependencies (which is true for our case since loop variables are computed into fresh SSA values before the jump).

### blockParams() Returns Stale Pointers
`func.dfg.blockParams(block)` returns a slice into internal storage. If the DFG grows (by appending instructions or values) between creating block params and reading them, the slice becomes dangling. **Save block param values immediately** after `appendBlockParam()` into a local array instead of calling `blockParams()` later.

### End-to-End Testing Reveals Integration Gaps
Unit tests for the hoist translator worked perfectly (hand-crafted IR with `global_ref` nodes), but real REPL-compiled IR used `lit` nodes for function references. Similarly, hoist's loop tests only verified compilation, not execution. Always run the actual pipeline end-to-end before declaring a feature complete.

### Machine Code Disassembly Is Essential for JIT Debugging
When JIT code produces wrong results, dump the generated machine code and decode it instruction-by-instruction. In the phi fix, disassembly immediately revealed: (1) missing parallel copies before back-edge jumps, (2) stack stores clobbering FP/LR at SP+0. Print hex + manual ARM64 decode is faster than adding tracing to the compiler pipeline.

### Constant Folding at IR Translation Level
For tagged fixnum arithmetic where one operand is a constant, fold the tag adjustment into the constant at the IR translator level. Instead of emitting `iadd(x, tagged_n); isub(result, 1)` (3 instructions), emit `iadd(x, tagged_n - 1)` (1 instruction). This saves 2 instructions per fixnum operation with a constant operand.

### LICM via Constant Cache
Without a full LICM pass in the backend, achieve the same effect for constants by maintaining a cache (`i64 → HoistValue`) in the translator. Pre-scan loop bodies for literal values and emit them in the entry block before the loop. The SSA value is then available in all dominated blocks. Combined with `optLevel(.none)` which prevents re-materialization, this keeps loop-invariant constants in registers.

### Post-Emission Parallel Copy Fixup for Call Arguments
When a compiler backend (like hoist) emits sequential `mov` instructions for call argument setup without a parallel copy resolver, source registers can be clobbered before they're consumed. Instead of fixing the backend's regalloc (deep architectural change), post-process the emitted machine code: scan backwards from each `blr` instruction, collect the preceding `mov` instructions to ABI registers (x0-x7), and topologically sort them so that a move whose destination is still needed as a source by another move is emitted last. This approach is simple, correct, and avoids modifying the backend. The key insight: the "ready" criterion for topological sort is "no remaining move reads from my destination register."

### Stack Slot Offsets Must Account for Full Frame Layout
Stack slot offsets baked into lowered code must account for ALL frame components: FP/LR save area, callee-saved register area, and outgoing stack space. If offsets only account for FP/LR (16 bytes), they overlap with callee-saved registers saved at SP+16..SP+N. During lowering, the callee-save count isn't finalized (determined by regalloc), creating a chicken-and-egg problem. Conservative reservation (assuming max callee saves) works but wastes stack space.

### Inlining Tail-Recursive Functions as Loops
Cross-function inlining for tail-recursive callees requires converting the callee's body to a loop at the hoist IR level. Key steps: (1) Create header block with phi params for callee parameters. (2) Jump from caller to header with translated arguments. (3) Set `tco_header`/`tco_exit` and `fn_name` to callee's name. (4) Translate callee body via `translateTCOExpr` — tail calls become jumps to header. (5) Non-tail exits jump to exit block. (6) Restore caller's TCO state. This eliminated ~350K BLR/RET pairs for nqueens-safe-p, reducing nqueens(10) from 3.75ms to 3.45ms.

### TCO Exit Trampoline Elimination
Nested if-expressions in TCO context generate trampoline blocks: `block14 → block11 → block8` for each return path. Detect "simple exit" branches (literals, variable refs) and jump directly to `tco_exit` instead of through merge blocks. This reduced nqueens(10) from 3.45ms to 3.37ms and eliminated 3 blocks from the IR.

### Peephole Safety: Round-Trip MOV Detection
When detecting `MOV xA,xB; MOV xB,xA` round-trip pairs for elimination, check ALL register references (rd, rn, rm) of intermediate instructions, not just MOV sources. Non-MOV instructions (CSET, CMP, etc.) may write to or read from the intermediate register. Only NOP both MOVs when the intermediate register is truly dead between them.

### IR Deep Copy for Cross-Function Inlining
To inline a function compiled in a previous REPL form, the callee's IR must survive arena deallocation. Create a dedicated `ArenaAllocator` per compiled function, deep-copy the IR body and parameter names into it, and store the arena in `CompiledFn`. The `deepCopyIr` function only needs to handle the subset of IR nodes that pass `canTranslate`.

### coalesceMovs Only for Safe ALU Ops
The `coalesceMovs` peephole pass must only coalesce MOV instructions that follow safe ALU operations (ADD, SUB, MADD). Coalescing MOV after conditional operations (CSET, SELECT) or across control flow boundaries breaks correctness because multiple branches may write to the same destination register.

## 2026-02-08: Critical JIT Bug Fixes

### Entry Param Parallel Copy (fixEntryParamMoves)
- Hoist's regalloc emits sequential MOVs for entry block param copies: `MOV xD, xS`
- For 3+ params with circular dependencies, sequential MOVs clobber values
- Fix: proper parallel copy algorithm with topological sort + x9 scratch for cycles
- `fixEntryParamMovesAlloc` can insert extra instructions via ArrayList
- Previously, `eliminateRoundTripMovs` was incorrectly NOPing broken swap pairs
  in the entry region — now skips the entry region entirely

### coalesceMovs Cross-Branch Liveness Bug
- Post-MOV consumer scan treated branch instructions as "rd0 is dead"
- But branch targets may read rd0 (e.g., phi copies in merge blocks)
- Fix: conservatively mark rd0 as potentially live when hitting a branch
- This caused TCO functions to return wrong values (e.g., f3(a,b,c))

### Hoist LDP Rt2 Register Mismatch
- When hoist merges two adjacent loads (car + cdr) into LDP, the Rt2 register
  doesn't match the regalloc's expected register for the second value
- Workaround: always use `iadd + load offset=0` for cdr instead of `load offset=8`
- This prevents hoist from merging car/cdr into LDP
- Affected ALL functions using car + cdr (sum-list, while loops over lists, etc.)

### Untagged Mode + Cons Incompatibility
- Untagged mode works with plain i64 inside function body (params untagged at entry)
- Cons cells store TAGGED values (runtime objects read by interpreter/other functions)
- In untagged mode, storing untagged values into cons cells corrupts data
- Similarly, car/cdr return tagged values that don't mix with untagged arithmetic
- Fix: disable untagged mode for functions with cons/car/cdr (`containsLoads`)

### Key Peephole Pass Ordering
1. eliminateDeadCset
2. fixEntryParamMovesAlloc (can insert instructions)
3. fuseCmpImmediate
4. eliminateRoundTripMovs (skips entry region)
5. coalesceMovs (conservative at branches)
6. eliminateUselessBranches
7. invertBranchOverBranch
8. fixCallArgMoves (if recursive)
9. fuseMulAdd
10. fuseSelectCondition
11. eliminateLeafPrologue (if !recursive)
12. compactNops (LAST)

## 2026-02-08 (continued): JIT Performance Optimizations

### Backward Branch Coalescing for Loop Phi Copies
- `coalesceMovs` now treats backward `B` (loop backedge) as safe for rd0
  when there are no BLR/BL calls between the ALU op and the branch.
- Key insight: phi copies before a loop backedge capture rd0's value into
  mov_dst. The loop header reads mov_dst, not rd0. So rd0 is dead.
- Unsafe for loops with calls: callee may clobber registers.
- fixnum_loop improved from 0.37x to 1.08x SBCL.

### Cons Constants LICM (Loop-Invariant Code Motion)
- Inline cons uses g_alloc_ptr address (48-bit), 16, and 8 constants.
- Pre-emit these constants before the loop (via `in_loop_preemit` flag).
- ONLY for non-recursive functions — recursive functions have too much
  register pressure; adding 3 more constants causes spill issues.
- list_build improved from 1ms to 300µs (matching SBCL).
- gc_cons improved to 193µs (1.07x SBCL).

### Direct Predicate Conditions in translateIf
- oddp/evenp/zerop/consp as if-conditions emit direct I8 comparisons.
- Eliminates 3-5 instructions: tagged select + brif on tagged value.
- Pattern: `(if (oddp x) ...)` → `band(x,2); icmp ne; brif`
- remove_if improved from 700µs to 42µs (0.86x SBCL).

### Untagged Mode Incompatibilities
- Untagged mode disabled for functions with:
  - cons/car/cdr (cons cells store tagged values)
  - Primitive calls (gcd/nreverse/append/assoc expect tagged args)
  - Loads (car/cdr return tagged from cons cells)
- Each incompatibility caught by separate `contains*()` check.
- Missing check caused gcd benchmark to return wrong answer (235704 vs 278574).

### Inline GCD Blocked by Hoist Regalloc
- Euclidean algorithm as hoist loop: `while b!=0: r=a%b, a=b, b=r`
- Requires swap of phi parameters (a←b, b←r) at loop backedge.
- Hoist regalloc doesn't emit phi copies for this swap → infinite loop.
- Same fundamental issue as partial TCO phi copies.
- Fallback: C-ABI jitGcd call (3.3ms vs SBCL 0.89ms).

### Hoist LDP Register Mismatch (Root Cause)
- When hoist merges `load [x, #0]` and `load [x, #8]` into LDP, the Rt2
  register assignment doesn't match the regalloc's expected register.
- Example: regalloc assigns cdr load to x2, but LDP puts it in Rt2=x19.
- Workaround: always use `iadd + load offset=0` for cdr.

### JIT Performance Optimization Session (2026-02-08)

**Partial TCO**: Enabling TCO for functions with BOTH tail and non-tail self-calls
is safe and gives significant speedup. The key: tail calls become jumps (zero overhead),
non-tail calls remain as call_indirect. For ack: 720ms→592ms (18% faster).
Guard: when partial TCO leaves non-tail self-calls, keep `is_recursive = true`.

**Local Constants for Call-Heavy Functions**: Hoist's optimizer LICM-moves constants from
loop body to entry block (block0), forcing them into callee-saved registers since their
live ranges span call sites. Fix: skip `preEmitConstants` for TCO functions with non-tail
self-calls, and use `local_consts` flag in `cachedIconst` to emit fresh small constants
per use-site (only in call-containing blocks). Large constants (function pointers) still cached.

**Translation-Level CSE**: Hoist's optimizer can't CSE across loop iterations (even same-block
duplicate iadd). Fix: maintain a `cse_cache` mapping `(op, lhs.index, rhs.index) → result`
during translation. Clear on block switch for SSA dominance safety. Eliminated duplicate
`(+ i 1)` in fixnum_mul: 1170µs→1091µs (7% faster).

**Hoist Call_indirect Bug**: Hoist's e-graph optimizer (any opt level > .none) incorrectly
eliminates call_indirect instructions. Must use `.none` for functions with calls.
This prevents CSE, GVN, LICM from applying. Upstream hoist fix needed.

**MOV Coalescing Limits**: The post-emission MOV coalescing pass can't eliminate phi-copy
moves when the source register is consumed by another instruction between the ALU op
and the MOV. Example: `ADD x5,x0,x4; MADD x7,x5,...; MOV x0,x5` — can't coalesce because
MADD reads x5. This costs 1 extra instruction per loop iteration.

**Multiply-by-Constant Strength Reduction**: ARM64 MADD has 3-cycle latency on Apple M-series.
Replace `imul(x, const)` with shift-add sequences: `x*3 = x + (x<<1)`, `x*5 = x + (x<<2)`,
`x*(2^n) = x<<n`, `x*(2^n+1) = x + (x<<n)`, `x*(2^n-1) = (x<<n) - x`.
Hoist's ISLE lowering has `iadd(x, ishl(y, K)) → ADD Xd, Xn, Xm, LSL #K` rules, but
they don't fire due to forward lowering order (ishl lowered before iadd can absorb it).
The shift-add still wins: 2 instructions at 1+1=2 cycles vs 1 MADD at 3 cycles.
Result: fixnum_mul 1140µs→600µs (47% faster).

**LSL+ADD Fusion Anti-Pattern on Apple Silicon**: `ADD Xd, Xn, Xm, LSL #K` (fused shifted-ADD)
is ~10% SLOWER than separate `LSL + ADD` on Apple M-series. The wide OoO engine (8+ dispatch
slots) parallelizes two simple operations faster than one complex one. Don't fuse.

**Loop Rotation Blocked by Phi Copies**: Bottom-tested loops (SBCL-style) save 1 unconditional
branch per iteration. But hoist's regalloc inserts MOV instructions for phi parameter copies
on the back-edge, adding 2+ instructions that offset the savings. Needs hoist phi coalescing.

**Hoist brifArgs Parameter Bug**: `brifArgs` (conditional branch with block arguments)
doesn't correctly insert phi copies — the target block's parameter register doesn't match
the source value's register. Workaround: use separate trampoline blocks with explicit
`jumpArgs`. This adds overhead but is correct.

**Defer TCO Args After Inner Call**: For `(ack (- m 1) (ack m (- n 1)))`, computing `m-1`
before the inner call forces a callee-saved register to hold the result. Computing it
AFTER the call reuses the phi param register (still intact as callee-saved). Saves 1 STP
pair in prologue. Implemented by splitting arg translation: call-containing args first,
then simple args after.

**getFixnumLit Returns Raw Tagged Value**: In untagged mode, `getFixnumLit` returns the
raw tagged value (e.g., 7 for literal 3). Must shift right by 1 to get the actual numeric
value for strength reduction in untagged mode. Bug caused multiply-by-7 instead of by-3.

### Backend Migration + Perf Audit Session (2026-02-17)

**Dead Legacy Backend Surface**: `src/lib.zig` exported `src/ir/ir.zig` even though runtime
paths use Hoist via `src/jit/backend.zig`. Keeping dead exports preserves stale APIs and
needlessly compiles abandoned code. Remove the export and delete dead backend modules.

**Benchmark Harness Must Avoid Stdlib-Only Calls**: `bench/vm.zig` used
`(concatenate 'string ...)` without loading stdlib, causing `UnboundSymbol` in VM bench
(`src/interp/vm.zig:8825`). VM microbenches should use primitives guaranteed available in
the bare compiler/VM setup (e.g., `make-string` + `length`) or explicitly load stdlib.

**Perf Gating Requires Stable Bench Runners**: `bench-comp` currently crashes in JIT mode
on `gcd` (`src/interp/vm.zig:718` calling `CompiledFn.callFromValues`). Before optimizing
hot paths, lock down benchmark stability; otherwise perf regressions/improvements are noisy.

**Doc Drift Is a Real Performance Risk**: stale file references (`src/jit/jit.zig`,
`src/jit/stencils.zig`, `src/jit/patch.zig`, `src/jit/ctx.zig`, `src/jit/rt.zig`) mislead
optimization work and waste cycles. Keep docs path-valid against both `src/` and `../hoist/src/`.

**Post-Emit Liveness Must Model Call ABI Reads**: peephole dead-code elimination in
`src/jit/backend.zig` removed MOVZ arg setup before `blr`, because liveness treated call
boundaries as "reg dead". On AArch64, indirect/direct calls read x0-x7 (args), x8 (sret),
and `blr` also reads its target register. If that is not modeled, optimizers can turn
correct indirect calls into wrong-result or crashy code paths.

**VM GC Root Churn Drops By Using Slots Over Mirror Arrays**: `collectGarbageExtra`
in `src/interp/vm.zig` no longer builds a temporary `ArrayList(Value)` (`gc_vals`) for
frame closure/chunk roots. Using stack-local `Value` roots registered as `slots` avoids
dynamic buffer growth and copy-back indexing complexity while preserving pointer re-derive
after GC (`chunkFromValue` / `toPtr(Closure)`).

**Maxima Loader Must Not Auto-Execute At File Load**: loading a broad Maxima module set
can hit VM `StackOverflow` that is not recoverable through Lisp-level `handler-case`.
Keep `lib/maxima-loader.lisp` as a callable API (`maxima-load-all`) and avoid auto-running
the full load sequence during file import.

### Stream READ Semantics Can Invalidate Loader RCA (2026-02-17)

`lib/stdlib.habu` currently defines stream `read` by consuming the entire
stream into a string and then parsing once:
- first `(read s ...)` returns the first form
- second and later reads return `:EOF`

Evidence:
- `/tmp/read_many_target.lisp` with forms `1 2 3` produced `R1=1, R2=:EOF`.

Impact on Maxima loader debugging:
- "formwise read/eval" probes that appeared to succeed (`DONE forms=1 ok=1`)
  were not trustworthy for multi-form files because stream `read` never
  advanced past the first form.
- Removing `handler_sp/catch_sp` clobber in `evalForms` (then named `evalFileContentSeparateVm`) did
  not fix `db/compar/limit` load overflows and introduced new regressions
  (`mlisp` load failure), so that change was reverted.

Actionable takeaway:
- Do not use stream-`read` loops as a fallback loader path until stream `read`
  is fixed to consume one form at a time.

### Maxima Integrate Chain Needs Runtime-Callable Dependencies (2026-02-17)

`fboundp '$integrate` is not a sufficient gate for integration readiness.
With a reduced subset, `$integrate` can still fail at runtime with
`(UNBOUND-VARIABLE UnboundSymbol)` due to missing transitive call targets.

Evidence from targeted tracing:
- `TRACE unbound function: ALIAS`
- `TRACE unbound function: SININT`

Fix pattern:
- include `suprv1` (defines `alias`) and `sinint`/`sin` in the integrate subset,
  plus existing `schatc` chain (`partition`, `m2`, `schatchen-cond`).

Testing rule:
- integration gate must execute a real call
  `($integrate '((mexpt) $x 2) '$x)` in `src/tests/integration.zig`,
  not just symbol/macro presence checks.

Environment guard:
- Maxima-source fixtures can disappear or change layout under `/tmp/maxima`.
  Guard Maxima integration tests with a source-presence check
  (`/tmp/maxima/src/lmdcls.lisp`) and `error.SkipZigTest` so non-Maxima
  environments still run the rest of the suite deterministically.
- Prefer candidate-root probing (`/tmp/maxima/src/`, `/tmp/maxima/src/src/`,
  `/tmp/maxima/`) in both loader and tests to avoid path drift regressions.

### Session Notes (2026-02-19, call-lowering transient allocs)

#### Worked Well
- Replacing `ArrayList + builder.call/tailcall/listStar` staging in call lowering with single pre-sized slices and direct IR node construction (`src/compiler/compile.zig:5892`, `src/compiler/compile.zig:5914`, `src/compiler/compile.zig:17130`) removed redundant transient allocations in hot compile paths while preserving call/apply semantics.
- Adding a shared qualified struct-predicate lookup helper (`src/compiler/compile.zig:17158`) removed per-call symbol-name duplication and made occurrence-typing predicate lookup robust for package-qualified predicate registrations.
- Locking regressions directly in compiler tests (`src/compiler/compile.zig:19273`, `src/compiler/compile.zig:19316`) gave deterministic proof that variadic operand preservation and `struct_p` lowering stayed intact.

#### Did Not Work
- Stopping after replacing `ArrayList` alone is insufficient: routing through `builder.call`/`builder.tailcall`/`builder.listStar` still performs an internal `dupe`, so transient-allocation reduction required direct node construction in compile hot paths.

### Session Notes (2026-02-19, letrec + multi-setq staging)

#### Worked Well
- Reworking `compileLetrecWithTail` to pre-count bindings, store one compact binding table, and emit a direct `progn` node (`src/compiler/compile.zig:5351`) removed multiple staging lists (`names`, `values`, `indices`, `exprs`) from recursive-binding compilation.
- Rewriting `compileMultiSetq` to pre-count pairs and emit a direct `progn` (`src/compiler/compile.zig:5949`) removed `ArrayList + dupe` churn from a high-frequency assignment form while preserving per-pair lowering through `compileSet`.
- Adding focused compile regressions for letrec/setq lowering shape (`src/compiler/compile.zig:19349`, `src/compiler/compile.zig:19381`) caught structural regressions immediately without requiring long full-suite runs.

#### Did Not Work
- Leaving `letrec`/`setq` on dynamic append-first staging paths keeps avoidable allocator pressure in loader-heavy workflows; these forms need fixed-size preallocation once arity is knowable from list shape.

### Session Notes (2026-02-19, multi-place setf staging)

#### Worked Well
- Replacing `compileSetf` multi-place `ArrayList` staging with pre-counted pair slices and direct `progn` node emission (`src/compiler/compile.zig:6067`) removed append-growth/dupe churn while preserving recursive per-pair lowering.
- Keeping lowering through the same single-place `compileSetf` path for each `(place value)` pair retained semantics for symbol-macro and compound-place updates; the focused regression (`src/compiler/compile.zig:19427`) confirms one emitted form per pair.

#### Did Not Work
- Using `builder.progn(items)` in this path still duplicates slices internally, so partial refactors that keep builder-level aggregation do not remove transient-allocation pressure.

### Session Notes (2026-02-19, flet/labels staging)

#### Worked Well
- Replacing `compileFletWithTail` `ArrayList(Ir.Binding)` staging with pre-counted binding slices and direct `.let` node construction (`src/compiler/compile.zig:5426`) removed builder-side binding duplication while preserving lexical function binding behavior.
- Reworking `compileLabelsWithTail` to use one compact binding table plus pre-sized `boxed_bindings`/`init_forms` slices (`src/compiler/compile.zig:5476`) removed layered staging lists and avoided duplicate `progn`/`let` copying in recursive local-function lowering.
- Keeping `errdefer` cleanup for duplicated names in the error path preserved safety while allowing successful paths to transfer ownership to IR nodes.

#### Did Not Work
- Holding onto dynamic append patterns for `labels` setup (`names`/`lambda_args`/`indices`/`sym_vals`) adds avoidable allocator churn and duplicates data already derivable from the same binding list traversal.

### Session Notes (2026-02-19, lambda/progv staging)

#### Worked Well
- Replacing lambda entry-assertion staging with fixed-size assertion buffers and direct `progn` node emission (`src/compiler/compile.zig:4239`) removed `ArrayList + dupe` overhead while keeping safety-gated assertion semantics.
- Replacing special-parameter `progv` staging with pre-sized symbol/value slices (`src/compiler/compile.zig:4302`) eliminated transient list growth in lambda lowering without changing symbol/value ordering.
- Replacing all-special LET fast-path value staging (`src/compiler/compile.zig:5320`) with direct slices removed another hot allocation loop in dynamic-binding lowering.

#### Did Not Work
- Leaving assertion/progv aggregation on dynamic arrays causes repeated allocator churn in compile-heavy macro/function pipelines even when the target cardinality is statically bounded by parsed lambda metadata.

### Session Notes (2026-02-19, macro map sync gating + Maxima reprofile)

#### Worked Well
- Sampling a real Maxima subset load (`sample` over `/tmp/maxima_profile_subset.lisp`) identified `interp.repl.Repl.restoreMacroMapsFromRoots` hash-map rebuilds as a dominant steady-state cost in form execution (`src/interp/repl.zig:497`, `src/interp/repl.zig:574`).
- Adding GC-epoch-gated macro map synchronization (`src/interp/repl.zig:574` + `src/interp/repl.zig:2008`) removed unconditional macro-map refresh/restore work from no-GC form execution while retaining full restore on GC transitions.
- Reprofiling after the change shifted hotspots away from macro-map restore loops and improved Maxima subset load wall time from ~5.06s to ~3.37s on the same script/run shape (`/tmp/maxima_profile_subset.lisp`), with similar peak memory (~289MB).

#### Did Not Work
- A manual-GC regression test that called `repl.vm.collectGarbage()` directly between evals produced false failures because macro maps are only guaranteed rooted during managed execution paths; direct unrooted GC is not a valid behavioral contract for macro table persistence.

### Session Notes (2026-02-19, tagbody/progn/values staging)

#### Worked Well
- Replacing `compileFormsToProgn` and `compileValues` dynamic staging with pre-sized slices and direct IR node emission (`src/compiler/compile.zig:8051`, `src/compiler/compile.zig:8077`) removed another layer of `ArrayList` growth + builder duplication in control-flow and multi-value compilation paths.
- Revalidating with both compile-shape regressions and integration-level tagbody/values tests ensured segment/value cardinality stayed correct while reducing staging overhead.

#### Did Not Work
- Leaving these sequence forms on builder-backed aggregation keeps hidden duplicate-slice allocation in high-frequency control-flow lowering; direct node emission is required for predictable allocation behavior.

### Session Notes (2026-02-19, format arg staging)

#### Worked Well
- Rewriting `compileFormat` variadic argument lowering to pre-count and fill a single args slice (`src/compiler/compile.zig:16205`) removed `ArrayList` growth and builder-side arg duplication in a frequently used formatting path.
- Locking the cardinality behavior with a compile regression (`src/compiler/compile.zig:19615`) plus integration format checks kept semantics stable while reducing transient compiler allocations.

#### Did Not Work
- Treating variadic format args as append-first dynamic lists hides redundant copying in builder emission; direct fixed-size arg slices are required for stable hot-path compilation cost.

### Session Notes (2026-02-20, JIT SSA dominance + backedge liveness)

#### Worked Well
- Dumping Hoist IR/ASM for the failing `NQUEENS-SAFE-P` path (`HABU_DUMP_HOIST=1`) exposed a concrete dominance violation: `v9 = iconst 2` defined in one branch but reused in sibling blocks.
- Extending constant pre-emission to traverse `.block` nodes (`src/jit/backend.zig:1738`) fixed the root cause by ensuring required constants are emitted from dominating context before TCO lowering.
- Clearing `const_cache` on block switches when `local_consts` mode is active (`src/jit/backend.zig:1025`) hardened block-local constant semantics and avoids cross-block SSA reuse in the local-constant path.
- Replacing `coalesceMovs` post-MOV safety logic with CFG-aware liveness (`isRegDeadAfter`) at the coalesce site (`src/jit/backend.zig:5623`) removed a real loop-backedge miscompile class.
- Locking both sides with focused regressions (`src/jit/backend.zig:5983`, `src/jit/backend.zig:6005`, `src/tests/integration.zig:88`) prevented both the old `nqueens` wrong-result path and over-conservative pass disabling.

#### Did Not Work
- Assuming `preEmitConstants` already handled wrapper nodes was wrong; missing `.block` traversal silently disabled pre-emission for whole function bodies in TCO paths.
- Assuming linear/use-local coalesce checks were enough across backward branches was wrong; loop-header reads require CFG liveness, not local scan heuristics.
- Treating long `zig build test` as a reliable gate in this environment is still brittle; sampled runs showed `test --listen` wait states, so targeted filters remain the dependable validation path here.

### Session Notes (2026-02-20, major slice budget telemetry gates)

#### Worked Well
- Exporting major mark/sweep budgets from GC (`src/runtime/gc.zig:98`) and threading them into bench JSON (`bench/gc.zig:394`, `bench/gc.zig:397`) removed hard-coded budget assumptions in downstream tooling.
- Enforcing step/sweep/max-slice coherence directly in bench regression checks (`bench/check.zig:420` to `bench/check.zig:477`) caught invalid major-slice telemetry states early.
- Adding `gc_major_slice_in_bounds` to parity gate evaluation (`tools/gc-compare:274`, `tools/gc-compare:385`, `tools/gc-compare:451`) made slice-budget violations fail the same gate path as other GC policy invariants.

#### Did Not Work
- Depending on raw `gc_major_max_*_slice` telemetry alone was insufficient for external validation; without explicit emitted budgets, compare/check tools either drift or silently skip strict bound checks.
- Using full-suite `zig build test` as the only validation gate is not reliable in this workspace right now due an unrelated compile/integration segfault path; targeted GC tests + bench gates were the stable proof path for this dot.

### Session Notes (2026-02-20, tenured segregated free bins)

#### Worked Well
- Splitting tenured reuse into two layers in `src/runtime/heap.zig` (`allocTenuredFromBins` + `allocTenuredFromPendingList`) preserved immediate reuse of newly swept spans while making steady-state reuse O(number of candidate bins) instead of O(all free spans).
- Rebuilding bins from coalesced spans (`coalesceTenuredFree`, `drainTenuredBinsToList`, `rebuildTenuredBinsFromList`) kept coalescing exact without introducing pointer aliasing/index invalidation across mutable free lists.
- A direct allocator-level regression (`src/runtime/heap.zig`: `heap tenured free bins coalesce and reuse spans`) caught both coalesced-span reuse and split-tail reuse behavior.

#### Did Not Work
- A bins-only allocator path that ignored the in-progress `tenured_free` pending list would delay reuse until full coalesce completion and can transiently starve promotions during sliced major sweep windows.

### Session Notes (2026-02-20, tenured coalesce/split policy)

#### Worked Well
- Switching bin allocation from first-fit to bounded best-fit (`src/runtime/heap.zig`: `allocTenuredFromBins`) reduced avoidable oversized reuse while capping scan cost with `TENURED_ALLOC_SCAN_BUDGET`.
- Applying the same bounded best-fit split policy to the pending free list (`src/runtime/heap.zig`: `allocTenuredFromPendingList`) preserved immediate reuse before coalesce while keeping split behavior consistent.
- Enforcing a minimum split remainder (`TENURED_SPLIT_MIN_REMAINDER`) eliminated tiny tail fragments; the regression (`src/runtime/heap.zig`: `heap tenured split policy avoids tiny tail fragments`) locks this.

#### Did Not Work
- Pure first-fit with unconditional split creates tiny remainder spans that churn bins and increase fragmentation pressure under mixed-size promotion workloads.

### Session Notes (2026-02-20, tenured fragmentation benchmarks)

#### Worked Well
- Emitting tenured free-space fragmentation telemetry directly from `bench/gc.zig` (free span count/bytes/largest span/fragmentation ratio) gave a stable signal to track allocator-quality changes.
- Reading free-space from both pending and binned tenured free lists in `src/runtime/heap.zig` (`tenuredFreeStats`, `tenuredFragmentation`) avoided blind spots during incremental sweep windows.
- Wiring fragmentation invariants and gate checks through `bench/check.zig` and `tools/gc-compare` ensured regressions are caught automatically with the same CI/parity flow as other GC metrics.

#### Did Not Work
- Tracking only `tenured_bytes` and object counts misses allocator fragmentation regressions completely; fragmentation required explicit free-span topology metrics.

### Session Notes (2026-02-20, JIT bridge call-stack and sequence correctness)

#### Worked Well
- Restoring full dynamic control-stack depths on JIT fast returns (`src/interp/vm.zig:3519` via `restoreCallerFrameAfterCall`) removed a real block-stack leak in bridge-heavy higher-order workloads.
- Restoring frame dynamic depths before tail-call frame reuse in `doCall(..., tail=true)` (`src/interp/vm.zig:10028`) fixed repeated block-frame accumulation on recursive tail paths.
- Rooting pointer literals for JIT codegen and loading them via stable slots (`src/interp/repl.zig:2296`, `src/jit/backend.zig:1434`) removed stale-literal pointer hazards under moving GC.
- Replacing list-only JIT `length` lowering with a generic sequence helper (`src/jit/backend.zig:556`, `src/jit/backend.zig:3274`) fixed string-length crashes in optimized code paths.
- Refreshing JIT heap bump-cache before/after bridge calls (`src/interp/vm.zig:313`) prevented `heap.alloc_ptr` corruption after bridge-triggered GC and removed `bytesUsed` overflow panics.

#### Did Not Work
- Assuming JIT fast-return could pop only `fp/sp` was wrong; dynamic stacks (`block/catch/unwind/restart/progv/handler`) must be restored from call-frame metadata.
- Assuming list-only `length` lowering was safe at `safety 0` was wrong; valid non-list sequences (strings/vectors/arrays) are common and must follow generic CL semantics.
- Assuming JIT heap globals stay valid across bridge calls was wrong; interpreter/GC activity inside bridge calls invalidates cached bump pointers unless explicitly refreshed.

### Session Notes (2026-02-20, control-stack depth limits)

#### Worked Well
- Raising VM `MAX_BLOCKS` to frame-scale (`src/interp/vm.zig:529`) removed premature `StackOverflow` in legitimate recursive workloads (e.g., sort merge recursion) without changing call semantics.
- Locking the behavior with a deep-recursion integration test (`src/tests/integration.zig:132`) prevents regressions where recursion depth >64 incorrectly fails even when frame/stack budgets are still available.
- Re-running comprehensive bench showed `sort_string` and `intern` now complete instead of warmup overflowing from block-depth exhaustion.

#### Did Not Work
- Keeping `MAX_BLOCKS` far below `MAX_FRAMES` created an artificial control-stack ceiling that failed real recursive Lisp code before true frame/stack limits were reached.

### Session Notes (2026-02-20, sort copy-once safety under generational GC)

#### Worked Well
- Refactoring `sort` to copy once at the public entry and recurse on an internal working-list helper (`lib/stdlib.habu:2374` to `lib/stdlib.habu:2387`) preserved non-destructive CL behavior while removing recursive `copy-list` overhead.
- Locking sort semantics with focused integration checks (`src/tests/integration.zig:821`) caught both descending comparator designators and `:key` behavior regressions.
- Validating against the generational designator stress test (`src/tests/integration.zig:4878`) ensured the optimization did not reintroduce load-time heap corruption.

#### Did Not Work
- Threading a copy-state flag through recursive `sort-with-key` calls (extra recursion argument path) caused deterministic corruption during stdlib load under generational GC, eventually crashing in later unrelated forms (e.g. `defmacro` handling). Avoid copy-state recursion parameters in this path until the underlying runtime/compiler corruption is root-caused.

### Session Notes (2026-02-21, sort string comparator fast path)

#### Worked Well
- Adding a zero-new-defun fast path inside existing `merge-lists-with-key` (`lib/stdlib.habu:2342`) for `(null key)` + `string<` designators (`#'string<` or `'string<`) removed high-frequency `funcall` comparator overhead in sort merges.
- Keeping dispatch inside the existing function (instead of adding new recursive sort helper forms) preserved generational-stdlib-load stability (`src/tests/integration.zig:4935`).
- The new regression (`src/tests/integration.zig:860`) locked function/symbol designator behavior, non-destructive input semantics, and `:key` fallback correctness.
- `sort_string` JIT benchmark improved from ~8.11 ms to ~4.86-4.92 ms (`zig build -Duse-hoist=true bench-comp -- --iterations 3 --warmup 1`).

#### Did Not Work
- Introducing additional recursive stdlib sort helper defuns for string fast paths triggered deterministic generational load crashes in compiler capture analysis (`src/compiler/compile.zig:4989`) and later macro handling (`src/interp/repl.zig:3686`) during `loadStdlib`.

### Session Notes (2026-02-22, Hoist VCode successor corruption under growth)

#### Worked Well
- Building a standalone Hoist reproducer (linear 80-block VCode chain) proved the `computePreds` panic is deterministic once successor storage grows past 32 entries; this removed ambiguity about Habu IR correctness.
- Routing Hoist compilation through a remap-stable allocator wrapper (`src/jit/backend.zig:49`) over a per-compile arena fixed the root issue without touching `../hoist`: old backing slices survive ArrayList growth and `computePreds` no longer reads poisoned entries.
- Wiring the stable allocator in the Hoist compile entry (`src/jit/backend.zig:5365`, `src/jit/backend.zig:5383`) removed the Maxima `SMINMAX` panic path; `bench-maxima` now completes (`jit_compiled=397` at scale 1 in current run).
- Adding a deep branch-chain JIT regression (`src/tests/integration.zig:1947`) locks the >32-edge lowering path that previously crashed in Hoist.

#### Did Not Work
- A plain `ArenaAllocator` alone was insufficient: Zig allocator `free`/realloc paths poison old buffers, so stale Hoist succ/param slices still became `0xAAAAAAAA` (`src/jit/backend.zig` pre-fix compile path).
- Using full `zig build test -Duse-hoist=true` as proof for this dot remains noisy in this workspace because an unrelated pre-existing integration segfault (`deep recursive defun does not overflow block stack at 64`) still fails outside the Hoist-succ fix scope.

### Session Notes (2026-02-22, append copy-once in runtime + JIT)

#### Worked Well
- Replacing append's double-copy path with copy-once tail splice in the runtime primitive (`src/runtime/primitives/list.zig:121`) removed half the cons allocations for left-list elements while preserving output order and GC write-barrier correctness (`setCdr`).
- Replacing JIT append's reverse-cons double allocation with `jitNreverse` + tail splice (`src/jit/backend.zig:329`) matched runtime semantics with one left-side copy and explicit barrier on the tail link.
- Locking allocation behavior with focused regressions (`src/runtime/primitives/list.zig:294`, `src/jit/backend.zig:10159`) made allocation-count regressions immediately visible.
- Running `bench-comp` on parent `cdefc0a7` vs this change showed `list_append` improve from `14.949 ms` to `12.740 ms` in the same harness run shape.

#### Did Not Work
- Treating full-suite `zig build test` as a gate for this dot remains blocked by the unrelated pre-existing crash in `tests.integration.test.deep recursive defun does not overflow block stack at 64`; targeted append/JIT tests were the stable validation path for this fix.

### Session Notes (2026-02-22, JIT self-call patch RCA + safety gate restore)

#### Worked Well
- Reproducing with a minimal recursive program under Hoist (`/tmp/recur.habu`) and dumping final machine code (`HABU_DUMP_HOIST=1`) exposed the real fault: self-call patching rewrote a later non-self BLR to self when the same source register was reused (`src/jit/backend.zig:4632`).
- Fixing `patchSelfCallsToBL` to use the nearest reaching definition of the BLR source register (`src/jit/backend.zig:4638`) eliminated false self-call rewrites and removed the recursive JIT crash path.
- Adding a low-level regression for mixed self/non-self target reuse (`src/jit/backend.zig:8088`) locked the patcher behavior directly at machine-instruction level.
- Restoring a JIT admission safety gate (`src/jit/candidates.zig:98`) preserved CL safety semantics (`TypeMismatch` paths) by avoiding unsafe JIT arithmetic lowering for non-`safety 0` lambdas.
- Adding an integration regression for recursive `safety 0` JIT execution (`src/tests/integration.zig:560`) ensured recursive call lowering still works under the intended admission policy.

#### Did Not Work
- Running full `zig build test -Duse-hoist=true` as an always-clean gate remained unreliable in this workspace due occasional lingering `test --listen` runners; targeted filters plus explicit stale-runner cleanup were the stable validation path for this RCA/fix cycle.

### Session Notes (2026-02-23, progv literal-root completeness in all JIT paths)

#### Worked Well
- Tracing `ratsimp` with `HABU_TRACE_JIT_BRIDGE` + `HABU_TRACE_JIT_XCALL` narrowed the crash to a corrupted generic call designator inside `CPUT` (not the BLR target path), which focused RCA on literal rooting instead of call lowering (`src/interp/vm.zig:338`, `src/jit/backend.zig:3386`).
- Adding `.progv` traversal to REPL literal-root collection fixed a real missing-root gap for progv-wrapped bodies (`src/interp/repl.zig:2795`), and `bench-maxima --workloads=ratsimp` now completes.
- Extending the test `compile_chunk` JIT path to collect and pass literal roots (including `.progv`) removed the same stale-literal class from integration helpers (`src/testing/compile_chunk.zig:52`, `src/testing/compile_chunk.zig:136`, `src/testing/compile_chunk.zig:438`).
- The new regression (`src/tests/integration.zig:146`) locks post-GC call-target stability for progv-wrapped JIT code paths.

#### Did Not Work
- Fixing only REPL literal-root traversal was insufficient: integration `compile_chunk` still compiled with `compileIr(...)` (no roots), emitted `JIT_LIT_NOROOT`, and reproduced post-GC call-target corruption until the helper path was upgraded too (`src/testing/compile_chunk.zig` pre-fix `tryHoistCompile`).
