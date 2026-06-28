---
title: "Sound content-hash artifact cache: build engine once per gate"
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-28T09:51:09.926533+02:00\""
---

GOAL: cut the native port gate from ~96s toward the ~62s idle floor (proven achievable) by building the engine ONCE and reusing it, the way the user directed ('pre-build hb once, reuse for tests, no rebuilding').

MEASURED (this 4-core Orin, slots=1 isolated per-phase):
- Total work ~250 core-sec => ~62s floor on 4 cores at idle (matches the user's 60s estimate).
- The engine is COMPILED 5x per run with NO content-hash caching: engine-build fixpoint 24.8s + AOT-pos 24s + AOT-neg 12.4s + warm-tools bake 13.8s + warm-checker bake 12.2s = ~87s.
- All caches key by PATH/EXISTENCE only, NOT content: HBB-MAKER-READY? (tools/hb-build-lib.f:372) reuses the maker if the file merely EXISTS; SUITE-WARM-CACHED? (test/gate-stdlib.f:140) checks image existence only. They stay correct ONLY because the cache lives under GT-ROOT (a fresh per-run temp), so it never persists -> every run recompiles from scratch.
- Therefore making any cache persistent across runs is UNSOUND as-is: a stale maker/warm image would mask a source change and green a broken gate (violates master-always-green).

SOUND DESIGN (build once, reuse, content-keyed):
1. Persistent cache base (stable path, e.g. <tmp>/hb-gate-cache), NOT GT-ROOT.
2. Key each artifact by sha256(bin/hb + EXACTLY the source files that produced it). Use SHA256-FILE-HEX (src/core/sha256.f / tools/sha256-file*). Store at base/<key>/. A hit is valid BY CONSTRUCTION (hash == inputs); any input change -> new key -> miss -> rebuild. No separate validation, no stale reuse.
3. Refactor SUITE-WARM-SUPPORT-ARGV (gate-stdlib.f) + the gate-diagnostics warm list into a DATA TABLE iterated by BOTH the arg-builder AND the hasher, so the key provably covers exactly what is baked.
4. Concurrency (Codex/parallel agents share the machine): bake to a temp dir, atomic-rename into base/<key>/. Reuse the maker-lock pattern (HBB-TRY-MAKER-LOCK?).
5. Apply to: warm-tools image, warm-checker image, the AOT maker (hb-aot-mk), and consider the fixpoint stage. For an unchanged-src change (e.g. tools/ptx+docs), ALL become cache hits => gate work ~163 core-sec => ~41-50s.

VERIFY: (a) 2nd run with unchanged inputs -> warm/AOT phases near-instant (cache hit), gate < 90s with margin; (b) touch one baked source -> that artifact's key changes -> it rebakes (prove with a minimal edit + the phase re-running); (c) full gate stays green, all 18 slices pass; (d) run host/filemap lints.

ALSO: the live ~96s measurements are inflated by a concurrent 'codex resume' agent (PID 288177, loadavg 8-10) saturating the shared 4-core machine; clean measurement needs that load gone or coordinated. Files: test/run.f (TR-BUILD-CACHE-ENV, HABU_GATE_WARM_ROOT wiring), test/gate-stdlib.f (SUITE-SET-ROOT/WARM-PATHS/CACHED?/WARM-RUN + support table), test/gate-diagnostics.f (checker warm), tools/hb-build-lib.f (HBB-MAKER-READY? content key).

Checkpoint 2026-06-28: `test/run.f` now keys `hb-under-test` from `bin/hb`, the
runner/build harness, all emitted engine sources, target-specific image/sys/repl
sources, and baked REPL/debug sources. With `HABU_GATE_WARM_PERSIST` set, a miss
runs the existing fixpoint build and installs the candidate under the content
key; a hit copies it into the per-run root, marks `HABU_UNDER_TEST` ready, skips
the engine-build phase, and starts under-test slices immediately. Verified miss:
80.81s wall with `candidate-miss=1 candidate-install=1`; verified hit: 63.64s
wall / 60.594s internal with `candidate-hit=1 candidate=0`. Remaining work:
content-key the AOT maker (`maker-miss=1 maker-build=1` still appears every gate)
and continue cutting the residual under-test/tool/diagnostic waves toward 30s.

Checkpoint 2026-06-28: `tools/hb-build-lib.f` now keys `hb-aot-mk`/`hb-build-mk`
by mode, `bin/hb`, the build library/helper loads, `tools/build-fixpoint.f`,
target source, common emitted engine source, `src/habu/maker.f`, and the selected
AOT/REPL driver. `test/run.f` now points `HABU_BUILD_CACHE` at
`$HABU_GATE_WARM_PERSIST/hb-build-cache` when a persistent warm root is set.
Verified first run after the key change: `maker-miss=1 maker-build=1`, 64.117s
internal / 66.92s wall. Verified second run: `maker-hit=1 maker-build=0`,
60.101s internal / 63.04s wall. Remaining work: checker warm is still per-run
(`warm-miss=1 warm-build=1`), and the residual 60s wall is dominated by the
under-test/tool/diagnostic waves, not by builder recompilation.
