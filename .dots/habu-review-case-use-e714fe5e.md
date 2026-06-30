---
title: Review CASE use
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-29T17:53:47.268080+02:00\""
---

Problem: CASE/OF/ENDOF/ENDCASE is now supported and checked, but older source still encodes multi-arm equality dispatch and enum/opcode selection as IF/ELSE chains or manual branches. Fix: audit src/, lib/, tools/, and tests for applicable multi-arm value dispatch; replace only clear equality-dispatch cases with checked case forms, preserving control-flow and stack effects. Priority areas: src/habu/habu1.f/habu2.f emitter dispatch, src/core/checker.f token/type/render selection, lib/ptx/cg*.f opcode/render tables, test/gate-* generated source helpers, and docs examples. Acceptance: audit note records converted vs intentionally-kept sites; converted words have typed stack effects and no deeper stack juggling; case misuse regressions still pass; focused tests for touched modules plus build-fixpoint-test pass. Files: src/habu/habu1.f, src/habu/habu2.f, src/core/checker.f, lib/ptx/cg*.f, test/gate-dictionary-lib.f, docs/forth.md.

2026-06-30 local checkpoint: converted clear numeric dispatch sites to checked `case`: `src/habu/habu2.f` `PFX-LOAD?`; `src/core/checker.f` `E-COPY` and `E-INST` effect-node tag dispatch; `test/run.f` stdlib-slice membership, nested-pool dispatch, runner selection, AOT-runner selection, and phase subject selection; `test/gate-stdlib-inline-lib.f` group-mode rendering; `test/gate-stdlib-lib.f` process-outcome rendering and lint-slice selection. Kept string argv/label predicate chains (`TR-PARSE-ARG`, `SUITE-SLICE?`, label filters) because current `case` is numeric value dispatch; those should move only after a symbolized argv/token dispatch abstraction exists. Kept larger render/control-flow rewrites for dedicated checker-record/symbol dots rather than mixing semantic checker refactors into this case cleanup. Proof on macOS/aarch64: full native suite cache-fill pass 41.981s internal / 44.31s wall; hot pass 24.214s internal / 26.60s wall.
