---
title: Share the primitive registry and completeness gate
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.542981+03:00"
closed-at: "2026-09-29T16:00:06.917086+03:00"
close-reason: landed on master c595e06e (Alder); interdiff against the reviewed bookmark empty
---

Problem: registration (`src/habu/habu1.f:109-155 FP-ARGS`) and the completeness gate (`habu2.f ENGINE-EMIT:EMIT-PRIMITIVE-SECTIONS`) live in the ARM64 builder.
Acceptance: both live in `src/habu/primitive-registry.f`, used by `habu1.f` and by `kernel-x64.f`; the ARM64 engine byte-identical (chain).
Files: `src/habu/primitive-registry.f`, `src/habu/habu1.f`, `src/habu/habu2.f`, `src/habu/treeshake.f`.
Verify: spark: rebuild; chain gen2==gen3 byte-identical to master's engine; gate.
Depends: none. Serialise on `habu2.f` with I6, I10c and X7.
Route: Alder (shared: src/habu/primitive-registry.f, src/habu/habu1.f, src/habu/habu2.f, src/habu/treeshake.f).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-share-the-primitive-58c235e5.
Preflight corrections (these override the lines above where they differ):
- `src/habu/primitive-registry.f` already exists (package `ENGINE-PRIMS`, required at `habu1.f:61`, tested by `test/primitive-registry.f`, mirrored in `tools/build-fixpoint.f:1053`); extend it, do not create it.
- Design: package `ENGINE-PRIMS` gains the publics `SPEC-CHECK ( ptr u8 n -- )` (`FP-ARGS`'s refusal, rc 76), `HELPER-REGISTER`, `DNAME` and `HELPER-WID` (`ENGINE-HELPER`, `habu1.f:87-103`, moved; used by `code-origin.f` and `habu2.f:1301`), `GLOBAL-INT-WID` (`PRIM-GLOBAL-INT-WID`, `habu1.f:71`), and `COMPLETE ( -- )` (`PRIM-TABLE-COMPLETE` and its `PTC-*` helpers, `habu2.f:10975-11000`, with a target-neutral message). The file takes labels only as `label` values (`>LABEL`/`LABEL>N`, `src/core/roles.f:124-125`); it never calls `LBL`/`LABEL@`/`LBL,` or any mnemonic, so it loads on x86. `FPRIM`, `FPRIM-L`, `FPRIM-WID`, `GDEREF-*` and their `s" name" ['] body` call sites stay in `habu1.f` unchanged (`tools/lint/shadow-lint.f:77-84,186` extracts names from them). `habu2.f:11016` calls `ENGINE-PRIMS:COMPLETE` at the same point. `treeshake.f`: header comment only (`KEEP?`, line 44, stays).
- Acceptance: replace "used by `kernel-x64.f`" with "consumed by `habu1.f` and `habu2.f`; the named surface is what K5/K6 bind".
- Files add: `tools/build-fixpoint.f` (the `BF-APPEND-HABU1` order check only).
- Verify: `bin/hb --load test/primitive-registry.f`; rebuild with the master engine: the product built from this tree is byte-identical to the base engine (`11c00585…`; the builder loads after the capture, `tools/native-build-core.f:347-348`, so host-side moves cannot enter product bytes); the chain; gate; if the registry gains a `require`, `HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh` on spark. The Gforth mirror needs no edit (`bootstrap/cg/forth.fs:7836` has its own emitter without the gate).
- Pre-change measurement: `rg -n '^: FP-ARGS|^: PRIM-TABLE-COMPLETE' src/habu/` finds only `habu1.f:121` and `habu2.f:10995`.
