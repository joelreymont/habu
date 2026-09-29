---
title: Return all-ones flags from x86 compares
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T16:05:17.013636+03:00"
---

Problem: a Habu flag is all ones (`1 2 < .` prints `-1`; the ARM64 emitter uses `CSETM`), but x86 `PUT-SETCC` (`src/compiler/native/emit-x64.f:474-477`) emits `setcc r8; movzx r64, r8`, which gives 0/1. It serves `PUT-CMPSET` and `PUT-CMPSETI` (482, 487). Found natively by C8 (`habu-run-emitted-x86-b704f918`): on the ThinkPad the `cmpset` and `cmpseti` peer images exit 22, because their first true case (`2 5 <`, expecting -1) gets 1.
Acceptance: every x86 compare result is 0 or -1; `PUT-SETCC` widens to all ones (for example `neg r64` after the `movzx`). Audit every other flag producer in `emit-x64.f` and `select-x64.f` for the same 0/1 assumption and fix any found. The pinned CMPSET/CMPSETI bytes in `test/compiler/x64-emit.f` (and any other pinned stream that contains the sequence) are updated from llvm-mc.
Failing check: C8's native comparison. Build a scratch tree that merges this change with C8's commit (`jj new <fix> <C8>` in a scratch workspace, not committed), run `test/x86-64-peer-routines.f` under qemu on the ThinkPad, and run the manifest comparison documented in C8's `docs/bootstrap.md`: before the fix `cmpset` and `cmpseti` exit 22; after it all 24 images match.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, and any other pinned x86 test stream the change alters.
Verify: the ThinkPad under qemu: `test/compiler/x64-emit.f`, `test/compiler/x64-chain.f`, `test/compiler/x86-64-asm.f`, `test/x86-64-emit.f`, `test/x86-64-peer-image.f`; the scratch-merge comparison above, all 24 statuses matching.
Depends: none; its native proof uses C8's unlanded commit.
Route: direct (x86-only: `emit-x64.f` is loaded by neither `src/habu/native-runtime.f` nor `src/habu/habu2.f`, so no ARM64 engine bytes change).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-return-all-ones-7f25749e.
Preflight (READY) additions: the fix falsifies the "three instructions" comments at `src/compiler/native/emit-x64.f:471-473` (PUT-SETCC header), `src/compiler/native/x64ir.f:1099-1101` (DEF-CMPSET header), `src/compiler/native/select-x64.f:1202-1203` and `test/compiler/x64-emit.f:381-383,391-392`, and `docs/x86-64.md:172` ("compare and set a boolean") should name the 0/-1 flag; correct each. The pinned streams are exactly `test/compiler/x64-emit.f:1080` and `:1089`; their `mc:` line groups (1076-1079, 1084-1088) gain `\ mc: negq %rax` per the file's convention (header lines 8-15). `ENC-NEG ( r64 ptr a -- )` exists (`src/arch/x86-64/asm.f:627`, pinned `48f7d8` at `test/compiler/x86-64-asm.f:253`). The failing check merges with C8's `wptxqqnsmrtu` (now on master).
