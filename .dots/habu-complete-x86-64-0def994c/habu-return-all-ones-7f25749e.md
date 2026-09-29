---
title: Return all-ones flags from x86 compares
status: open
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
Claim: unassigned.
