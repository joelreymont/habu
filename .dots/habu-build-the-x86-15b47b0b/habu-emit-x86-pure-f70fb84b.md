---
title: Emit x86 pure-op bodies from HIR
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.551789+03:00"
closed-at: "2026-09-30T17:40:00.000000+03:00"
close-reason: ThinkPad x86 proof plain and Mac-shim green; hb-x64-kernel-pure 0, -negative 21, three -armed 83; spark suite ok on 925c
blocks:
  - habu-share-the-parity-045ddf20
---

Problem: the kernel has no bodies for the pure-op rows of `src/habu/prims.f`.
Acceptance: the arithmetic/compare/shuffle rows and the memory rows (`@ ! +! c@ c! ptr-field byte-view cell-view count cells cell+ chars char+`) get x86 bodies by building a one-op HIR module per row and running the x86 rows (as `test/compiler/x64-chain.f` builds modules), so callable and inline semantics share one lowering; `test/prim-parity.f`'s case sets run natively through C8-style images (the parity file itself needs an engine; until X5 the cases are data for routine images); the rows join the `docs/x86-64.md` kernel inventory. Alternative: hand-written bodies (~150 lines); the HIR route is recommended because the parity gate then proves the compiler's own lowering of every op.
Files: `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images carrying the parity case sets.
Depends: habu-emit-x86-frame-d8d25223 (C1), habu-render-x86-neg-43e4e8f8 (C2), habu-render-x86-idiv-9c6d9516 (C3), habu-render-x86-trap-33eba82b (C5), habu-boot-and-exit-367c46f5 (K3), habu-share-the-primitive-58c235e5 (K4).
Route: direct.
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Rows (52) through `PRIM-HIR`, per the word model (`hir-word.f:1196-1470`, `elaborate.f:3415-3455`): `+ - * / and or xor lshift rshift = <> < > <= >= invert 1+ 1- 0= cells cell+ @ c@ ptr-field mod max`, picks of `dup drop swap over nip tuck rot 2dup 2drop byte-view cell-view`. Unmodeled, stated here (`hir-word.f` unchanged, r2:173): `-rot 2swap 2over` (picks), `chars` (identity), `char+` (x 1 add), `negate` (0 x sub), `0<` (x 0 lt), `abs` (m = x 0 lt; x m xor m sub), `min` (MAXIMUM with lt), `/mod` (one div; a q b mul sub, q), `count` (x 1 add, x bload). Hand-written: `! c! +!`, guarded twins of `habu1.f:1607-1620` (compiled stores call them, `elaborate.f:3329-3334`), and `?dup` (`BQDUP`).
- Seam: `src/habu/kernel-hir-x64.f`, package `X64KHIR`, `COMPILE ( ptr u8 n n n [ -- ] [ -- ] -- )` (name in out stager use): one function under `X64ABI:BINDING` in `IR-CTX:WITH-CONTEXT`; NBACK DECLARE (`L-NONE`) SELECT PRUNE FIXPOINT; `X64PASS:EMIT-UNPLACED` (`NBACK:EMIT` always places, `passes.f:233-238`), then `use`. Stagers use `ARG ( n -- v )` (0 deepest), `RESULT ( v -- )`, `LIT`, `OP1`, `OP2`, `FETCH`; a result count other than `out` dies 76. `PRIM-HIR ( ptr u8 n n n [ -- ] -- )` appends the bytes between the row's labels, no `ret`. A call row other than `NEMIT:CALL` to `X64SEL:THROW-ENTRY` (made public), or an address site, dies 76; that call's field (`CALL-SITE@` + new `X64ASM:CALL-REL32-OFF`) becomes `X64CODE:REL32-SITE ( n label -- )` to `s" throw" ENTRY-LABEL`. New `X64ABI:BINDING`. `PURE,` ends `KERNEL,`. The peer harness's `REL32-AT` becomes `CALL-REL32-OFF`.
- Test: `test/x86-64-kernel-pure.f`, package `X64K-PURE`, `SUITE x86-64-kernel-pure` after `x86-64-kernel-ffi`. Each case check dies 21 through `die`, naming subject and case (ten statuses). Images: `hb-x64-kernel-pure` 0, `-negative` 21, `-store-armed`, `-cstore-armed`, `-addstore-armed` 83. Docs: `### Pure rows` follows `### FFI rows`.
- Pre-change: `$HOST --load test/x86-64-kernel-pure.f` dies 76, `x64kernel: no registered body named dup` (`kernel-x64.f:160-167`).
- Verify (ThinkPad, qemu host): that file, `test/x86-64-emit.f`, `test/compiler/x64-{emit,chain}.f`, `test/x86-64-peer-routines.f`, the five kernel suites; every image natively.
- Depends add: scaffold, K7, the case dot. Files: as above, not `peer-routines.f`.
