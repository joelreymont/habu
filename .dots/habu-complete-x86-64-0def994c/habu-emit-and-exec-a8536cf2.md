---
title: Emit and execute x86 scalar floats
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.492952+03:00"
---

Problem: after C7a the float forms select and allocate but `src/compiler/native/emit-x64.f` renders none of them. Second half of the float work.
Acceptance: every C7a float form renders; spills across calls; NaN compare semantics equal to ARM64's; the float fixtures execute natively on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-declare-and-select-bfbb301b (C7a), habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: krait.
C7a landing note: (a) `realint` must meet the `f>s` contract (saturate, NaN to 0): settle it in `select-x64.f`'s `realint` arm (`asm.f:576-577` leaves range and NaN to lowering) and prove it with NaN, +2^63 and -2^63 rows; (b) the `fcmpset` render maps `gt` to `seta` (C-A; after `ucomisd` SF=OF=0, so `setg` is wrong), `equal` to `sete` plus `setnp` into the scratch, and refuses any other condition or a result count other than 1 (gt) or 1 plus 1 scratch (equal): nothing enforces the one-scratch rule today (hand-built modules with the wrong count pass freeze, allocation and A64RAV).

Preflight corrections (2026-09-30; override the lines above where they differ):
- Files: `src/compiler/native/emit-x64.f`, `src/arch/x86-64/asm.f`, `src/compiler/native/select-x64.f`, `test/compiler/x64-emit-fixture.f`, `test/compiler/x86-64-asm.f`, `test/compiler/x64-select.f`, `test/x86-64-peer-routines.f`, `src/compiler/native/x64ir.f` (the comment at 1186-1195), `lib/errors.f` (`E-X64EMIT-FORM` text), `docs/x86-64.md`. `test/compiler/x64-emit.f` is a 3-line runner; the fixtures live in `x64-emit-fixture.f`, which `x86-64-peer-routines.f:47` loads.
- Asm: `asm.f` has no `movq` in either direction (its double section ends at `ENC-CVTTSD2SI-RR`, 553-581), yet `fconst` (`select-x64.f:870-877`), the `fnegate`/`fabs` masks (1330-1338) and `bitsreal`/`realbits` render `x64.movq-xr`/`movq-rx` (`x64ir.f:1817-1818`). Add `X64ASM` `ENC-MOVQ-XR` (66 REX.W 0F 6E /r) and `ENC-MOVQ-RX` (66 REX.W 0F 7E /r), pinned against `llvm-mc` (`/usr/bin/llvm-mc`) for xmm0/xmm8/xmm15 x rax/r8/r15.
- realint (landing note (a)): select `(cvttsd2si x) xor (fcmpset gt x, bits $43DFFFFFFFFFFFFF) and (fcmpset equal x, x)`: branch-free, built only from forms this dot renders ($43DFFFFFFFFFFFFF is the largest double below 2^63; cvttsd2si answers MIN-N for NaN and out of range).
- Native fixtures (`x64-emit-fixture.f`, one routine image each): addsd/subsd/mulsd/divsd, sqrt, fnegate/fabs, intreal, realint rows {NaN->0, +2^63->MAX-N, -2^63->MIN-N, +inf->MAX-N, -inf->MIN-N, -2.7->-2}, bitsreal/realbits, fconst, and flt/fgt/feq/fltz/feqz with NaN on each side answering 0; a double live across a wordcall whose callee writes every XMM register (the only callee today, `x86-64-peer-routines.f:614-625`, leaves XMM alone, so it cannot catch a missing spill); a wrong-count fcmpset refused.
- Pre-change failing check: every double form throws `E-X64EMIT-FORM` (`emit-x64.f:882-896`).
- Verify add: spark `bin/hb --load test/compiler/x86-64-asm.f` and `test/compiler/x64-select.f`.
