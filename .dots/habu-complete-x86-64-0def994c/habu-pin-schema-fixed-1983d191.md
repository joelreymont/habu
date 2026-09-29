---
title: Pin schema-fixed operands, forbid their overlap
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T17:34:20.074440+03:00"
blocks:
  - habu-render-x86-neg-43e4e8f8
---


Problem: `src/compiler/native/regalloc.f` treats a form's schema-fixed operand register as a want (`MB-FIXED-PLANE`, `regalloc.f:1177`; `MB-WANTED` grants it only when free, `:1730-1738`) and indexes only fixed results (`MB-FIX-MASK`, `:2215-2222`). So a value in rcx that is read by or live across `x64.shl`/`x64.shr` keeps the count copy out of rcx, and the validator (`regalloc-verify.f:734-741`) refuses a valid program with `E-A64RAV-FIXED`: `( a b n -- x ) lshift +` (b, the tied destination, holds rcx) and `( a n -- x ) 2dup lshift rot xor +` (crossing holders), under LEAF and DLEAF. `select-x64.f:40-48` point THREE claims the copy repairs this; it does not. Found by C2; repros in the C2 worker's notes (`dbg2.f`, `dbg3.f`).
Acceptance: (a) a schema-fixed operand is a pin, not a want: `MB-FIX-MASK` masks every fixed side, operands as well as results, so `MB-FORBID-FIXED` keeps every class that crosses the operation, or is read by it and is not the declared class, out of that register, the twin of the result rule (`MB-FIXED-BITS` `:1685-1689`, exemption `MB-DECL-BIT` `:1663`); the comments at `:1627` and `:1670-1689` say so. (b) `MB-FIXED1` (`:1180-1187`) declares `D-FIX`; `MB-FIXED-PLANE` is deleted; the comment at `:1158-1166` is rewritten. Contract result wants stay wants (`MB-PLAN-MOVES` `:2013-2023` repairs them with `P-MOVE`). ARM64 declares no fixed operand (`a64ir.f` has no `ADD-FIXED`) and neither ABI places an argument in a register, so ARM64 placement is unchanged. (c) `test/compiler/x64-regalloc.f` gains two accepted rows under LEAF and DLEAF: `( a b n -- x ) lshift +` and `( a n -- x ) 2dup lshift rot xor +`, each asserting the count copy's register is rcx and `ACCEPTED?`; `TWO-COUNTS`, `RESERVED-FIX` and `test/compiler/native-regalloc.f:2871-2905` stay refused as they are (`TWO-COUNTS` then throws `E-A64RA-FIXED` from `MB-PIN`, `:1725`). (d) `test/compiler/x64-emit.f` pins both shapes' bytes; `test/x86-64-peer-routines.f` runs both natively (counts 0, 1, 63, 64, with negative twins). (e) `select-x64.f:40-48` names the allocator's forbid; `docs/x86-64.md:193-196` likewise. (f) `lib/errors.f:1259`: `E-X64EMIT-FORM`'s description drops "a variable shift" and "a frame access" (C1, C2).
Files: `src/compiler/native/regalloc.f`, `src/compiler/native/select-x64.f`, `test/compiler/x64-regalloc.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md`, `lib/errors.f`.
Verify: ThinkPad (qemu): `test/compiler/x64-regalloc.f`, `x64-select.f`, `x64-emit.f`, `x64-chain.f`, `test/compiler/native-regalloc.f`; `test/x86-64-peer-routines.f`, then every image in `$HB_TMP/x64-routines` natively against the manifest. spark: rebuild (`regalloc.f` and `lib/errors.f` are in the engine closure, `native-runtime.f:98,109`, so the product moves off its base sha); the five-generation chain converges; the gate. ARM64 generated code is expected unchanged; the gate proves it.
Out of scope, recorded: under pressure `MB-CANDIDATE?` (`:1858-1863`) can evict a coalesced count class whose reload lands outside rcx; the validator then refuses (never a miscompile). It needs nine or more live GPR classes; no fixture reaches it.
Depends: habu-run-emitted-x86-b704f918 (C8), habu-render-x86-neg-43e4e8f8 (C2), habu-emit-x86-frame-d8d25223 (C1: both edit `select-x64.f`, and C1 makes "a frame access" stale). Base: the C1 and C2 stack on K2+C8.
Route: Alder (engine closure: `regalloc.f`, `lib/errors.f`).
Ownership: krait (Intel lane).
Claim: unassigned.

Preflight corrections (Fable preflight 2026-09-30, READY; these override the lines above where they differ):
- Pre-change failing check (verified on the C1+C2 stack with K2's engine, through `X64SEL:SELECT` -> `A64RA:ALLOCATE` -> `A64RAV:ACCEPT`): both rows throw `E-A64RAV-FIXED` (-8461) under LEAF and DLEAF.
- (a) also names the `MB-FIX-MASK` header comment `regalloc.f:2213-2214` ("for its RESULTS") and `MB-FORBID` `:1707-1709` ("forbids its fixed results").
- (e) `docs/x86-64.md:193-196` is `:196-203` on the C1+C2 stack. The closure cite is `src/habu/native-runtime.f:98,109`.
- Feasibility: `MB-STEP` (`:1812-1820`) runs `pos 1+ MB-EXPIRE` before `MB-PLACE-PINNED`, so a count dying at the copy frees rcx before the pinned copy is placed.
- Base: master once C1 and C2 land (they are being ported onto master's split test layout: pinned bytes now go in `test/compiler/x64-emit-fixture.f` and cases in `test/compiler/x64-emit.f`, per master `0b0421fa`). Host engine: K3's product `264c829e…` (ThinkPad `~/.cache/habu-krait/qhb/hb-k3-264c`) for the x86 suites; the rebuild, chain and gate run on spark.
- Route: lands on master after the Linux gate (rebuild, five-generation chain, spark gate); Alder pools the Mac gate.
