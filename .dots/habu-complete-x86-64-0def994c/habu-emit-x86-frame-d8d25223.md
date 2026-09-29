---
title: "Emit x86 frame forms: reserve, release, store, load"
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.432839+03:00"
closed-at: "2026-09-30T09:51:10.274084+03:00"
close-reason: x86 frame forms emit; ties copied in allocator order; 17/17 native images
---

Problem: `src/compiler/native/emit-x64.f:40-51` refuses `reserve`, `release`, `store` and `load` with `E-X64EMIT-FORM`; spills lower but cannot execute.
Acceptance: pressure fixtures (spills across branches, loops, calls, several slots) execute natively on the ThinkPad with exact `rsp` restoration (compare `rsp` before and after as the peer fixture does with `RSP RBP`); bytes pinned against `llvm-mc`.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f` and `bin/hb --load test/compiler/x64-chain.f`; ThinkPad routine images (C8).
Depends: habu-run-emitted-x86-b704f918 (C8).
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-emit-x86-frame-d8d25223.

Preflight corrections (these override the lines above where they differ):
- Seam: `PUT-OP` refuses the four forms (`emit-x64.f:659-662`); `NBACK:EMIT` of the fixpoint row's spilling module (`test/compiler/x64-chain.f:297-306`: 8 stores, 8 loads over slots 0..24, one `reserve` of 32) throws `E-X64EMIT-FORM` through the real rows. That is the failing check before the change.
- The emitter binds neither `KEY-SLOT` nor `KEY-FRAME` (`BIND-DIALECT` `emit-x64.f:1000-1015`, `BND-*` `187-196`); this leaf binds both, each with its accessor.
- "spills across calls" means a framed routine that calls, with pressure on both sides of the call and `reserve` held across it: `CALL-SAVE` (`select-x64.f:699-716`) moves every value live across a call to data-stack cells, so no register value is live across an x86 call.
- Pressure fixtures (branch, loop, call, several slots) run natively through C8's family in `test/x86-64-peer-routines.f`; exact `rsp` restoration is already checked by `X64HARNESS:INVOKE,`. `BUILD-PRESSURE` is private to `X64CHAIN-TEST` (`test/compiler/x64-chain.f:47,173`): make it public and reuse it (Files add `test/compiler/x64-chain.f`); never copy it.
- Base: K2 (`xlvxmoou`) merged with C8 (`wptxqqns`), as K3's; host engine K2's product `6f6ae5b9…`. Route: direct (x86-only files) once K2 and C8 are on master.
