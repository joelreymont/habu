---
title: Place x64 divide fixtures in reach
status: open
priority: 1
issue-type: task
created-at: "2026-09-30T14:19:35.652919+03:00"
---

Problem: C3 (habu-render-x86-idiv-9c6d9516) renders the divide's cold side as a call to the running engine's `throw` (select-x64.f THROW-ENTRY). Three fixtures link that call from low placements: x64-emit's DIV-BYTES places the divide at address 0 (test/compiler/x64-emit-fixture.f DSTACK-EMITTED, checked at the `47 REL32@ THROW-TARGET 51 -` pin), and the peer images div/divneg (test/x86-64-peer-routines.f DIV-BODY through the selector) and remainder (REMAINDER-BODY, THROW-TARGET) link it through X64HARNESS LINK-CALL. On Linux the engine loads at 0x400000, so rel32 reaches; on Darwin arm64 the engine's code sits above 4 GB (the same cause as habu-place-x64-trap-11e142ea for `die`), so the emitter's REL-TO/FIT and LINK-CALL throw E-X64EMIT-REACH and the Mac gate's compiler-x64-emit and x86-64-peer-routines rows go red.
Acceptance: keep the emitter's signed rel32 contract and its far-call refusal. x64-emit: place the selected divide at an SP-ALIGN slot next to THROW-TARGET (the TRAP-SLOT pattern) and derive the expected call field from that slot; every other pinned byte unchanged. Peer images: follow the trap precedent (the peer-routines header: a routine the image carries stands in for the host entry): div, divneg and remainder render the cold side to a callee the image carries, so no peer image links to a host-engine address; the selected-divide path stays covered by x64-emit and x64-chain.
Files: test/compiler/x64-emit-fixture.f, test/x86-64-peer-routines.f (and its header note).
Verify: ThinkPad with the stack fixpoint hb-stack-613a: test/compiler/x64-emit.f, test/x86-64-peer-routines.f rc 0; every peer-routines image natively with its stated status; the x64-routines manifest loop bad=0. Pre-change evidence is the Darwin placement (not reproducible on Linux): state in the report the lowest and highest call target each changed fixture links, showing each is within rel32 of its site for any throw address.
Route: direct (test-only).
Ownership: krait (Intel lane).
Claim: krait.
