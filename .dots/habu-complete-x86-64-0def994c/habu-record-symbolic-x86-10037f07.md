---
title: Record x86 call and code sites with exact spans
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.475867+03:00"
---

Problem: `wordcall`/`tailcall` resolve to absolute host entries at emit time (`docs/x86-64.md:192-206`) and `TRAILING-RETURN?` mirrors ARM64's size-minus-one rule; the publisher, the capture and the linker need every site recorded and every span exact.
Acceptance: every inter-word call, tail call, `codeaddr` and `MOVABS` literal is a site row (byte offset, kind, target = the callable row's host entry address, `src/compiler/native/hir-word.f:62,574-578`, or the literal's kind); callee identity is resolved later through the capture's xt->record index (`aot-capture.f:86-100,137-150`), so `hir-word.f` is not changed; `MOVABS` literal rows use the `SNAP-RELOC:MOVABS` kind (cross-build obligation (3): the equality pin in `test/x86-64-seam.f` stays until that kind is defined once); spans exact for return, tail, no-return and `does>` bodies; in shadow mode the call displacement is emitted as zero and the row is authoritative; an emission linked at two placements executes identically on the ThinkPad.
Files: `src/compiler/native/emit-x64.f`, `test/compiler/x64-emit.f`, `test/x86-64-peer-routines.f`.
Verify: spark `bin/hb --load test/compiler/x64-emit.f`; ThinkPad: routine images linked at two placements.
Depends: habu-render-x86-trap-33eba82b (C5).
Route: direct.
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-record-symbolic-x86-10037f07.

Preflight corrections (2026-09-30; override the lines above where they differ):
1. Kinds: literal and `codeaddr` rows keep `X64IR:ADDR-DATA`/`ADDR-CODE` as kind (`x64ir.f:235-238`); no `SNAP-RELOC` kind is added (none named `MOVABS` exists: `src/habu/address-carrier.f:113-152` has only `MOVABS-*` shape constants) and the `test/x86-64-seam.f:219` pin stays.
2. Trap row: every displacement measured through `FROM-PLACE` (`wordcall`, `tailcall`, `trap`; `emit-x64.f:657-685`) files a call row `( byte-offset kind target )`, kinds `CALL`/`TAIL` as `NEMIT` (`src/compiler/native/emission.f:66-67`), target = `x64.entry`/`x64.trap-entry`; readers `CALL-SITES`/`CALL-SITE@`/`CALL-KIND@`/`CALL-TARGET@` beside `ADDR-SITES`.
3. C3's `idiv` throw-entry branch (`select-x64.f:1184-1204`) is another absolute site: it files a row through the same word; whichever of C3 and C6 lands second adapts.
4. Shadow mode is `EMIT` without `PLACE-AT` (`PLACED?` false); call/tail/trap rel32 = 0 with no reach check; `codeaddr` imm64 = the function's byte offset in the emission; DATA imm64 = the value; placed mode is unchanged.
5. Exact spans: remove `X64EMIT:TRAILING-RETURN?` and its pins (`test/compiler/x64-emit-fixture.f:1421,1461,1496,1540`; `docs/x86-64.md:373`); a `;does` body's span is `SIZE` less `FUNCTION-OFFSET@ 1`; P3 answers `NEMIT:RET-BYTES` 0 (the size-minus-one rule lives in `publish.f:80-83` `RECORDED-LEN`).
6. Files add `test/x86-64-peer-harness.f` (owns `POSITION`/`PAD-TO`/`APPEND-ROUTINE`, lines 212-244; linking rows at a second placement needs a patch word there). Design reference: `docs/x86-64.md:359-368`. Fixtures live in `test/compiler/x64-emit-fixture.f` (`x64-emit.f` is a three-line entry).
7. Pre-change failure: `X64EMIT` has no call-site reader and `EMIT` refuses an unplaced run (`emit-x64.f:1133,975-976`).
