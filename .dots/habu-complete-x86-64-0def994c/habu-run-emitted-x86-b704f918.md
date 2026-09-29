---
title: Run emitted x86 routines natively as a fixture family
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.424177+03:00"
---

Problem: `test/x86-64-peer-image.f` executes one routine; new forms have only pinned bytes (`test/compiler/x64-emit.f`). Every other C leaf verifies through this family.
Acceptance: `test/x86-64-peer-routines.f` builds one ELF per HIR fixture through the real rows (`NBACK:EMIT` at a placement, as `X64PEER:PEER-ROUTINE` does), each with a positive and a wrong-expectation negative; a manifest lists expected exit statuses; on the ThinkPad every positive exits 0 and every negative its stated code.
Files: `test/x86-64-peer-routines.f`, `test/gate-stdlib-cases.f` (host row builds only).
Verify: spark `bin/hb --load test/x86-64-peer-routines.f`; ThinkPad runs `$TMP/x64-routines/*` and compares the statuses with the manifest.
Depends: habu-add-the-x86-aad02c7e (K1).
Route: Alder (shared: test/gate-stdlib-cases.f).
Ownership: krait (Intel lane).
Claim: unassigned.
Preflight corrections (these override the lines above where they differ):
- Sink and harness: bind to `X64CODE` (K1). The ELF harness words both peer files need (`test/x86-64-peer-image.f` `ENTRY,`/`CASE,`/`FAIL-IF`/`PAD-TO`/`EXIT,`/`BUILD`, lines 27-129) move into one shared test module, `test/x86-64-peer-harness.f` (package `X64HARNESS`), used by `test/x86-64-peer-image.f` and the new file; nothing is copied.
- Fixtures: the `X64EMIT-TEST` `BUILD-*` words, reached by requiring `test/compiler/x64-emit.f` and reopening its package (as `test/x86-64-peer-image.f` reopens `X64CHAIN-TEST`). One ELF per fixture the rows emit today; `BUILD-DIFF` is proven through the rows (`test/compiler/x64-chain.f:352-366`); the others were selected under the register LEAF contract (`test/compiler/x64-emit.f:736-755`), not the rows' `*-FRAMED` contracts (`src/arch/x86-64/passes.f:88-105`), and `BUILD-PRESSURE` is refused at emit (`src/compiler/native/emit-x64.f:656-659`). Report each fixture the rows refuse, by error. `WORDCALLER`'s callee is a second routine placed in the same image.
- Manifest: `$HB_TMP/x64-routines/manifest`, one line per image, `<file> <status>`, written by the Habu builder (the directory made with `MAKE-DIR`, `lib/fs-mutate.f:139`). The ThinkPad comparison is a documented command in `docs/bootstrap.md` beside the peer-image procedure (`docs/bootstrap.md:142-152`); no checked-in shell or Python runner.
- Package `X64ROUTINES`. Gate row `SUITE x86-64-peer-routines` beside `test/gate-stdlib-cases.f:1200-1203`, one spawn.
- Files: `test/x86-64-peer-routines.f`, new `test/x86-64-peer-harness.f`, `test/x86-64-peer-image.f`, `test/gate-stdlib-cases.f`, `docs/bootstrap.md`. Route stays Alder (`test/gate-stdlib-cases.f`).
