---
title: Run emitted x86 routines natively as a fixture family
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.424177+03:00"
closed-at: "2026-09-30T09:26:25.077189+03:00"
close-reason: C8 landed as c0cd21a8 (interdiff empty)
---

Problem: `test/x86-64-peer-image.f` executes one routine; new forms have only pinned bytes (`test/compiler/x64-emit.f`). Every other C leaf verifies through this family.
Acceptance: `test/x86-64-peer-routines.f` builds one ELF per HIR fixture through the real rows (`NBACK:EMIT` at a placement, as `X64PEER:PEER-ROUTINE` does), each with a positive and a wrong-expectation negative; a manifest lists expected exit statuses; on the ThinkPad every positive exits 0 and every negative its stated code.
Files: `test/x86-64-peer-routines.f`, `test/gate-stdlib-cases.f` (host row builds only).
Verify: spark `bin/hb --load test/x86-64-peer-routines.f`; ThinkPad runs `$TMP/x64-routines/*` and compares the statuses with the manifest.
Depends: habu-add-the-x86-aad02c7e (K1).
Route: Alder (shared: test/gate-stdlib-cases.f).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-run-emitted-x86-b704f918.
Preflight corrections (these override the lines above where they differ):
- Sink and harness: bind to `X64CODE` (K1). The ELF harness words both peer files need (`test/x86-64-peer-image.f` `ENTRY,`/`CASE,`/`FAIL-IF`/`PAD-TO`/`EXIT,`/`BUILD`, lines 27-129) move into one shared test module, `test/x86-64-peer-harness.f` (package `X64HARNESS`), used by `test/x86-64-peer-image.f` and the new file; nothing is copied.
- Fixtures: the `X64EMIT-TEST` `BUILD-*` words, reached by requiring `test/compiler/x64-emit.f` and reopening its package (as `test/x86-64-peer-image.f` reopens `X64CHAIN-TEST`). One ELF per fixture the rows emit today; `BUILD-DIFF` is proven through the rows (`test/compiler/x64-chain.f:352-366`); the others were selected under the register LEAF contract (`test/compiler/x64-emit.f:736-755`), not the rows' `*-FRAMED` contracts (`src/arch/x86-64/passes.f:88-105`), and `BUILD-PRESSURE` is refused at emit (`src/compiler/native/emit-x64.f:656-659`). Report each fixture the rows refuse, by error. `WORDCALLER`'s callee is a second routine placed in the same image.
- Manifest: `$HB_TMP/x64-routines/manifest`, one line per image, `<file> <status>`, written by the Habu builder (the directory made with `MAKE-DIR`, `lib/fs-mutate.f:139`). The ThinkPad comparison is a documented command in `docs/bootstrap.md` beside the peer-image procedure (`docs/bootstrap.md:142-152`); no checked-in shell or Python runner.
- Package `X64ROUTINES`. Gate row `SUITE x86-64-peer-routines` beside `test/gate-stdlib-cases.f:1200-1203`, one spawn.
- Files: `test/x86-64-peer-routines.f`, new `test/x86-64-peer-harness.f`, `test/x86-64-peer-image.f`, `test/gate-stdlib-cases.f`, `docs/bootstrap.md`. Route stays Alder (`test/gate-stdlib-cases.f`).
Preflight corrections, rev 2 (after K1 landed; these override everything above where they differ):
- Harness range: the shared words are `test/x86-64-peer-image.f` lines 13-106 and 123-152, not 27-129: `EXIT-CELL`/`ROUTINE-CELL`/`EXIT-LBL`/`ROUTINE-LBL` (17-21), the `required` ELF/OS-seam block (26-30; it loads into exactly one package, per its comment at 23-25, so it moves into `X64HARNESS`), `IMM` (32), `ASSERT-EQ` (44), `RESERVED,` (58), `LINKED-ADDRESS,` (64-69), `POSITION`/`APPEND-ROUTINE` (99-105), `BUILD` (132-142), and the sink lifetime `CODE-CAP-BYTES BUF:INIT`/`BUF:DISPOSE` (147/150). The `X64CODE` binding already exists (peer-image.f:6,10).
- Harness shape: `ENTRY,` (71-96) hard-codes DIFF's five cases and `CASE,` (46-56) is fixed at 2-in/1-out. The shared harness splits the entry into prologue, per-fixture cases and epilogue, and adds a 1-in case word: SQUARE, SHIFTS, NOT, MOVI, DADDRESSED, WORDCALLER and SELFCALLER are `1 1 OPEN-FUN`.
- Fixtures: the 14 `X64EMIT-TEST` HIR fixtures at `test/compiler/x64-emit.f:323-518`. Exclude `BUILD-NEGATOR`/`DIAMOND`/`PAIR`/`RELOC` (596-727): they are `( -- IR-BUILD:module )` machine-dialect modules that bypass the rows. `BUILD-ADDRESSED` (413) takes a memory-order argument and is refused `E-A64RAV-ORDER` (x64-emit.f:940); report it as a refused fixture. `BUILD-PRESSURE` is `X64CHAIN-TEST:BUILD-PRESSURE` (x64-chain.f:173), not an `X64EMIT-TEST` word.
- Driver: `X64CHAIN-TEST:CHAIN` (x64-chain.f:199-204) reads `X64CHAIN-TEST:CC`/`BB` (78-83), while the `X64EMIT-TEST` fixtures write `X64EMIT-TEST:CC`/`BB` (x64-emit.f:161-165) after its own `HIR-MOD` (175-183). So the driver runs `NBACK:DECLARE`/`SELECT`/`PRUNE`/`FIXPOINT` inside reopened `X64EMIT-TEST`. WORDCALLER and SELFCALLER declare `NBACK:L-CALLED` (passes.f:83, 104-105, giving `CALL-FRAMED`, as x64-emit.f:893/901 use `CALL-ALLOCATED`); the rest declare `L-NONE` (x64-chain.f:201).
- Line fixes: "x64-chain.f:352-366" is 355-379 (`DIFF-IMAGE?` 358, `PLACED-BODY` 367); "passes.f:88-105" is 89-107 (`ROUTINE` 91).
- Manifest directory: `MAKE-DIR` throws E-FS-IO on an existing directory, so a documented re-run into one `HB_TMP` (docs/bootstrap.md:142) would die. Use `MAKE-DIRS` (lib/fs-mutate.f:203-211, which tolerates `DIR?` at 197-201). `TMP-PATH` (src/os/env-base.f:112-119) joins `$HB_TMP/<name>` and accepts a slash.
- WORDCALLER's callee: `BUILD-WORDCALLER ( n -- )` (x64-emit.f:507) bakes the absolute entry (`WCALL-ATTRS` 481-485), and `ENTRY-TARGET` (emit-x64.f:599-602) subtracts the placement. So the callee is emitted first at a 16-aligned offset (`X64IR:SP-ALIGN`, x64ir.f:148), and its address is `VMBASE CODE-OFF + off`.
Landing note: the native comparison found x86 compares returning 1 for true (`cmpset`/`cmpseti` exit 22); `habu-return-all-ones-7f25749e` fixes the emitter, and this leaf lands after it so every positive exits 0.
