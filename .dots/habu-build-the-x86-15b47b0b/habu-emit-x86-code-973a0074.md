---
title: Emit x86 code window and publication rows
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T09:22:47.240129+03:00"
---

Problem: the code window and publication rows need the x86 protection window and the sorted map inserts; K8a covers only encoders and atomics. Split from K8 by the K-lane design (2026-09-30).
Acceptance: in the K8 section of `kernel-x64.f`:
- `PROT:LSPAN/LOPEN/LCLOSE` twins (`habu1.f:3890-3990`: band widening of `WLO/WINDOW`, `RLO/RHI`, the `CF` latch).
- `code-publish`: span guard (`GUARD-CODE-SPAN`, `habu1.f:2495`; append-only at CP, `habu1.f:2555-2557`), then mprotect RW over [CP, CP+len), copy, mprotect RX, drop map rows of both kinds in the span, CP += len. No `LFLUSH` (x86 needs no cache maintenance); no TIER-PROV call (K9b adds it).
- `callmap-set`/`addrmap-set`: sorted insert with move-up per `prims.f:443-469`; `reloc-maps-clear`.
- `xref-retarget` (`habu1.f:2670-2705`; ARM64 `STLR` becomes a plain `mov` under TSO) and `int-mark`/`min-in-mark` (`habu1.f:3111-3141`) through `PROT-REC,`.
- `does-record` (`habu2.f:3350-3358`).
- `patch32` (`habu1.f:2412-2431`) with `PROT-REC,` and `PROT-SPAN-CALL,`; its provenance INVALIDATE is K9b's.
- `does-patch` is not here: I7 owns it (slot contract in I7).
Each row runs in `test/x86-64-kernel-atomics.f` through the booted harness with a negative twin; the table joins `docs/x86-64.md` `### Atomics and publication rows`.
Files: `src/habu/kernel-x64.f`, `test/x86-64-kernel-atomics.f`, `docs/x86-64.md`.
Verify: host K3's product engine: `--load test/x86-64-kernel-atomics.f`; ThinkPad: the images natively.
Depends: habu-emit-x86-atomics-c02e092e (K8a). Hand-written through `X64ASM`; no allocator dependency.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-emit-x86-code-973a0074.

Preflight corrections (2026-09-30; override the lines above where they differ):
- x86 spans are byte-granular. The `code-publish` span guard keeps the nonzero, no-wrap, `[DBASE+DICT-SIZE, DBASE+REGION)` and dst = CP checks and drops ARM64's `& 3` traps (`habu1.f:2394-2402`); `xref-retarget` keeps the NDICT+1 bound, `RAW-MAX` and the FULL-with-empty-body refusal and drops `& 3` (`habu1.f:2593`). Reason: `publish.f:79-83,97-101` pass byte sizes on x86 (a `ret` is one byte); a faithful twin would exit 83 on every x86 `COMMIT`.
- Line refs (I4a shifted `habu1.f`): LSPAN/LOPEN/LCLOSE bodies `habu1.f:3794-3868`, contract `3711-3746`, band cells `layout.f:630-636`; `GUARD-CODE-SPAN` `2394`; append-only `2455-2457`; `BCODEPUBLISH` `2447-2475`; map contract `prims.f:484-508` and `layout.f:1666-1704`; `xref-retarget` `2569-2610`; `int-mark`/`min-in-mark` `3016-3050`, registered into `ENGINE-PRIMS:GLOBAL-INT-WID` (`habu1.f:3383-3384`; x86 `PRIM-WID`, `kernel-x64.f:132`); `does-record` = `DOES-REC:NATIVE-PRIM` `habu2.f:3278-3360` (`NAME$` reads `PEND-CELL`; `RECORD` declares its own `LSPAN` over the pending+1 record; `LOPEN`/`LCLOSE` around the name copy), registered `11081`; `patch32` `2311-2333`.
- Tests: single positive images; the suite's `-negative` image proves the harness (no negative twins). Each image holds ten checks (`test/x86-64-peer-harness.f:37,55`) and `hb-x64-kernel-atomics` already uses all ten (`test/x86-64-kernel-atomics.f:63`), so K8b adds its own images to that suite.
- Not in scope: `src/habu/code-span.f` (`INSN-BYTES 4`) on the Habu side; P3 (`habu-fill-nemit-from-d8c030e4`) owns making it byte-granular.
