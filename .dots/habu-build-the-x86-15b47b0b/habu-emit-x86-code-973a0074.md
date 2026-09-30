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
Claim: unassigned.
