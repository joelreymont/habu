---
title: Emit x86 heap, printer and hook rows
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T09:22:47.260112+03:00"
blocks:
  - habu-scaffold-the-x86-9af80979
---

Problem: the heap, printer and checker-hook rows are unassigned to any x86 leaf. Split from K9 by the K-lane design (2026-09-30).
Acceptance:
- `here allot align , c,` with a `DP-CHECK` twin (`habu1.f:1807-1836`) and a kernel-local `LDPBAD` (fd-2 line, exit 76; `habu2.f:9969-9975`).
- `. u. .s depth emit cr space type` with `G-PRINT9`/`G-PRINTU9`/`G-EMITC`/`G-OUT` twins in `src/arch/x86-64/rt.f` (`src/habu/rt.f:185-250`); `G-OUT` honours `GENIO-ABI:OUT-CELL`. The device arm's `(LGENIOOUT)` contract is not traced yet: trace it first and state it in the leaf's docs table.
- `set-check check@ set-preflight set-top-check top-check@` (`habu1.f:2905-3064`, `1398-1399`).
Cases through the booted harness in `test/x86-64-kernel-engine.f`, each with a negative twin; the printers' fd-1 bytes are compared by the peer against the ARM64 engine's output for the same values (`MIN-CELL`, `MAX-CELL`, 0, -1). The table joins `docs/x86-64.md` `### Engine-state rows`.
Files: `src/habu/kernel-x64.f`, `src/arch/x86-64/rt.f`, `test/x86-64-kernel-engine.f`, `docs/x86-64.md`.
Verify: host K3's product engine: `--load test/x86-64-kernel-engine.f`; ThinkPad: the images natively.
Depends: habu-scaffold-the-x86-9af80979 (scaffold). Hand-written through `X64ASM`.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.
