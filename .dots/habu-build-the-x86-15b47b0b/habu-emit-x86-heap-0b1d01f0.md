---
title: Emit x86 heap, printer and hook rows
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T09:22:47.260112+03:00"
blocks:
  - habu-size-the-x86-fb635c93
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

Preflight corrections (Fable, 2026-09-30; these override the lines above where they differ):
- `(GENIO-OUT)` belongs to this leaf. Its ARM64 form is `EMIT-GENIO-OUT` (`habu2.f:1340-1376`), registered `(GENIO-OUT)` through `HELPER-REGISTER` and reached by label from `rt.f:182-195`: x0 = `OUT-CELL` index, x1 = span, x2 = length; it writes to fd 1 when `GENIO-ABI:BUSY-CELL` is nonzero, the index exceeds `DEVICES` (8) or row `[WRITE-OFF + 8*(index-1)]` is zero; else it saves `ACTIVE-CELL` in the frame, stores the index there, sets `BUSY-CELL` = 1, pushes span and length on the data stack, calls the row's xt, clears `BUSY-CELL` and restores `ACTIVE-CELL`. `X64KERNEL:HELPERS,` emits its twin (rdi index, rsi span, rdx length; the same rules; registered `(GENIO-OUT)`) at a label `X64RT` declares as `LGENIOOUT`'s twin; `G-OUT` loads `OUT-CELL` through rbp, takes the `write(1)` path on zero and calls the label otherwise. Case: `OUT-CELL` = 1 through `CELL!,`, row 0 pointing at a stub that pops the span into scratch cells.
- `DP-CHECK`'s ceiling is `DATA-SIZE PROF-CNT-BYTES -` (`habu1.f:1706-1718`) with the target's `DATA-SIZE` (`src/os/linux-x86-64/layout.f:17`), not the host's (macOS `src/os/macos/layout.f:10` differs): `kernel-x64.f` includes `src/os/linux-x86-64/layout.f` in a private section as `boot-x64.f:32` does.
- The `LDPBAD` refusal writes the text of `habu2.f:9979-9985` (head, refused DP, ` of `, ceiling, ` bytes`) with both numbers through a kernel-local fd-2 unsigned writer, then exits 76.
- Fixed fd-1 bytes for the ThinkPad check: `.` writes the digits then LF (`rt.f:207-209`): `-9223372036854775808`, `9223372036854775807`, `0`, `-1`; `u.` of -1 is `18446744073709551615`.
- ARM64 twin lines on master: `BHERE` 1685, `DP-CHECK` 1706, `allot..c,` 1719-1736, `BTYPE` 1737, `. u.` 440-444, `emit cr space .s depth` 1453-1477, hooks 2818-2990, getters 1297-1298. `S0-CELL` is `STACK-ABI:BASE-CELL` (`layout.f:469`), filled by the boot (`boot-x64.f:116`).
- Tests follow master's convention (`505b7f52`): single positive images, no per-image negative twin. Host engine: master's product `5f4d3321…`.
