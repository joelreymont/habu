---
title: Size the native trap table to its compilation
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T11:04:22.779487+02:00"
---

After the tier-1 name cap went (3d1ce798, 92797093), src/compiler/native/trap.f stores each no-return callee's name whole in a fixed 32 KB ARENA-CAP with ROWS-MAX rows, so four 7000-byte no-return callees fill it and the fifth tier-1 caller fails -8642 while tier 0 runs the program (probe $HOME/.cache/tmp/kestrel-r4-namecap/z/p/arena1.f); short names already hit the 1024-row limit. The table's ordinals never leave a compilation (TRAP-ARGS compiles the message bytes, not the number), so the table can be per-compilation and grow, or go. Acceptance: tier 1 compiles every program tier 0 runs with any count and length of no-return callees, failing first through test/compiler/native-trap.f; trap.f's header comment (lines 7-8) describes the result.
