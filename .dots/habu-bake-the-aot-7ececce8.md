---
title: "Bake the AOT DATA window's content"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T18:38:00.849316+02:00\""
---

EM-AOT-RELOC-DATA (habu2.f:3853) reserves the captured DATA span as ZEROED anon-mmap and never copies content, so any initialised DATA in a capture window silently arrives as zeros in the seeded engine (aotsite lane 2026-08-11, reproduced). Replace the zeroed reserve with a copied blob of [d0,d1). Independent of the site record. Acceptance: an initialised cell in the window reads its value after capture+boot; the zeroed-reserve mutation reds it. Files: src/habu/habu2.f. Depends: habu-per-site-relocation-bb9b6d70.

Claim: agent=aotsite workspace=.jj-ws/habu-aot-persite

RULE DISCOVERED WHILE IMPLEMENTING (aotsite lane 2026-08-11), and the leaf's
"copy a blob of [d0,d1)" is incomplete without it. The window is NOT all inert
bytes: a `defer` compiled inside it allots a dispatch cell registered in
SNAP-RELOC's XTCELL table, and that cell holds a code address in the BUILDING
host. Baking it verbatim makes the image depend on the run that produced it -
measured: two builds of identical source differed by 34 bytes (2 of cell plus
the 32-byte signature). So: declared address cells are ZEROED in the image,
their window offsets travel in a table beside the content, and the seed writes
the booting engine's own `defer-unset` xt into each one before the boot-run
installs the real vectors. The boot-run owns the real value; the trap xt is what
a cell keeps if the boot-run has no entry for it.
ZERO IS NOT THE MASK VALUE, and that was the trap. A dispatch cell is read and
branched to (`ldr x16,[x9]; blr x16`), so a zero cell is a jump to address 0 -
measured as SIGSEGV, rc 134, no diagnostic. `$46` is the COMPILE-time defer/is
token error, not a runtime unset guard; the runtime guard is the xt of
`DEFER-UNSET` sitting in the cell, dying `$4C` "defer: unset execution vector".
The seed resolves it through the same LFIND-by-keyword the compiler uses, so
one authority decides what "unset" means.
ACCEPTANCE VEHICLE: the AOT seed is armed at the INTERACTIVE REPL ENTRY only
(AOT-SEED-ARM-CELL), so nothing about it is observable from a piped or --load
boot - a fixture must be PTY-booted. Cases live in test/aot-data-span-forge.f,
which already owned that constraint.
