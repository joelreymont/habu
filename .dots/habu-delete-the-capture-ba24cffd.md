---
title: Delete the cell-grid heap form when x86-64 uses the linker
status: open
priority: 4
issue-type: task
created-at: "2026-11-09T17:59:25.302424+02:00"
---

Problem: nothing writes the heap's cell-grid form. HEAP-GRID (src/habu/snapshot-format.f:22) is read only by the x86-64 snapshot readers src/habu/snap-decode-x64.f:61-192, src/habu/boot-x64.f:194-196 and src/habu/snap-x64.f:23, and src/habu/cell-grid.f's words only by snap-decode-x64.f and tools/data-table-census.f.

Acceptance: when x86-64 builds through the linker (docs/design.md "Machinery being replaced"), the grid form goes with its readers and their citations, unless the x86-64 snapshot writes it; rg finds none of its names over src tools test, and the x86-64 build rebuilds itself byte for byte.

Depends: x86-64 on the linker (Joel 2026-11-09: "forget about x86, get arm64 done.").

Claim: unassigned.
