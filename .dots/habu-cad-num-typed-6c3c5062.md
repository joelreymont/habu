---
title: CAD-NUM typed extent division (units-in-extent)
status: open
priority: 3
issue-type: task
created-at: "2026-07-15T10:01:17.714551+02:00"
---

Found by the memnum lane 2026-07-15: CAD-NUM's closed B5.2 algebra has positive-divisor DIV/REM but NO extent-by-extent division ('how many units of size S fit in extent E' - byte-len byte-len -- numeric-result<item-count>), and byte-len -> item-count is checker-rejected by design. Consequence: MEM:64K-COUNT-FOR needs a raw byte read and reuses the mmap-seam projection ALLOC-BYTES>N for its ceil-division, relaxing the B5.5 prose that the projection appears solely at the mmap operand (hard constraints held: exactly two projections, roles can't swap). Fix: add the typed extent-division op (ceil and floor variants, overflow per the table style) to lib/cad-num-arithmetic.f + boundary matrix + manifest rows; then rewrite MEM:64K-COUNT-FOR purely (byte-len 64K-extent DIV-CEIL) and update the TRUSTED.md row note so ALLOC-BYTES>N returns to solely-at-mmap. Acceptance: 64K-COUNT-FOR has no raw read; legacy parity pins stay green (1/64K/64K+1/MAX-N); the new op's zero/max/overflow cases executed. Files: lib/cad-num-arithmetic.f(+test), lib/std.manifest, lib/memory.f 64K-COUNT-FOR, TRUSTED.md note. Verify: cad-num + memory suites, lint-manifest. Ownership: CAD-NUM algebra (coordinate with the seal dot 36dbeec6 - land before sealing).
