---
title: "Core bootstrap: resize source prefix arena"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T18:40:14.505036+02:00"
---

Checker registry migration exhausts the composite IBUFSZ 0x180000 arena:
the measured stage2 source is 1,036,134 bytes and the cold-prefix sources are
544,174 bytes, totaling 1,580,308 bytes, 7,444 bytes above the 1,572,864-byte
cap. `install --force` emits `bin/hb refresh OK`, then the later stage exits 74
with `hb: source prefix buffer full`; the binary replacement is therefore not
a proven install. Enlarge the capacity owner in src/habu/layout.f and its
bootstrap/cg/forth.fs mirror with explicit measured headroom, keep mirrors
equal, derive the oversize regression input from IBUFSZ instead of the stale
0x1A0000 literal, add a near-capacity positive and cap-plus-one labeled-negative
regression, and prove bootstrap/fixpoint byte parity. Do not shrink record
assertions or bypass the fail-closed buffer. Serialize after CELL relocation
because both changes own bootstrap/cg/forth.fs.
