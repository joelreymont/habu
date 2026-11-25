---
title: "Read the target's table offsets in the linker"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T19:13:21.492917+01:00"
---

Problem: src/habu/linker.f LAY-ADDRESS-HEADER and BELOW and src/habu/link-copy.f HEADER-FIELD! and KEEP-LIST place the target's address-cell tables at the running hb's own offsets (XTCELL-N-CELL, XTCELL-END), so no change can move them: with the site maps gone, a 2 MiB hole stays below XTCELL-N-CELL (src/habu/layout.f), since an hb with the old offset rejects the new DATA-START (LINKER:E-HEAP-STORE, -9692). Acceptance: the kernel object states the offsets and the linker reads them as it reads DATA-START; then XTCELL-N-CELL is USE-BAND-END and the hole is gone; chain-run-build reaches a fixpoint and the x86 engine is no larger. Depends: none. Claim: unassigned.
