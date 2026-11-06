---
title: "Cut the store compare's copy of the space below each file"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T23:38:09.864291+02:00"
blocks:
  - habu-file-objects-l2e-87cba73d
---

Problem: RUN-FILE's COPY-BELOW (src/habu/linker.f) copies, for every file a build compiles, the space below the file's data less two bands, and FIND-STORES compares it after the compile: about 2.5 MB of header ([0, XTCELL-N-CELL) is about 2.3 MB, of which the 2 MiB CALLMAP/ADDRMAP band [CALLMAP-OFF, ADDRMAP-END) changes through no named word, only through a plain store to a computed address) plus every earlier file's data, so a cold build of F files copies F x 2.55 MB plus a sum quadratic in its data. At about 0.9 GB/s (STR= is a Habu cell loop, src/lib/string.f:83) that is about 3.4 ms per file on test/file-objects.f's fixture; no real build runs inside LINKER:WITH yet. The L3 review (carl-l3/.out, 2026-11-06) found the header band can change: ROUTE-HEADER (habu2.f:8069) sends xt! and ptr-cell! stores below DATA-START into the space, and each such store marks its cell, so the rows name every cell a named word changes there. Acceptance: measure, on the first real build through the linker (a library or hb's own files, once L2e lets a build's file require), the copy's and compare's share of the build's wall and CPU time, on spark (DECISIONS 'where timings run'); if it is material, cut it without losing a refusal test/file-objects.f checks (STOP-GAP's plain store into a gap must still be refused), for example by comparing only the rows' cells in the header's lower band. Files: src/habu/linker.f. Verify: test/file-objects.f, test/linker.f, arm64-link-data.f, arm64-link-run.f; the measurement before and after. Depends: L2e (a real build through the linker). Ownership: carl (lead). Claim: unassigned.
