---
title: "Claim regalloc's DATA tables in data-claims"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T07:10:01.012544+02:00"
---

Problem: src/habu/data-claims.f has no rows for src/habu/regalloc.f's VRTAB-OFF $3600..$3620, VRITAB-OFF $3620..$3640 and FRFREE-CELL $36B0, and its header names only VRFREE-CELL ($208) as left out, so CLAIMS-ASSERT cannot refuse an overlap with those bands and the header understates what is unclaimed (found by the r4-mirror lane while matching the mirror's band table to native). Acceptance: every DATA offset regalloc.f declares is a claims row, or the header names each one left out with the measured reason; a forced overlap with VRTAB-OFF in a scratch copy is refused at build; the engine converges (g1/g2 cmp). Files: src/habu/data-claims.f, src/habu/regalloc.f or src/habu/layout.f if a declaration must move so the table can see it. Verify: the forced overlap, native build g1/g2 cmp, two-generation build. Ownership: native DATA claims for regalloc tables.
