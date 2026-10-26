---
title: Fuse tier 0 compares with branches
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:18.237762+02:00"
blocks:
  - habu-keep-tier-0-bfdcdd19
---

Experiment step after step 2. VCMP-ENTRY (jit.f:516-525) emits cmp;cset;neg then cbz: fuse compare+branch into cmp;b.cond, and use compare-immediate (LVBINIPREP, not LVBINPREP) for literal operands. zz 18%, tender 25% of the tier gap. Measure as step 2.
