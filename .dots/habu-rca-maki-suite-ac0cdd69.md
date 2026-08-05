---
title: RCA maki suite stop at competitive-evidence
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T18:56:23.094755+02:00"
---

Full context: with the fusion-plan phase green, bin/hb --load maki/test.f now runs 169 suites (up from 37) and stops at maki/competitive-evidence-test.f with 'habu: bad enum declaration ucat: declaration failed at ucat', throw 7169, run exit 67. The file passes STANDALONE (rc 0) on both the pristine and the fixed sources, so this is a cross-suite ordering or shared-state interaction — most likely a duplicate enum row or unretired declaration state left by an earlier suite in the same process — that was simply unreachable before, because the run died at fusion-plan cases 97/98. Root-cause the shared declaration state and decide whether the maki suite runner should isolate it per suite. Do not shrink, skip, or reorder the fixture to get green. Owner: maki/competitive-evidence-test.f plus the maki suite runner.
