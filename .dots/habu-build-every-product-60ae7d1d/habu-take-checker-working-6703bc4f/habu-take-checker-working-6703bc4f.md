---
title: Take checker working state out of DATA
status: open
priority: 1
issue-type: task
created-at: "2026-10-26T15:50:44.404285+02:00"
---

Design /home/joel/.cache/tmp/carl-file/checker-scratch-design.md (approved by Joel 2026-10-26, 'Spans + region'; §3B deferred). Checker per-check scratch is declared inside CHECKER-REG:SCRATCH{ / }SCRATCH spans and per-definition arenas live in one checker bump region; one retire before every IMAGE-TAKE and IMAGE-DIFF unmaps the region and zeroes the spans, so no file object compares or carries checker residue. Acceptance: census (loader-common.md) shows no checker site; frame and slots e2e no new TFAIL; full suite and convergence at the tail. Children are the lanes.

carl 2026-10-29 (lane 3c review): PRM-FIRST is now an earlier-write site in 28 files (was 9). Lane 3c rests HIDX-VALID at every file boundary, so PRIM-FIRST-SCAN (checker.f ~10604, ~10608) writes PRM-FIRST on the first primitive lookup of each interval. One span line or a local clears all 28; its other reader is VIS-PICK save/restore (~22359, ~22382). TOP-* stays at 9 files.

Census 2026-11-04: prerequisite of the cross-build (design /tmp/jerry-big/design-xbuild.md): while a target space is open, DP points into the product, so any checker write at `here` lands in product DATA. Checker working state outside DATA stays required; the retire before IMAGE-TAKE and IMAGE-DIFF goes with the window build.

2026-11-09 (carl): was active with no claim and no lane; set open. Claim: unassigned.
