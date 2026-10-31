---
title: Probe DATA layout after a literal-pool opening
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T18:12:50.766588+02:00"
---

Problem: 11D2 review (vltnxqmk) residual risk: within a segment the DATA layout depends on the literal pool's fill at file start, which no key records. Checker-slice references to a file's own DATA (CHECK-TARGET o:2574) map linearly from the segment's live start (MAP-THROUGH o:3487) with no check of live against stored layout. 11D2 removed the literal-pool refusal, so an object whose cold run opened a pool segment is now stored; a warm run whose pool has room may place later DATA differently. Acceptance: a reproduction (a file whose cold run opens a new literal-pool segment, warm with a pool that has room) shows whether checker-slice targets or the product differ; differ -> a fix dot keyed on the cause; equal -> close with the evidence. Files: tools/native-unit-object.f. Verify: the reproduction, cold vs warm. Depends: the merged lane-11 stack (carl-l11m). Ownership: none (probe). Claim: unassigned.
