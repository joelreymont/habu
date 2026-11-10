---
title: "Drop a refused definition's shadow rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T13:51:16.785778+02:00"
---

Problem: a section refused after it filed shadow rows (the native shadow's map from a dictionary record to the emission a cross-build ships) leaves them filed. src/compiler/native/shadow.f PUBLISH (:457-466) only raises N-RECS and nothing short of CLEAR (:283) lowers it, so the refused section's rows keep naming the record index its rollback gave back, and the word later made at that index gets a second row. The linker's SHADOW-ROW (src/habu/linker.f:1124-1135) walks rows in publication order and returns the first row naming an index, which is the stale one. A cross-build stops on a refusal, so this bites only a refusal caught inside product text. test/compiler/shadow-kinds.f measured `stale shadow rows: 2` (its STALE word, :133-137, counts them; FINISH, :214-215, does not fail on them).
Acceptance: a refused section's shadow rows are given back with the records its rollback gives back, so no row names a rolled-back record; test/compiler/shadow-kinds.f prints `stale shadow rows: 0` and fails when the count is not zero.
Files: src/compiler/native/shadow.f, the rollback that gives back the records (per the cause), test/compiler/shadow-kinds.f.
Verify: on the arm64 host, bin/hb --load test/run.f -- compiler-shadow prints `stale shadow rows: 0` and passes.
Depends: none.
Ownership: unassigned. Claim: unassigned.
