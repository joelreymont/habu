---
title: Report census count and baseline together
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T22:37:27.009781+02:00"
---

Full context: src/compiler/numeric-policy.f and src/compiler/target.f landed at commit 254b12e231f4 with five and four new ENUM declarations and never re-recorded tools/enum-census-baseline.txt. The staleness stayed hidden for a day because REQUIRE-WALKED throws inside WALK, which runs BEFORE VERIFY, so the walked-file count guard masked the baseline divergence entirely; it surfaced only when an unrelated lane fixed the count. Two fixes: report both conditions in one run instead of letting the count mask the baseline, and give a lane that adds an ENUM declaration a check it can run against its own diff so the re-record is its own responsibility rather than the next lane's surprise. Depends on the content-determined report work, which makes the re-record reviewable.
