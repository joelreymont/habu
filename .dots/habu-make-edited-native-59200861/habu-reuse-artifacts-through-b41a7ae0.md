---
title: Reuse artifacts through the native entry
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T18:23:42.115270+03:00"
closed-at: "2026-10-19T18:40:38.199507+03:00"
close-reason: "The legacy native-build entry now restores the existing unchanged engine artifact just like hb-build --engine, retaining source-loaded/preloaded-backend behavior and edited-source checks. Existing warm E2E passed: identical executable/map and runnable values, artifact-hit marker, repeat 4.190s wall on the loaded host. Independent Astra review cleared the production delta and final source-action assertion; that final assertion was reviewed statically, not rerun. No new cache or force option."
---

The ordinary native-build.f compatibility entry disables HBB-RESTORE-ARTIFACT? solely because HBB-ENGINE-COMPAT is set, even after source inputs match. Same host/source measured10.40 CPU seconds with13299 definition hits versus0.76 CPU seconds through hb-build --engine restoring the identical executable/map. Both spellings must use the same safe unchanged-artifact path; preserve compatibility output/status and the source-loaded/preloaded-backend route. Revise the existing native-build-edit WARM check before code to require the artifact hit plus identical runnable product/map; edited-source and prefix reuse checks stay. Verify that focused existing warm case and get independent Astra review; no new force flag or cache mechanism.
