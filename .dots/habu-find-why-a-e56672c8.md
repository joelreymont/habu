---
title: Find why a profiled load runs five times slower
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T22:33:55.796309+01:00"
---

Problem: the Tender placement load took 11.2 s under 1000 prof-rate on pulsar for a 2.05 s load, and arm64 recorded 14901 weighed samples (about 14.9 s armed) for the same load, while a warm engine build under tools/build-profile.f costs a tenth more wall time. Hypothesis: every cp!/ndict! lowering runs a full profiler sync (src/habu/prof.f:1507-1567, prof-x64.f:1423-1470). Acceptance: the cause is named with a measurement on both arches, and a profiled load's wall time is within a tenth of its unprofiled time. Depends: none.
