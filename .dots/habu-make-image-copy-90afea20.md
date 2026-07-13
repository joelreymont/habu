---
title: Make image copy overlap-safe
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T03:14:28.585709+02:00\""
---

Full context: pending tools/image-doctor.f COPY always moves forward and corrupts right-overlapping ranges. Implement typed memmove direction or explicitly reject overlap before mutation; add both overlap-direction regressions.
