---
title: Write doctored images atomically
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T03:14:28.588471+02:00\""
---

Full context: pending tools/image-doctor.f WRITE mutates the destination non-atomically. Write to a same-directory isolated file, fsync as required, rename atomically, and preserve the prior image on every failure. Add interruption/failure regressions.
