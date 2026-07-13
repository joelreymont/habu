---
title: Make retained builds transactional
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T03:14:28.580154+02:00\""
---

Full context: pending tools/build-fixpoint.f retained build rename dance can temporarily remove canonical hb-stdin and public BUILD resets caller CLEANUP/BF configuration. Build directly to isolated destinations under a package-owned scoped transaction; save/restore caller context and cleanup frames; canonical input must never disappear.
