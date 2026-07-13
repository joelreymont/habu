---
title: Propagate build close errors
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T04:49:18.301035+02:00\""
---

Full context: tools/build-fixpoint.f drops the source/output close status on the normal success path, so late writeback failures can report a successful certified build. Split checked normal close from cleanup-only close, preserve primary plus cleanup outcomes, and add injected close-failure regression. Group with active build-fixpoint hardening dots.
