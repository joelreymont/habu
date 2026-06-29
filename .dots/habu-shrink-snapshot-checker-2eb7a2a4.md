---
title: Shrink snapshot checker arenas
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-29T14:20:45.770502+02:00\""
---

RCA 2026-06-29: bin/hb grew to ~22MB on macOS and Linux because snapshots write live data through HERE and src/core/checker.f reserves USIGS-BOOT with create ...  allot. Actual UEND must be measured; correct fix is not timeout loosening. Move growable checker arenas out of persistent HERE where possible or size boot snapshot storage to actual certified state, keep fail-closed capacity checks, then rebuild bin/hb and verify imgdump/imagedisasm plus full gate. Zed has working ~/Work/habu/bin/hb; do not bootstrap unless bin/hb is missing.
