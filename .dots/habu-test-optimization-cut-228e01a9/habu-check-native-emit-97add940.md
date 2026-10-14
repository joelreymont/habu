---
title: Check native-emit.f in the order its load runs
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T12:35:54.026461+02:00"
closed-at: "2026-10-15T01:26:46.877457+03:00"
close-reason: Completed or superseded on current master. vovrlkv loads image bytes before target writers and records checker success for native-emit; check-test has the consumer case. Read-only source/history evidence checked in backlog review; no new code or broader runtime claim.
---

Problem (lane 295 dupdiag): bin/hb --load tools/check.f -- tools/native-emit.f exits 70 E-UNDEFINED MSIZE (src/os/macos/macho.f). check.f expands a body's loaders right after that body; tools/native-emit.f:18 LOAD-IMAGE (inside package NATIVE-EMIT) expands at the first ;package (:39), before 'require src/os/image-bytes.f' (:46), while the real load runs it later through "' LOAD-IMAGE ;package execute" (:50-51). The same model forced tools/object-image.f to reorder its loaders (a4da213d). Acceptance: check.f accepts every file whose load passes and refuses with a located diagnostic otherwise; decide whether native-emit.f states its load order plainly (load image-bytes.f before the body that needs it, as object-image.f now does) or check.f follows execution order; tools/check.f -- tools/native-emit.f rc 0, seen failing first. Files: tools/native-emit.f or tools/check-core.f.
