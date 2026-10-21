---
title: Reuse direct whitebox builder setup
status: active
priority: 2
issue-type: task
created-at: "2026-10-21T04:17:38.718562+03:00"
---

whitebox-engine-key costs178.83s in the latest gate and copies/changes engine source before every positive provider call, forcing cold support/engine builds. The native-build edit E2E already covers source invalidation. Exercise direct provider publication/reuse/private-copy execution from the unchanged original candidate/root using the ordinary builder cache, clearing bundle selection; assert real internal-word access and whitebox class. Keep copied source/private caches for timeout and missing-input failures. Move the unloaded-file artifact assertion into existing native engine-artifact warm-hit coverage without an extra build. Measure both affected rows before/after, preserve real publication/runtime behavior, independent Astra review. Cold first build remains real; no manufactured cache records or bundle substitution.
