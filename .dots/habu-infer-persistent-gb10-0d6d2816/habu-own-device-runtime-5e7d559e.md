---
title: Own device runtime core
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T00:55:48.211793+02:00"
blocks:
  - habu-delete-maki-cuda-22ba9ede
---

Why: CUDA scope and stream lifetime are shared by both explicit model arms and must not be mixed with model modules or workspace geometry. Interface: package DEVRT defines one private linear core containing the canonical CUDA scope, target, stream, and monotonic session generation; OPEN-CORE publishes only after all acquisition succeeds and CLOSE-CORE synchronizes then releases in reverse order. Owner: device runtime core lifetime only. Production red: existing launch code creates scope state per operation. Acceptance: two cores coexist; every acquisition or close refusal preserves exact ownership; generation is unique and no model module or buffer is allocated. Forbidden: model selector, module registry, plugin, global handle, second CUDA scope, historical CUDA forwarding package, version, or compatibility wrapper. Smallest owning check: bin/hb --load maki/infer/device-runtime-core-test.f on DGX Spark.
