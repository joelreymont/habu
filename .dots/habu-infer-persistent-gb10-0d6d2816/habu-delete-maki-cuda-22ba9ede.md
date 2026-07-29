---
title: Delete Maki CUDA shims
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.484052+02:00"
---

Why: maki/cuda-types.f and maki/cuda-driver.f only forward the canonical lib/ptx CUDA package and preserve E-MK-GPU. Result: migrate every production caller and fixture to lib/ptx/cuda-driver.f and E-CUDA, update suite/file inventories, then delete both wrappers and their wrapper-only tests. Owner: the obsolete Maki CUDA entry files and their direct callers only. Production red: two public paths name the same CUDA roles and error. Acceptance: no require or E-MK-GPU reference remains; deleted paths fail to load; canonical CUDA, Maki device, lower-launch, evaluator, suite, package, and file-map gates pass. Forbidden: forwarding file, alias, deprecation period, version check, compatibility name, or unrelated CUDA refactor.
