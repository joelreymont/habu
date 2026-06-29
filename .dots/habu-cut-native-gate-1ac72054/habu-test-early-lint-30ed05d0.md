---
title: Test early lint-tools scheduling
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-06-29T03:26:54.868018+02:00\\\"\""
closed-at: "2026-06-29T03:28:36.318467+02:00"
close-reason: "rejected: early stdlib-lint-tools regressed hot full gate to 43.632s internal / 46.46s wall by slowing warm/check/manifest/libs and AOT-positive; code reverted, evidence kept"
---

Problem: after guarded early manifest/libs, hot full gate remains ~43.26s internal / 46.29s wall. The late stdlib-lint-tools tail still finishes near the end (~9.8-10.6s once launched), while hot early schedule has an unused outer slot after runner cache hit. Fix: empirically test starting phase 17 (stdlib lint tools) in TR-TRY-EARLY-LINTS only when Habu-under-test is already ready, skip its late duplicate, and keep only if full hot gate improves without worsening counts or causing contention. Files: test/run.f, dot checkpoint, LESSONS.md if accepted. Verify: focused/full hot native gate and commit gates before master.

Rejected 2026-06-29: starting phase 17 early filled the initial pool and slowed
the warm/check/lint-manifest/lint-libs front half. Hot full gate regressed to
43.632s internal / 46.46s wall; checker warm rose to 8.619s, manifest to
9.247s, libs to 9.421s, and AOT-positive to 31.974s. Reverted the code change.
