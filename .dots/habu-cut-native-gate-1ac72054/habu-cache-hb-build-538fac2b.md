---
title: Cache hb-build output artifacts
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-06-29T05:08:04.258164+02:00\\\"\""
closed-at: "2026-06-29T05:31:32.069498+02:00"
close-reason: Implemented final hb-build artifact cache under HABU_BUILD_CACHE. Key uses builder/maker inputs plus stable user-source digest, not temp source path; restores executable before maker, publishes atomically after miss, and trace counters distinguish artifact-hit from maker-run. Verified hb-build-test with and without cache, gate-stats-test, typed-local-diff-lint, focused AOT cold/hot (50.32s -> 12.93s), full post-change cache-fill gate 70224ms, and steady hot full gate 43273ms <= 90000ms with artifact-hit=1.
---

Problem: current hot full gate is green but still 43.668s internal / 46.54s wall; AOT-positive remains 31.356s even with maker-hit=1 because tools/hb-build-lib.f caches only the maker image and still runs the maker to rebuild identical final AOT artifacts each gate. Fix: add a content-hashed final artifact cache under HABU_BUILD_CACHE keyed by bin/hb/build source inputs, target/mode, and generated program source; restore cached executable before HBB-RUN-MAKER while still running lints, and publish stamp-last after a miss. Files: tools/hb-build-lib.f, test/gate-build-hbb.f, tools/hb-build-test.f, test/gate-stats.f if counters are added, docs/stdlib.md or LESSONS.md. Acceptance: cache hit skips maker run for unchanged source, changing source invalidates cache, hb-build CLI semantics preserved, AOT-positive focused time drops materially, full hot gate passes and moves toward 30s.
