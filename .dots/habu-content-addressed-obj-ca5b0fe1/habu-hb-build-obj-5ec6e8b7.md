---
title: hb-build object cache hit path
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T22:27:42.383398+02:00\\\"\""
closed-at: "2026-07-01T22:34:41.710693+02:00"
close-reason: "Implemented hb-build AOT object-cache hit path. Proof: bin/hb --load tools/hb-build-test.f; object-resolve/image/link focused tests; typed-local-diff-lint; stdlib-manifest-test; filemap-lint; host-lint; trust-lint; stale-status-lint; lint-artifacts-fast; full native suite 25232ms <= 40000ms."
---

Problem: content-addressed object records can be stored/resolved/written as native images, but hb-build still falls through from artifact-cache miss to maker rebuild, so cached objects are not usable. Files: tools/hb-build-lib.f, tools/hb-build-test.f, test/run.f if needed. Fix: for non-REPL AOT builds, after artifact-cache miss, compute source SHA256, resolve an object by source+target/checker/compiler ABI through OBJRES, write it through OBJIMG to HBB-OUT$, install artifact, set trace, and fail closed on stale/corrupt/wrong-key objects while missing index falls through to maker. Acceptance: focused test proves object hit writes runnable image and does not run maker; wrong/stale object errors do not silently fallback; native lints and full test suite pass.
