---
title: Green aot-sig-pool and app-image
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-12T18:21:57.352577+03:00\""
---

Problem: test/aot-sig-pool.f F21 'the checker defines the mirrored constant exactly once' expects 1 and gets 0, F22 expects 168 and gets 0; test/app-image.f's first failure is an app-image child killed by SIGILL (signal 4) with a habu-crash register dump, followed by -2500 E-PROC-SPAWN. Both red on the root 2026-09-12 (recorded set of 15 in LESSONS.md), both undiagnosed, distinct roots. Acceptance: each failure reduced and attributed to the responsible layer (declaration, checker, compiler, runtime, capture, or fixture) and fixed there with a regression that fails before and passes after; both suites green on a cold-built engine; two same-host builds byte-identical and tools/two-generation-build.f gen 5 = gen 4. Files: by diagnosis, expected under src/habu/aot-*.f, src/habu/habu2.f, src/core/checker.f, test/aot-sig-pool*.f, test/app-image*.f. Verify: the two suites, test/run.f. Depends: none. Ownership: hazel. Claim: agent=hazel-worker workspace=.jj-ws/habu-green-aot-sig-d88e5532.
