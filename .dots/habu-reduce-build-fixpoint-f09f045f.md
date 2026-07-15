---
title: Reduce build fixpoint gate latency
status: active
priority: 1
issue-type: task
created-at: "2026-07-15T05:40:20.753650+02:00"
---

Problem: on master@origin 4e3a59c9, tools/build-fixpoint-test.f passes alone only after about 161 seconds while test/gate-stdlib.f enforces a 120000 ms phase timeout; two exact runs time out at build-fixpoint-fixtures after load-reject-diag is repaired. Raising the timeout is forbidden. Cause: profile the fresh BF-BUILD-ALL/FIXPOINT path and prove which compiler generations, certification passes, source regeneration, codesign, or cache-key misses dominate. Fix: remove duplicated compilation/checking or restore correct within-attempt cache reuse without weakening fresh-build, byte-fixpoint, certification, boot-pin, or cache-hit evidence. Acceptance: instrumented before/after phase evidence; standalone fixture and full stdlib gate pass below the existing timeout with at least 20 percent headroom on the same host; artifact hashes, cold/fresh semantics, cache counter contract, and negative fixtures remain exact; maki, ptx-stdlib, host-lint, filemap-lint, dot-dep-lint, typed-local diff lint, and full native gate pass. Files: tools/build-fixpoint*.f and the minimal proven compiler/cache owner only. Verify: bin/hb --load tools/build-fixpoint-test.f; bin/hb --load test/gate-stdlib.f. Claim: agent=build-fixpoint-latency workspace=.jj-ws/habu-reduce-build-fixpoint-f09f045f.
