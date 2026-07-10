---
title: Restore AOT preseed cache hit
status: active
priority: 2
issue-type: task
created-at: "2026-07-10T17:51:50.772753+02:00"
---

Full context: master e5e1d362f694 and fix-forth-style a538da294e5e both fail test/gate-aot-positive-lib.f:204-228 at HBB-OBJECT-HIT after the --json artifact-key-only rebuild. Exact repro: run the gate with test/gate-common.f, build libraries, test/gate-build-common.f, test/gate-build-hbb.f, then test/gate-aot-positive.f; it exits 1 with 'FAIL: hb-build AOT preseed object-cache hit' while the produced image correctly exits 85 with 'hb: bad gemt tag'. Baseline proof: the same focused gate fails on an isolated workspace at master e5e1d362f694, so the Forth-style commit is not causal. Cause to establish: why the object cache does not report/reuse the preseeded object when only --json changes the artifact key. Fix the cache-key/index/restore invariant at its owner; do not relax the assertion. Verify: focused AOT-positive gate, bin/hb --load test/run.f, bootstrap fixpoint, host-lint, and filemap-lint all pass on the exact intended tree.
