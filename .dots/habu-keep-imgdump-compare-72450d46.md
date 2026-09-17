---
title: Keep imgdump-compare inside its pool budget under load
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T15:00:11.387351+03:00\""
---

Problem: the imgdump-compare suite (test/gate-stdlib-cases.f:50 -> tools/imgdump-test.f) recompiles imgdump in every child it spawns and gives each child a 60 s deadline (tools/imgdump-test.f:18 IDT-TIMEOUT-MS) and the self case 240 s (:22). At pool load 27-33 the gate reds it two ways: chain AB hit the pool's 360 s budget (kind=TIMEOUT-UNDER-LOAD sat=1/6 waits=71 ran=360102ms) and chain AC hit a child deadline, which surfaces as an uncaught -2502 E-PROC-TIMEOUT (exit 67, kind=exit, unclassified, no output). Alone on the same engine at load 14 the suite is green in about four minutes. A test-internal deadline expiry under a saturated pool is a load artifact the pool cannot see, so every gate under load has to be re-run by hand. Acceptance: the suite finishes well inside the pool budget under a full pool (compile imgdump once per run and share the checked image across the cases instead of once per child; give the cases that only differ in argv one compiled child) and an inner deadline expiry reports itself as a classified load outcome (the pool's TIMEOUT-UNDER-LOAD path with its saturation suffix) instead of exit 67; regression: a gate-pool-test case where a suite's own E-PROC-TIMEOUT under saturation is classified, never kind=exit. Files: tools/imgdump-test.f, test/gate-pool.f, test/gate-pool-test.f. Verify: bin/hb --load tools/imgdump-test.f; bin/hb --load test/gate-pool-test.f; bin/hb --load test/run.f under load. Depends: none. Ownership: tools/imgdump-test.f, test/gate-pool.f. Claim: agent=hazel-imgdump-load workspace=.jj-ws/hazel-imgdump-load.
