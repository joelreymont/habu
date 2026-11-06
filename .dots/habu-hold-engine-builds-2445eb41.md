---
title: Hold engine builds in tools and tests to 5 minutes
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T16:57:03.694894+02:00"
---

Problem: six build limits exceed 5 minutes: 30 minutes (1800000 ms) at tools/chain-run.f:54 BUILD-IN, tools/two-generation-core.f:30 TG-BUILD-MS, test/product-host-identity.f:22 and test/host-checker-row-e2e.f:22 BUILD-TIMEOUT-MS; 10 minutes (600000 ms) at tools/hb-build-lib.f:43 HBB-MAKER-DEFAULT-MS (every engine build through hb-build or native-build.f without a preload or `--target`). test/engine-stack-machine.f:15 BUILD-MS (600000 ms) bounds the stripped application's hb-build, not an engine build, and is held to the same limit. DECISIONS 'build cost' (Joel 2026-11-06): an engine build takes at most 5 minutes and no limit is raised to get a build past it; a slow build is root-caused at once. Measured 2026-11-06 at load 10-20: chain builds 101 s, 124 s, 43 s; at load 25-34 the same first build took 339 s wall for 97 s user CPU (28% of a core: the Mac was oversubscribed, a runaway Sublime crash_handler took 1.8 cores). Acceptance: each limit is 300000 ms, its comment states the 5-minute rule; a build that overruns is root-caused, not given more time. Files: the six above. Verify: tools/chain-run.f one generation; test/product-host-identity.f and test/host-checker-row-e2e.f pass. Depends: none. Ownership: those six files. Also tools/check-verify-core.f VFY-BUILD-MS, whose comment ties it to HBB-MAKER-DEFAULT-MS. Claim: agent=carl workspace=.jj-ws.noindex/carl-limits.

Proof (2026-11-06, run in carl-limits on the lane's tree c73c4135 with bin/hb a copy of the installed hb): tools/chain-run-build.f rc 0, fixpoint at generation 2, 197 s wall 173 s user; test/host-checker-row-e2e.f rc 0 (325 s wall at load 13-17); `bin/hb --load test/run.f -- engine-stack-machine two-generation-fixtures check-verify` PASS, 179 s wall. test/product-host-identity.f could not run: it stops before building on a rotted anchor (dot habu-delete-the-rotted-15feb716).

Review (code-reviewer, 2026-11-06): limits right; engine-stack-machine's comment and the docs/gate.md sentence corrected; native-layout-transition.f:16, already 300000, gained the pointer comment. The paths no per-build limit bounds (native-build.f's support image and engine child timed apart, in-process builds, recovery stage runs, HB_BUILD_TIMEOUT_MS up to 2^31-1) belong to the capture build the cross-build replaces: an acceptance case of habu-build-every-product-60ae7d1d.
