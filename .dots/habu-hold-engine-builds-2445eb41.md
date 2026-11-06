---
title: Hold engine builds in tools and tests to 5 minutes
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T16:57:03.694894+02:00"
---

Problem: six engine-build limits exceed 5 minutes: 30 minutes (1800000 ms) at tools/chain-run.f:54 BUILD-IN, tools/two-generation-core.f:30 TG-BUILD-MS, test/product-host-identity.f:22 and test/host-checker-row-e2e.f:22 BUILD-TIMEOUT-MS; 10 minutes (600000 ms) at tools/hb-build-lib.f:43 HBB-MAKER-DEFAULT-MS (every engine build through hb-build or native-build.f without a preload) and test/engine-stack-machine.f:15 BUILD-MS. DECISIONS 'build cost' (Joel 2026-11-06): an engine build takes at most 5 minutes and no limit is raised to get a build past it; a slow build is root-caused at once. Measured 2026-11-06 at load 10-20: chain builds 101 s, 124 s, 43 s; at load 25-34 the same first build took 339 s wall for 97 s user CPU (28% of a core: the Mac was oversubscribed, a runaway Sublime crash_handler took 1.8 cores). Acceptance: each limit is 300000 ms, its comment states the 5-minute rule; a build that overruns is root-caused, not given more time. Files: the six above. Verify: tools/chain-run.f one generation; test/product-host-identity.f and test/host-checker-row-e2e.f pass. Depends: none. Ownership: those six files. Claim: unassigned.
