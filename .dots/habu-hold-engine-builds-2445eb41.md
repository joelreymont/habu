---
title: Hold engine builds in tools and tests to 5 minutes
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T16:57:03.694894+02:00"
---

Problem: four engine-build limits are 30 minutes (1800000 ms): tools/chain-run.f:54 BUILD-IN, tools/two-generation-core.f:30 TG-BUILD-MS, test/product-host-identity.f:22 and test/host-checker-row-e2e.f:22 BUILD-TIMEOUT-MS. DECISIONS 'build cost' (Joel 2026-11-06): an engine build takes at most 5 minutes and no limit is raised to get a build past it; a slow build is root-caused at once. Measured 2026-11-06 at load 10-20: chain builds 101 s, 124 s, 43 s. Acceptance: each limit is 300000 ms, its comment states the 5-minute rule; a build that overruns is root-caused, not given more time. Files: the four above. Verify: tools/chain-run.f one generation; test/product-host-identity.f and test/host-checker-row-e2e.f pass. Depends: none. Ownership: those four files. Claim: unassigned.
