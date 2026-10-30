---
title: Remove environment variables Habu can do without
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T20:39:52.856503+02:00"
---

Joel 2026-10-29: remove env vars except where Habu really needs one and cannot do without it (e.g. HABU_WHITEBOX_IMAGE becomes an argument of the build word in the one-process hb-build; HB_BUILD_TIMEOUT_MS goes with the build timeouts). Census brief /tmp/jerry-big/env-census-brief.md (worker running); each replaceable variable is removed in the lane that owns its code.

Census done: /tmp/jerry-big/env-census.md (raw /tmp/jerry-env/). NEEDED: PATH, TMPDIR, LC_ALL/LANG for foreign children, the gate's signal-fd and gate-wide image variables, HABU_NET_TESTS opt-in, env-API test data and GETENV-as-input fixtures. Replaceable in the one-process build lanes: HB_BUILD_TIMEOUT_MS (goes), HABU_WHITEBOX_IMAGE (build-word argument; cold-boot users bootstrap.sh:535 and cold-naming-test.f:80 need their own carrier), HABU_AOT_INPUTS, HABU_BUILD_COUNTS, HABU_NATIVE_CACHE_DIAG, HABU_OUT, HABU_FIXPOINT_ENGINE, HABU_GATE_COMPILER_BUNDLE. Replaceable elsewhere: HABU_UNDER_TEST (and its seven resolver copies), test-child variables (script arguments or program text), zed/seed/bootstrap switches (flags or arguments).

HABU_GATE_COMPILER_BUNDLE removed by the bundle commit "Build the suite compiler bundle as one engine" (009315dc): the bundle always writes the whitebox engine.
