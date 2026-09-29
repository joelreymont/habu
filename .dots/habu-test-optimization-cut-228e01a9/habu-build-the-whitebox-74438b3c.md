---
title: Build the whitebox engine beside the suite pool
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-29T13:58:20.606992+02:00\""
---

Problem: SUITE-SETUP (test/gate-stdlib-lib.f:108-113) runs COLD-ENGINE:ENSURE and WB-ENSURE before the pool starts any row. The whitebox native build took 84 s of the 787 s gate (hb-whitebox artifact mtime 13:09:05 against the log birth at 13:07:26), and only WHITEBOX-SUITE rows use its engine (SUITE-WB-RUN, gate-stdlib-lib.f:97-99). Its key changes whenever bin/hb or the prefix closure changes, so a development gate pays it every time. Acceptance: the pool runs non-whitebox rows while the whitebox engine builds; no whitebox row starts before a successful build; a failed build turns every whitebox row red with the build's output and exit status, never a silent skip; the cold engine stays ready before the first fork (gate-stdlib-lib.f:105-107). Design before dispatch: record the chosen mechanism (for example a pool slot for the build and a wait for it before the first whitebox spawn) in this dot. Files: test/gate-stdlib-lib.f, test/whitebox-engine.f, test/gate-pool.f as the design requires. Verify: the gate log shows pool rows passing before the whitebox build completes, and a forced build failure makes the whitebox rows red. Depends: none. Ownership: gate setup and whitebox spawn. Claim: agent=kestrel workspace=.jj-ws/whitebox-overlap.
