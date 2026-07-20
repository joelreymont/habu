---
title: Fix runner-entry artifact build on macOS
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T00:27:42.436210+02:00\""
---

Master red on macOS since the decl-event landing 8763905f: test/gate-runner-entry-test.f fails asserts 2 (expected exit 64, got 1) and 3 (expected true, got false), deterministic standalone and in-pool, and test/run.f reds the stdlib/tail-process group with 'Habu-under-test build artifact missing' (RC_RUN=1). Green at 5ad22cff with engine 987eeb27; red at 8248a9d5 and on the envleak train tree with engine c53c2766 (install --force x2 byte-identical, so the installed engine itself is consistent). The habu-under-test candidate build inside the test exits 1 instead of producing the artifact - suspect one of the four source-assembly paths touched by 8763905f (src/habu/habu2.f, bootstrap/cg/forth.fs, tools/build-fixpoint.f, tools/bootstrap.sh) breaks on macOS for the runner-entry build variant. Build-the-tool rule applies: capture the candidate builder's stderr/exit through a reusable probe (WHY-THREW or the gate's kept logs) before editing anything; no print-bisecting. Fix the root cause in the assembly path; test expectations (exit 64 usage contract, artifact presence) stay as-is; add the missing macOS regression if the path lacked one. Evidence logs: battery b2853qq60, capture root /var/folders/98/l2ptpkyn41q7d3sp6x4xp87m0000gn/T//hb-gate-2272976663770666-7 (pool-0-15-24-*.log).

Claim: agent=runentry workspace=.jj-ws/habu-fix-runner-entry-f698bb48
