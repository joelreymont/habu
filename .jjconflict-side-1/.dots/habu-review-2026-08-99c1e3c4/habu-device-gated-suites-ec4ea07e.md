---
title: device-gated suites pass vacuously and orphan tests exist
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.989707+02:00"
---

Problem: off-device the suite prints SKIPPED and T-REPORT with zero real assertions: maki/device-smoke.f:33-34 (and :30 '0 0= TTRUE' counted as a test), eval/emit-device-test.f:26-28, eval/device-fault-test.f:138-139, infer/gpt2-model-test.f:363, infer/gpt2-pin-test.f:86, eval/live-author-test.f:96; 32 *-device-test.f files are in no slice; 5 host tests scheduled nowhere (maki/db/budget-dim-test.f, budget-ledger-test.f, capability-test.f, commit-store-auth-test.f, maki/gpu-session-test.f). AGENTS.md: red, skipped or unrun means no merge - the runner does not report skipped as such. Acceptance: the runner counts SKIPPED as a distinct outcome and the merge gate fails on it unless the device slice ran (recorded where); the tautology removed; the 5 orphans scheduled or deleted; the 32 device tests listed in one device slice with its host requirement. Files: maki/test.f, lib/test/suite.f, the listed tests. Verify: maki/test.f output names the skipped count. Depends: none. Ownership: maki gate. Claim: unassigned.
