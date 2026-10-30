---
title: "red cv: check-verify trusted-tick-order on the stack"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T21:29:36.632479+02:00"
closed-at: "2026-10-31T12:12:25.069589+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510 on the final candidate"
---

Problem: TRUSTED-TICK-ORDER reports E-UNDEFINED at the later token instead of E-CAP-TRUSTED at patch32 on yuxsxwrk. Acceptance/Files/Verify: /tmp/jerry-big/red-cv-brief.md. Depends: none. Ownership: the brief's files. Claim: agent=worker workspace=.jj-ws.noindex/jerry-cv

2026-10-30: vxmotwvu "Expect T0's checking order in trusted-tick-order" (tools/check-verify-test-lib.f TRUSTED-TICK-ORDER expects E-UNDEFINED at CVT-NOT-DEFINED col 41: zxwoonrk made tier 0 check the whole body first, as tier 1). Row rc 0. Awaiting the batched red review.
