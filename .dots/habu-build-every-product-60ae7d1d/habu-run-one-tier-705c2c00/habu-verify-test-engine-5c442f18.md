---
title: Verify test/engine-suite.f on the release engine
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.855301+02:00"
closed-at: "2026-11-10T22:35:26.504654+02:00"
close-reason: "obsolete: tools has no whitebox or release engine split left; on the audit's arm64 engine (some commits behind master) 'hb --load tools/check.f -- --verify-only test/engine-suite.f' exits 0 with 9 W-CHECK-DEFERRED warnings and no refusal"
---

Problem: tools/check.f --verify-only test/engine-suite.f refuses on the release engine and passes on the whitebox engine. Acceptance: find what the release engine lacks (sealed or stripped words the verifier reads) and fix that layer. Claim: unassigned.
