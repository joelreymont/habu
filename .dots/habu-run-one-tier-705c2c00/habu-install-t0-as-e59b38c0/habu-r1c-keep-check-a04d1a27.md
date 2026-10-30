---
title: "r1c: keep check-test's nominal redrive and scan cases on both engines"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:09:27.445294+02:00"
---

Problem: tools/check-test-lib.f NOM-REDRIVE-TEST and NOM-SCAN-TEST relied on DEFTYPE being unbaked; R1 bakes every nominal declarer on arm64 (asserts 422, 423, 667, 669). Acceptance: each case tests its comment's claim on both engines; check-test rc 0 on R1's candidate. Files: tools/check-test-lib.f. Brief /tmp/jerry-big/t0-r1c-nominal-cases-brief.md. Base qqqmkrqy (R1b). Depends: R1b. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-r1c
