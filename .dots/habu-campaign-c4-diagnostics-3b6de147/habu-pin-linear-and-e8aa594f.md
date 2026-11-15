---
title: Pin linear and cast-negative underflows by their record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T11:36:00.937722+01:00"
closed-at: "2026-11-16T04:07:35.162752+01:00"
---

Problem: test/linear-suite.f and test/cast-negative-suite.f pin their top-level underflow cases loosely, so a wrong head or place would pass. Acceptance: each pin names error[E-INPUT-UNDERFLOW] and its place, as the merged engine prints them. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
