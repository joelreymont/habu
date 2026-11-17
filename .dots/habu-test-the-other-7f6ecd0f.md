---
title: Test two window rejections, drop two guards
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T15:54:31.900600+01:00"
---

Problem: no test holds the E-VERIFY-WINDOW records of src/core/checker.f NESTED-REJECT and src/habu/verify-source.f COMPOSE-NESTED, which programs reach; src/core/checker.f WINDOW-REJECT and the window check in CHECKER-REPLAY-NAME-OK? guard words only the verifier's scan calls, inside its window, so they are runtime checks no program reaches. Acceptance: one rejected program each for NESTED-REJECT and COMPOSE-NESTED in test/load-reject-diag-test.f, asserting the record's place; WINDOW-REJECT and the registrar window check deleted. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-vw2.
