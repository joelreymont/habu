---
title: Delete the duplicate-status leftovers
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:55:39.486987+01:00"
closed-at: "2026-11-16T04:07:36.336681+01:00"
---

Problem: with duplicate records written where they happen (status 70), DUP-RC, CA-DUP-RECORD$, DUP-RECORD$ (tools/check-all-errors-core.f), DUP-RECORD$ and LOADER-RECORD$ (src/lib/verify-diagnostics.f), VERIFY:FAULT-LEN@, SOURCE-COMPOSE-STOPPED-SOURCE$ and check-verify-core.f's VFY-OPEN-* cells have no reader, and tests still pin status 78 (E-DUP-DEFINITION). Acceptance: each is deleted or named with its caller; no test expects 78. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
