---
title: Restore the rejection label after a check
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:23:42.265437+01:00"
closed-at: "2026-11-16T04:07:34.150540+01:00"
---

Problem: tools/check-all-errors-core.f CA-ARM (:154-157) and CA-RUN-CAPTURED (:302-307) set REJECTION:FILE! and ORIGIN! and never put back what they found, so an in-process CHECK-ALL-ERRORS:BUF (test/gate-dictionary-lib.f:541) leaves later records naming its label. Acceptance: after the core returns, records name the file they named before it; gate-dictionary-lib.f:823 passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
