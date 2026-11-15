---
title: Print an uncaught throw in check.f as a record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.676704+01:00"
closed-at: "2026-11-16T04:07:34.911950+01:00"
---

Problem: tools/check-core.f CHK-MAIN-UNCAUGHT (:1259-1267) prints `check.f: uncaught throw code N`, also under --json-errors. Acceptance: one error[E-THROW] record in the form asked for. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
