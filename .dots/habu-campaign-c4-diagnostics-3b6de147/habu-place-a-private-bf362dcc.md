---
title: Place a private generated-name clash
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:00:46.651430+01:00"
closed-at: "2026-11-16T04:07:35.058830+01:00"
---

Problem: check.f (plain and --verify-only) on /tmp/jerry-c4/pclash/p.f prints 'hb: uncaught throw code 7110' and exits 69: src/habu/verify-source.f RECORD-PRODUCT (:2683) and RECORD-SUMTYPE-BODY (:2613) replay constructors after TDECL-RUN returned, so the replay's throw escapes with no record. Acceptance: both modes print one record, rc 70. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
