---
title: Report string escapes as the load does
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:23:42.282174+01:00"
closed-at: "2026-11-16T04:07:34.227268+01:00"
---

Problem: a bad escape in a body literal (s\" inc-t\yarget.f" included) is rejected by the load at its opener (E-UNTERMINATED-STRING), missed by the plain check and --all-errors and given a generic E-REJECTED by --verify-only: src/habu/verify-source.f STRING-REST (:781) checks no escape and BODY-LIT! (:1066) only marks the literal LIT-BAD. Acceptance: every reader rejects it with the load's record at the opener; LIT-BAD is gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
