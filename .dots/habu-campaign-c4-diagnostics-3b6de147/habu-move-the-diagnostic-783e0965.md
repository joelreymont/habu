---
title: Move the diagnostic tools onto the one form
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.376241+01:00"
---

Problem: tools/check-core.f ends five failures with a bare `check.f:` prose line, no record, no JSON under --json-errors and no help: CHK-PATH-UNSAFE, CHK-RUN-TOO-BIG, CHK-RUN-LATE, CHK-STATUS-LN, CHK-TRUNCATED-LN. Brief: /tmp/jerry-w227-brief.md. Acceptance: each prints one REJECTION record with a help, prose or JSON as the mode says; test/check-signal-test.f pins the record. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w227.
