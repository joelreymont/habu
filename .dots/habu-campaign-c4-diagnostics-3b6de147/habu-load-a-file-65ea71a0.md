---
title: Load a file from a body alike in every reader
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:52:14.019116+01:00"
closed-at: "2026-11-16T14:39:36.997237+01:00"
---

Problem: include or require inside a definition body is rejected by the runner (E-UNMODELED-IMMEDIATE) while check.f follows the path, and --verify-only prints two records for a body include of a path that resolves too long (/tmp/jerry-emptypath/f body-incres). Acceptance: the three readers give one record and the same status for each body-load fixture. Depends: habu-reject-a-path-672d66ba. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w223.
