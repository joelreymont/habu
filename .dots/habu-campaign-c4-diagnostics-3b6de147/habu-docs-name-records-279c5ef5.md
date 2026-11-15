---
title: Docs name records, not one-line forms
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:23:42.299179+01:00"
closed-at: "2026-11-16T04:07:36.269925+01:00"
---

Problem: docs/forth.md (:194, :207, :261, :825, :840, :1033, :1145, :2051, :2064, :2257, :2376, :2408, :2414, :2420), docs/forth-card.md (:58, :65, :66, :209, :211, :216, :534) and docs/debugging.md (:14-45, :231, :237) quote the old 'E-X habu: ...' and 'habu: in W: ...' lines, the deleted E-DUP-DEFINITION, and statuses 74, 75 and 84 for rejections. Acceptance: each passage names the record's code and meaning and status 70, or is cut where it only tracks output. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
