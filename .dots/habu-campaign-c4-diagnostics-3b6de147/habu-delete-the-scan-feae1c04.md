---
title: "Delete the scan's other unread stop stores"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T21:28:04.285479+01:00"
closed-at: "2026-11-16T04:07:36.309254+01:00"
---

Problem: src/habu/verify-source.f SIGNATURE-STOP, SEAL-STOP and GROUP-NEXT store TOKEN-BYTE right before REJECTION:STOP, as the stores deleted from UNCLOSED-STOP did; TOKEN-BYTE's comment names a stopped-token reader that may be gone, and CLAIMED-LOAD claims nothing now. Acceptance: every store no reader reaches is deleted, the comment says what reads TOKEN-BYTE, CLAIMED-LOAD's name says what it does; every record stays as it is. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (integration).
