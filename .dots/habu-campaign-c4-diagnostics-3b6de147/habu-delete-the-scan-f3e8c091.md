---
title: "Delete the scan's unread stop stores"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:50:18.915378+01:00"
closed-at: "2026-11-16T04:07:36.324508+01:00"
---

Problem: src/habu/verify-source.f STOP-AT and STATEMENT-STOP (:421-429) have no caller; UNCLOSED-STOP, UNCLOSED-DECL-STOP and BARE-TRUST-STOP store TOKEN-BYTE right before REJECTION:STOP, and CLAIMED-LOAD (:3304) stores TOKEN-BYTE and FAULT-LEN, which only its own guard reads. Acceptance: every store no reader reaches is deleted, with the words no caller reaches; every record stays as it is. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
