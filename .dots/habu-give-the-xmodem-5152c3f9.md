---
title: Give the xmodem and event-stream headers a record
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:11:01.341717+01:00"
---

Problem: src/lib/serial-xmodem.f:41 and src/lib/net/event-stream.f:65 keep a caller-carved session header as ^n, reached through byte-offset views (serial-xmodem.f:46-51 HANDLE-FIELD and the rest, event-stream.f ST@ and the rest) and CAST: >VECTORS, as EDIT did before src/lib/byte-edit.f's header STRUCTURE. Acceptance: each header is a STRUCTURE its package allocates and frees; no cast, offset or view reaches it, and no caller sizes it. Depends: none. Claim: unassigned.
