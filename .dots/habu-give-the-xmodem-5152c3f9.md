---
title: Give the xmodem and event-stream headers a record
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T12:11:01.341717+01:00"
---

Problem: src/lib/serial-xmodem.f:41-62 and src/lib/net/event-stream.f:63-75 keep a caller-carved session header as ^a or ^n, reached through byte-offset views and CAST: >VECTORS, and their callers size it (SERIAL-XMODEM:SESSION-BYTES, SSE:STORAGE-BYTES, src/lib/net/mcp.f:1692-1751). Acceptance: each header is a public OPAQUE STRUCTURE the caller declares as typed storage, as JSON:cursor is; no cast, offset or view reaches it; the byte-size words are gone. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-xmodem (xmodem); SSE unassigned.
