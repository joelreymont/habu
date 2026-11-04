---
title: Let an HTTP handler learn its request was cancelled or its client left
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T19:43:36.628051+02:00"
---

From tim (Maki), 2026-11-04. An HTTP/MCP handler never learns its client has gone, so a long call holds one of the HTTP workers until it ends on its own. Evidence at master 3378e2d6: src/lib/net/http-arena.f:38 MAX-WORKERS 8 (a protective server limit: stays); src/lib/net/mcp.f:722-724 NOTIFIED answers every 2025-11-25 notification 202 and drops it, notifications/cancelled included (the MCP spec says a server SHOULD stop the cancelled request); 2026-07-28 takes only requests, so closing the connection is a client's only cancel signal there, and nothing watches for it. src/lib/pg.f:1017-1036 READABLE-WITHIN/NOTIFY-WAIT watch only the PG socket. Maki case: its wait tool has no total bound (Maki DECISIONS 'Limits' 2026-11-04); a host times each call out (Codex tool_timeout_sec) and calls wait again, so each host timeout leaves a wait parked in AWAIT -> PG:NOTIFIED? until the job ends; after 8 the service answers nothing. Not reproduced end to end. Needs a design (a handler-side cancel signal the handler can wait on beside its own socket, or a cancel that unwinds the handler); new public words go to Joel.
