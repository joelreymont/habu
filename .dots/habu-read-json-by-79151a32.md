---
title: "Move JSON readers onto JSON:value"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T18:02:40.228483+01:00"
---

Problem: Joel 2026-11-19: "json users should not have to deal with pointers, cursors". The JSON:value words read JSON (src/lib/json/value.f), but libraries and programs still read through the caller-owned JSON:cursor (READ-INIT and its kin, src/lib/json/read.f) and POINTER-SPAN, and JSON:VALIDATE does not exist. Acceptance: Habu libraries read tokens only in a reader scope JSON opens on a value (WITH-READER), never caller storage; the library words that take or answer JSON text or a cursor (ODATA:PAGE, RECORD$, COPY-ENTITY, JSON-RPC:METHOD? and >MESSAGE, JSON-SCHEMA:VALID?, JSONL:READER, MCP:ADD-TOOL's body) take or answer JSON:value; JSON:VALIDATE checks a value against a compiled schema and rejects at the first violation with the document's line and column; READ-INIT, READ-CLOSE, READ-FINISH, READ-DISPOSE, the public JSON:cursor, POINTER-SPAN, READ-MEMBER$ and READ-REPEAT are deleted once their callers move (READ-REPEAT's are Zen's zen/src/server/mcp.f:653, :689 and zen/src/get.f:643, which PARSE covers), and so is every token word left with no caller. Depends: none. Claim: unassigned.
