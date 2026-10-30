---
title: Refuse a line break inside a CURL header line
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T23:48:48.991783+02:00"
---

Lane CURL-CRLF (brief brief-curl-crlf.md): CURL:HEADER+ refuses CR or LF with CURL:E-OPERAND (MCP-L3 review proved TOKEN! with CR/LF injects a header). Ruling: Joel 'what's best for long term' -> refuse in CURL for every caller. Done pqnyvvko 0b224d46 on master in .jj-ws.noindex/carl-curl, ACCEPTED (curl-test red 2/green, http-test ok). Lands in the MCP H + Zen H integration stack before L3. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-curl.
