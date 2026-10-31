---
title: Refuse CR and LF in HTTP response header names and values
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T16:41:40.641348+02:00"
---

HTTP:HEADER! (src/lib/net/http-response.f:236) writes the name and value raw between ': ' and CRLF, so a value holding CR or LF injects header lines or ends the head early. Same rule as CURL:HEADER+ (curl.f ONE-LINE, E-OPERAND on CR/LF): HEADER! throws E-OPERAND for CR or LF in the value and for a name outside RFC 9110 token characters, before writing anything. Callers to check: http-static.f ETag/Cache-Control/Content-Type, bearer.f CHALLENGE, every HEADER! user rg finds. Test in src/lib/net/http-test.f. Found while gating kevin's BEARER:METADATA!.
