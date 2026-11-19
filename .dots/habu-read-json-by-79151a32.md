---
title: "Read JSON by value: the JSON:value words"
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T18:02:40.228483+01:00"
---

Problem: Joel 2026-11-19: "json users should not have to deal with pointers, cursors"; today a program reads JSON through a caller-owned JSON:cursor (READ-INIT and its kin, src/lib/json/read.f:554). Acceptance: a program reads JSON only through JSON:value and Joel's words JSON:PARSE, TEXT$, GET, FIND, AT, EACH, EACH-MEMBER, STRING=, WITH-STRING, >N, >BOOL, the kind tests and REJECT (design /tmp/carl-json-design.md until it lands); PARSE rejects malformed text and a repeated member name with the place; each word's misuse is a rejection with the document's line and column. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-jsonval.
