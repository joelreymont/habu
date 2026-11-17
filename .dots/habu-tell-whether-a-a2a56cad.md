---
title: Tell whether a request path climbs
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:05:55.547981+01:00"
---

Problem: mark (SAP OData client) needs one Habu word answering whether a request path (before ? or #) holds a dot segment in any reading a client or server may make: raw or percent-decoded once, / or \ as separators, a segment up to its first ;. SAP's hand-made check passes ..%2f and ..#x (spark:/tmp/mark-habu-path-basic.md, examples there). Acceptance: the word answers every example in that file. Depends: Joel's name for the word. Claim: unassigned.
