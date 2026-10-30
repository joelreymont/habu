---
title: Refuse JSON text that is not UTF-8
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T23:48:49.002900+02:00"
---

Lane JW-UTF8 (brief brief-jw-utf8.md): JSON-WRITE STRING/KEY refuse text UTF8:VALID? rejects, with E-JW-BYTE, appending nothing (RFC 8259 8.1; H4a review F3: TEST-ISSUER signed claims no verifier could read). Done kunxtmln 363db0b5 on master in .jj-ws.noindex/carl-jw, ACCEPTED. Integration fold-in: TEST-ISSUER ISSUE answers 400 invalid_request on E-JW-BYTE, with a scope=%FF%FE case. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-jw.
