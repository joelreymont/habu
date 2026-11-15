---
title: Print the word a derived init rejects
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:37.141971+01:00"
closed-at: "2026-11-16T04:07:34.863667+01:00"
---

Problem: `STRUCTURE gt DERIVE addr init FIELD ab n ;STRUCTURE` at global scope prints `= word:` followed by two NUL bytes, from sumtype.f TDINIT-REFUSE. Acceptance: the record names the word, no NUL byte. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
