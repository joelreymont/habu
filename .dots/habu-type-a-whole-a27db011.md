---
title: Type a whole JSON number as integer
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T14:35:25.654622+01:00"
---

Problem: src/lib/json-schema.f INTEGER-TYPES :833 types only a T-INT lexeme a signed cell holds as integer, so 1.0, 1e2 and 12345678901234567890 fail type integer, while JSON Schema 2020-12 (the CONTRACT's dialect) matches any number with a zero fractional part. Acceptance: those three are integer, 1.5 and 1e-1 are not; json-schema and mcp pass. Depends: the public whole-number word tim asked for (/tmp/tim-json-whole.md), named by Joel. Claim: unassigned.
