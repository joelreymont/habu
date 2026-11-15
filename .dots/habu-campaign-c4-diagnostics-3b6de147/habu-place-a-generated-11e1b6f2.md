---
title: "Place a generated name's sealed-package record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:37.129831+01:00"
closed-at: "2026-11-16T04:07:35.090351+01:00"
---

Problem: src/habu/xref.f CHECK writes its own E-SEALED-PACKAGE record (a copy of INTERPRETER:SEALED-REJECT) at a token in the plan buffer, so it is unplaced; --verify-only accepts `ENUM xref rec ;ENUM` (rc 0) that the load rejects. Acceptance: the three readers print one placed record at the declaration, rc 70. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
