---
title: Give the remaining bare throws records
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.760660+01:00"
closed-at: "2026-11-16T04:07:36.020267+01:00"
---

Problem: public words outside a declaration still end in the generic E-THROW record (rc 67): DECL-EVENT:OPEN, DECL, ARITY, POLICY, DERIVE, OPAQUE, FIELD, PAYLOAD-*, PREPARE, FINALIZE, CURRENT (E-DEV-*), STRUCTURE-MAKE:GENERATE and ARM (E-SM-*), GENERATED-DECL-CTOR:ARM (E-CTOR-ARM), SOURCE-ROOT:CANON-OS, JOIN and CANONICAL (E-PATH-*), E-PF-SCHEMA and E-MAP; under check.f, E-OVERLAY-CAP at checker.f:12995 (a package named when the dictionary is full). Inside a declaration the same codes print E-BAD-DECLARATION through generated-declaration.f RENDER/GUARD-CODE and sumtype.f TDECL-REPORT. Acceptance: each writer prints its own placed record and stops 70, so the declaration catchers print nothing by code; a code callers recover from (E-PATH-RANGE, E-PATH-NUL, E-MAP) stays catchable and carries its placed record (REJECTION:SAVE, then PASS), printed with rc 67 when nothing catches it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
