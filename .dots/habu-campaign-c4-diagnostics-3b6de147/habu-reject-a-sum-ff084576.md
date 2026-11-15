---
title: "Reject a sum payload's bare ^ as STRUCTURE and ENUM do"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:35:23.695681+01:00"
closed-at: "2026-11-16T04:07:34.485425+01:00"
---

Problem: src/core/sumtype.f TDECL-PAY-ELEM (:537) and its twin (:742) reject a bare `^` as 'bad payload token' (E-TDECL-SYNTAX) where STRUCTURE (structure-decl.f) and ENUM (enum-decl.f EN-PTR-TEXT?) say 'pointer without a pointee' (E-TDECL-PAYLOAD). Acceptance: the three declaration forms reject a bare ^ in one wording; tests pin it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
