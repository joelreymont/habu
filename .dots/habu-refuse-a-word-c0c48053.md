---
title: Generate SUMTYPE and PRODUCT words in the declaration participant
status: active
priority: 2
issue-type: task
created-at: "2026-10-03T21:18:22.299969+03:00"
---

Problem: ENUM and STRUCTURE generate their words in the declaration transaction's participant (src/core/generated-declaration.f PART-COMMIT), on the load and on tools/check.f's replay alike; SUMTYPE and PRODUCT generate in src/core/sumtype.f, and check.f's replay runs that generation after the family is committed, through GENERATED-DECL-CTOR:REPLAY-LEGACY (src/habu/verify-source.f:2438, :2503), outside TDECL-RUN and the transaction. So under check.f a generated name's clash (`: CKDPROD:MAKE ( n -- n ) 1 + ;` then `PRODUCT ckdprod FIELD x n ;PRODUCT`; a private part's `: HUE-RED ( -- n ) 1 ;` then `SUMTYPE hue VARIANT red ;VARIANT VARIANT blue ;VARIANT ;SUMTYPE`) reports only `E-STATEMENT-THROW … throw 7110`, a DERIVE init rejection only `throw 7119`, and the rejected family stays registered. Acceptance: one generation path for every declaration on the load and the replay; REPLAY-LEGACY and its callers gone; check.f gives the load's rejection for each reproducer and leaves no family behind. Depends: none. Claim: agent=Plan (design).
