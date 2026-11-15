---
title: "Reject a comment as a type's name"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:15.593204+01:00"
closed-at: "2026-11-16T04:07:34.726611+01:00"
---

Problem: DEFLINEAR, DEFTYPE, NEWTYPE, SUMTYPE, PRODUCT and VALUE-RECORD take a ( or \ token as a type's name in the loader and the verifier; a field word's record names it upper-case. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same record, rc 70. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
