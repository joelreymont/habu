---
title: Let a SUMTYPE or PRODUCT body be any length
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T20:04:02.714819+01:00"
---

Problem: src/core/sumtype.f buffers a declaration's body in TDECL-BUF, a fixed TDECL-CAP ($1000, :32) bytes; a longer body is rejected E-TDECL-CAP 'declaration too long' (TDECL-C, TDECL-OVERSIZE, :223), and TDECL-SEEN-CAP (:80) is sized from it, a limit Habu set itself that a sum of about a hundred payload variants reaches. Acceptance: a SUMTYPE and a PRODUCT whose body is far past 4096 bytes declare and work, as a type-decl test; TDECL-CAP's body use, TDECL-OVERSIZE and the 'declaration too long' rejection are gone (E-TDECL-CAP stays for a name past TF-NAME-MAX). Depends: none. Claim: unassigned.
