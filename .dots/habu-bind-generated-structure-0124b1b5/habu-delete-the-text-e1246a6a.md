---
title: Delete the text-evaluating declaration machinery
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T19:06:02.610149+01:00"
closed-at: "2026-11-14T20:44:51.418879+01:00"
blocks:
  - habu-build-a-constructor-c436b87a
  - habu-make-make-unmake-3a5c9b8a
---

Problem: once constructors and the product and DEFTYPE words are built without text, these have no caller: TDECL-EVAL-XT and TDECL-EVAL-ARMED, the src/core/include.f BOOT and INSTALL parts that arm it, the TDPLAN text rows (TDPLAN-WORD+, -DEF$, -TEXT?, -TEXT-N, -TEXT$, the DEF-OFF and DEF-U fields), the TDGEN text words, TDECL-PROD-WORD, TDECL-CTOR-WORD's unused family parameter, CTOR-PEND (src/core/check-walk.f and its sumtype.f and generated-declaration.f callers) and deftype.f NG-EVAL. TDPLAN-WORD+ also records a definition span that means nothing for a built row, and TDGEN-DRV-NAME's n0 is always 0. Acceptance: rg -i --no-ignore finds none of them in src or test; test/closed-source-suite.f's header says what the suite checks without them. Depends: habu-build-a-constructor-c436b87a, habu-make-make-unmake-3a5c9b8a. Claim: unassigned.
