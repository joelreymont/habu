---
title: "Fix the create axiom's declared effect"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T04:07:09.182387+02:00"
---

PRIORITY 1 - CHECKER AXIOM DEFECT, found by the reader re-entry design probe (a65e56e5). src/core/checker.f:5807 declares PRIM: create PE-PTR-A PE-OUT - create as ( -- ptr a ). Three independent measurements say ( -- ): runtime depth probe prints 0 0 around create; the checker's own body walk certifies : X ( -- ) create ; and refuses ( -- ptr a ) AT the create token; the tree's certified sources (ENUM+, BEGIN-STRUCTURE, +FIELD) do not balance otherwise. Consequence: NDICT:SPELL-ARITY answers 0-in/1-out for create, so every definer half refuses E-NELAB-ARITY - which the census classifies as instrument SELF-CHECK, so lifting the recorder refusal without this fix would make the real gap invisible. Same family as a certified-miscompile: the axiom mis-states a fact any body calling create reads. Fix the axiom row, add the regression (a body calling create certifies with the corrected effect; the old declaration refused), re-derive the ratchet if the row count moves. Files: src/core/checker.f. Depends: none; the a65e56e5 conversion does not block on it but reads SPELL-ARITY honestly once fixed.
