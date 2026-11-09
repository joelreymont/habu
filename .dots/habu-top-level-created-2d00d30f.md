---
title: "Top-level created word takes its definer's created effect"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T11:58:19.980855+02:00"
---

Problem: a word a does> definer creates at top level takes the definer's created effect as its own facts (src/core/checker.f STORE's WRAP-TAKE at the top-level run), and a straight-line reader of it inherits it, cold and warm alike. Reduction: d.f ': MINI-PV ( -- ) create here scratch-cell-mark 0 , does> ( -- ptr ptr a ) ;' then b2.f 'MINI-PV MINI-P  : MINI-P@ ( -- ptr u8 ) MINI-P @ ;' — the facts texts of MINI-P and MINI-P@ carry ' creates-effect ( -- ptr ptr a ) raw a'; MINI-P creates nothing and MINI-P@ is no definer. Same in the tree: xref.f's XREF-A/-B/-SN/-FN (PTR-VARIABLE) and their readers. Acceptance: a created word's facts carry its own effect only; a reader carries none; a rejected-program or facts-text case through the real load path. Files: src/core/checker.f (STORE, WRAP-TAKE), src/core/check-hook.f. Verify: the reduction's facts texts (object FACTS rows) without creates-effect. Depends: none. Ownership: src/core/checker.f. Claim: unassigned.
