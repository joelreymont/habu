---
title: "checker: diagnostic renderer garbles family names for locals-sourced actuals"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T19:35:57.899574+02:00"
---

FOR THE TYPE-SYSTEM LANE. Found by the raw-audit destruction review (2026-07-12), pre-existing: when a rejected word's actual types arrive via {: :} locals or on-stack computation, the E-MISMATCH diagnostic prints a corrupted family name spliced with the current word-name buffer - e.g. 'actual: n n pack-shapeols<>' instead of 'CAD-KIND:cols<>'; reproduces on untouched SHAPE-ELEMS ('ape-elemscols<>'). Soundness unaffected (rc 70 still), but the message is misleading at exactly the real call sites nominal kinds protect. Owner: src/core/checker.f diagnostic renderer - the family-name render appears to reuse/overlap the word-name string buffer. Repro: swapped-role call through typed locals in any package-MAKI word; negatives that declare swapped types in the signature render clean (that is why suite fixtures dodge it). Fix + regression pinning the exact rendered string for a locals-sourced mismatch.
