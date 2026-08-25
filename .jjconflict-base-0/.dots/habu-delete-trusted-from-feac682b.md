---
title: "Delete TRUSTED: from the reader"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:44.332840+02:00"
---

Phase 8, the endpoint of 4fd12d60: remove the TRUSTED: keyword from the language. After phases 1-7 land (blocked by fab55650, 1f5980b8, b2cd1a61, bc70057e, the phase-5 dot, the phase-6 dot, the phase-7 dot) and the cut's phase E deletes the 12 dies-at-E sites (679cfd35), the reader refuses TRUSTED: as an unknown word and the checker refuses any unchecked definition BY CONSTRUCTION - the type system is the enforcement; no ledger, no lint, no inventory. Acceptance: rg 'TRUSTED:' over .f/.fs finds zero definition sites; the engine builds and all gates green through the fixpoint.
