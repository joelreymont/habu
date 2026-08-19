---
title: "TRUSTED: dies; PRIM axioms remain for the foreign handful"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T09:53:28.150430+02:00"
---

User ruling (2026-08-19): no trusted ledger, no TRUSTED: keyword - the type system describes these things within habu, and PRIM:/PPRIM: axioms remain only for true foreign primitives (FFI, syscalls). Current state: 1,482 TRUSTED: sites, 399 PRIM axioms, TYPED-BUFFER/TYPED-VARIABLE landed with 185 uses (the typed-storage discharge path exists). Plan: (1) one-shot census by ASSERTION CLASS - buffer/field views, decl-machinery metaprogramming (enum-decl 44 + structure-decl 40 + roles 36 + generated-declaration 23), foreign calls (cuda-driver 33, pty 18, task 15), checker-bootstrap, casts - output is dots, never a standing ledger; (2) typed-storage sweep: migrate views onto TYPED-BUFFER/TYPED-VARIABLE, extending layout-buffer where a view shape is missing; (3) the definer-typing capability for the decl machinery; (4) foreign class converts to PRIM: axioms; (5) delete TRUSTED: from the reader - the checker then refuses any unchecked definition BY CONSTRUCTION, which is the enforcement, replacing ledger/lint/inventory forever. Overlap credit: the cut's phase E deletes the migration/aot TRUSTED sites for free. Each phase gets its own dot once the census gives real numbers.
