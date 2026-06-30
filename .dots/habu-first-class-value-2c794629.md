---
title: First-class value records
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T15:49:04.746881+02:00"
---

Problem: current structures are sufficient for pointer/arena-backed PTX IR and checker records, but not a great general value-record system. Missing: true by-value stack records with copy/destructure semantics; nominal struct types in stack effects instead of only pointer-field access; typed constructors/setters that prove full initialization before use; generic structure fields beyond current byte/cell/pointer field shapes; ownership/lifetime model for arena-backed records. Fix: design and implement value records as first-class typed Habu values, with structure declarations publishing nominal effect types, checked constructors/destructors/accessors/updaters, initialization-state checking, and arena-owner/lifetime tokens where records are backed by memory. Acceptance: positive fixtures for stack copy/destructure, constructor initialization, field access/update, generic fields, and arena-backed record ownership; negative fixtures for uninitialized field use, wrong nominal record, lifetime/owner mismatch, and partial construction escape; migrate at least one PTX IR/checker record path where it reduces trust or offset plumbing; docs/forth.md and TRUSTED.md updated; focused structure/checker/PTX IR gates plus full native suite pass. Files: src/core/structures.f, src/core/structures-effects.f, src/core/checker.f, docs/forth.md, TRUSTED.md, lib/ptx/ir.f, tests.
