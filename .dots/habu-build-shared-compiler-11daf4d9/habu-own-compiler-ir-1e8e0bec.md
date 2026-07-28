---
title: Own compiler IR arena
status: active
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:38.117645+02:00"
blocks:
  - habu-own-compiler-ir-9d25246b
---

Full context: design sections 6.2-6.3 require disposable append-only typed storage with geometric growth and committed ceilings. Build the one IR-RAW-authorized arena over VEC/MEM allocation and NOM seal/truncate invariants without copying NOM or exposing pointers/casts. Acceptance: mark/rollback, growth, overflow, cross-owner, abort, freeze, stale, and whole-range release fixtures pass; frozen readers accept nominal IDs only. Dependency: compiler context.

Claim: agent=ir-arena workspace=.jj-ws/habu-own-compiler-ir-1e8e0bec
