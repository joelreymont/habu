---
title: Own compiler IR arena
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:54:38.117645+02:00\""
closed-at: "2026-08-15T14:07:28.859219+02:00"
close-reason: "Closed (vintage audit 2026-08-15, re-executed after the pool incident): arena (mark/rollback deleted CG-12). Production-consumed by the native chain; suites dual-registered, green through the real entry."
---

Full context: design sections 6.2-6.3 require disposable append-only typed storage with geometric growth and committed ceilings. Build the one IR-RAW-authorized arena over VEC/MEM allocation and NOM seal/truncate invariants without copying NOM or exposing pointers/casts. Acceptance: mark/rollback, growth, overflow, cross-owner, abort, freeze, stale, and whole-range release fixtures pass; frozen readers accept nominal IDs only. Dependency: compiler context.

Claim: agent=ir-arena workspace=.jj-ws/habu-own-compiler-ir-1e8e0bec
