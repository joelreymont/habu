---
title: Define rigid host region generations
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T16:43:48.445391+02:00\""
---

Problem: ptr T and existing type variables cannot name an allocation generation rigidly, so equal-sized containers and recreated owners may unify and stale indices may regain authority. Fix: add checker/type-family domains for fresh rigid host region, extent, and mutation-generation identities with monotonic nonreuse and exhaustion-before-wrap; keep runtime counters private and never treat numeric handles as type authority. Acceptance: equal-sized cross-region unification, recreated-owner reuse, extent mismatch, generation mismatch, and wrap/reuse fixtures reject; two accesses within the same region/generation certify; snapshot/native/bootstrap paths preserve the domains. Files: src/core/checker.f, src/core/type-family.f, focused checker/type-family fixtures, docs/effects.md. Verify: red-first candidate matrix with resolving positives, type-family suites, native fixpoint, bootstrap parity, typed-local diff lint, full native gate.

Claim: agent=rigid workspace=.jj-ws/fable-rigid machine=spark (owns checker/type-family rigid-generation domains + fixtures; NOTE type-family.f also mid-edit by declevents lane - orchestrator hand-merges at landing)
