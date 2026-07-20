---
title: Define rigid host region generations
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T16:43:48.445391+02:00\""
---

Problem: ptr T and existing type variables cannot name an allocation generation rigidly, so equal-sized containers and recreated owners may unify and stale indices may regain authority. Fix: add checker/type-family domains for fresh rigid host region, extent, and mutation-generation identities with monotonic nonreuse and exhaustion-before-wrap; keep runtime counters private and never treat numeric handles as type authority. Acceptance: equal-sized cross-region unification, recreated-owner reuse, extent mismatch, generation mismatch, and wrap/reuse fixtures reject; two accesses within the same region/generation certify; snapshot/native/bootstrap paths preserve the domains. Files: src/core/checker.f, src/core/type-family.f, focused checker/type-family fixtures, docs/effects.md. Verify: red-first candidate matrix with resolving positives, type-family suites, native fixpoint, bootstrap parity, typed-local diff lint, full native gate.

Claim: agent=rigid workspace=.jj-ws/fable-rigid machine=spark (owns checker/type-family rigid-generation domains + fixtures; NOTE type-family.f also mid-edit by declevents lane - orchestrator hand-merges at landing)

Design ratified 2026-07-20 (orchestrator, after the lane's probe-validated design report): domains live in checker.f ONLY (type-family.f zero footprint - load order puts checker.f first and region/gen are minted-fresh-per-allocation atoms, not declared TFAM nominals; the existing fresh-extent-*/fresh-mask-* template-atom substrate at FRESH-ATOM-TOK? checker.f:2551 / E-I-AK :4329 / ATOM-OK? :1067 is the proven carrier - one-call-shared-id certifies, two-calls-distinct-ids reject, validated empirically with no source change). Decisions: (1) per-domain counters RGN-N/EXT-N/GEN-N with exhaustion-before-wrap (named E-RIGID-EXHAUST) AND domain-aware ATOM-OK? (kind>0 RES-TRUE -> CORE-STR=) so numeric handles are never authority - structurally enforced, with a cross-domain equal-id must-not-unify fixture; (2) render.f AUTHORIZED beyond the declared file list for named domain-mismatch diagnostics via the MDIAG machinery, making the reject fixtures genuinely red-first (specific diagnostic asserted, unknown-token base cannot pass); (3) the legacy shared RIGID-FRESH exhaustion retrofit is EXCLUDED - own dot habu-exhaustion-guard-the-2c85fee5.
