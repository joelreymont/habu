---
title: "V2 types: finite CAD capability effects design"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-11T10:25:02.714102+02:00\""
---

Problem: MODEL-CAD-V2-PLAN.md R8 requires rewrite/fusion/recompute/cache legality to distinguish pure, parameter-read, state-write, random, IO, device, atomic, collective, allocation, and publication effects; ordinary stack effects alone do not state these semantic reorder constraints. Fix: specify a finite sealed static op-schema row, checker propagation, mandatory Maki registration, explicit capability tokens, planner legality, and a separate runtime resolver; cache owners consume sealed completeness-proven domain projections from the full binding set, not ad hoc filters or an indiscriminate global digest. Do not design a general ambient effect calculus or embed runtime values in schemas. Acceptance: multiple parameter/state bindings compose canonically with stable site paths; random/stateful duplication and atomic reorder reject; pure analysis needs no IO/device token; publication requires one-shot authority; every relevant runtime parameter/capability-controlled input changes or disables cache identity and every omission has a tested irrelevance proof. Implementation leaves include habu-define-finite-cad-0bdf52ad, habu-seal-cad-effect-49cac404, checker/registry/capability/planner dots, habu-resolve-runtime-cad-2864336f, habu-census-cad-effect-3240237b, habu-define-complete-cad-90a9945c, and habu-key-caches-by-fddcea19. Files: MODEL-CAD-V2-PLAN.md, docs/effects.md, tracker decomposition only. Verify: effect census, adversarial static/runtime/projection mutation matrix, dot dependency lint.

Claim (RELEASED 2026-07-18, design merged): agent=tfinite workspace=.jj-ws/fable-tfinite (design phase)

Design-phase record (2026-07-18, tfinite):

Spec written into `docs/effects.md` (new "## R8 capability-effect design" part after the existing R8-0 vocabulary/algebra section), numbered steps:
- R8-1 finite sealed static op-schema effect row: ten-class sealed vocabulary, binding-record field encoding (atom tag, slot-kind tag, varint slot index, length-prefixed varint site path), content-addressed `KEY`, the `REMAP`-then-`UNION` canonical composition rule, and an explicit non-goals subsection (not a general ambient effect calculus; no runtime values in schemas).
- R8-2 checker propagation over the existing machinery: opaque nominal handle in usig metadata, `REMAP`/`UNION` at call and quotation boundaries, the `NP-CHECK`/`NP-MINT-CHECK` declared-row seal, the `TVK-RAW` raw-storage seal, the `CPPSLOT` order-sensitive-typestate precedent, and the M5/M5b `CTL-BARRIER` tie for `collective`.
- R8-3 mandatory Maki registration, capability tokens (`CAPTOK` precedent), and the per-atom planner-legality table of forbidden rewrite/fusion/recompute/cache moves.
- R8-4 the separate runtime resolver, the cache-identity rule (sealed versioned projections, completeness evidence, tested irrelevance proofs, policy version in key), and a 37-row adversarial static/runtime/projection mutation matrix.
- R8-5 leaf/spec-ownership table and dependency order; the doc ends with the explicit R8 acceptance-mapping table.
`MODEL-CAD-V2-PLAN.md` § R8 gained a one-line cross-reference to `docs/effects.md`; `FILEMAP.md` now lists `docs/effects.md`.

Leaf decomposition (design-reference note appended to each leaf dot): define-finite (R8-0, closed), seal (R8-1 authority), persist-cad-semantic (R8-2 checker), require-maki-op (R8-1/R8-3 registry), add-explicit-cad (R8-3 capability), enforce-effect-aware (R8-3 planner), resolve-runtime (R8-4 resolver), census (R8-4 census), define-complete (R8-4 projection), key-caches (R8-4 integration). The `blocks:` dependency edges were audited against the target order define-finite -> seal -> {persist, require-maki-op} -> add-explicit-cad -> resolve-runtime -> {census, enforce-effect-aware} -> census -> define-complete -> key-caches; they already form the correct DAG, so no edge repair was needed (enforce-effect-aware correctly branches off resolve-runtime because it consumes resolved bindings).

Gates on the design tree (native `bin/hb`): dot-dep-lint 0 finding(s); stale-status-lint 0 finding(s); host-lint 0 finding(s); filemap-lint 0 finding(s). No `.f`/`.fs` source touched, so no engine battery applies.

First implementation leaf to start: `habu-seal-cad-effect-49cac404` (R8-1 authority boundary). Its prerequisites define-finite (row vocabulary) and add-immutable-nominal (arena) are closed/landed; it waits on `habu-checker-sealed-destructure-d967fc03` for owner-only construction and package closure. Then it builds `src/cad/effect-authority.f`, admits only arena-validated handles, exposes no raw handle mint or cast, and closes `CAD-EFFECT` exactly once, per `docs/effects.md` § R8-1.

DESIGN PHASE COMPLETE 2026-07-18 (tfinite lane, merged 9e324100): the dot
stays OPEN as the umbrella - closure follows its implementation children
(dot off correctly refused while leaves are open). Design deliverable and
leaf decomposition recorded above; next leaf habu-seal-cad-effect-49cac404
waits on habu-checker-sealed-destructure-d967fc03.
