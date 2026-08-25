---
title: Define complete CAD cache projections
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:40:22.798569+02:00"
blocks:
  - habu-census-cad-effect-3240237b
---

Full context: the runtime resolver produces a complete semantic binding set, but schedule, compile, result, evidence, and promotion artifacts do not necessarily depend on identical subsets. Ad hoc cache-side filtering is unsound, while hashing the full set can cause needless recompilation or retuning. After the cache census freezes domains, define a sealed versioned projection policy per cache/artifact class and a checked projector that returns projection digest plus completeness evidence from the full resolved set. Unknown atoms/domains or any unproved omission fall back to the full digest or typed uncacheable; cache owners cannot construct policies or filter entries. Acceptance: mutate every resolved binding for every domain and prove either the projected digest changes or a declared independently tested irrelevance rule applies; policy version changes keys; conflicting facts and missing entries reject; projections are deterministic and address/order independent. Files: new maki/effect-projection.f and focused test, projection policy table from the census, docs/effects.md. Verify: exhaustive mutation matrix, policy completeness tests, Maki focused/full gates. Depends on the cache-identity census; every generated cache migration leaf must depend on this authority.

Design-reference (2026-07-18, tfinite): implements `docs/effects.md` § R8-4 (sealed projection policy and cache-identity rule). Acceptance: `maki/effect-projection.f` defines a sealed versioned projection policy per cache/artifact class and a checked projector returning a projection digest plus completeness evidence from the full resolved set; mutating any relevant resolved binding changes the projected digest unless a declared, independently tested irrelevance rule applies, an unknown atom/domain or unproved omission falls back to the full digest or a typed uncacheable result, the policy version participates in the key, cache owners cannot construct policies or filter bindings, and projections are deterministic and address/order independent — mutation-matrix rows 33, 34, 35, 36.
