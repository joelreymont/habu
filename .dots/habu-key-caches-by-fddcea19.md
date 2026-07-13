---
title: Key caches by resolved CAD effects
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:26:04.366620+02:00"
blocks:
  - habu-census-cad-effect-3240237b
---

Full context: a sealed static effect row proves legality but cannot by itself distinguish executions using different parameters, mutable generations, RNG positions, device facts, or publication authority. Coordinate the exact disjoint cache/promotion migration leaves produced by habu-census-cad-effect-3240237b; no consumer source is edited in this integration dot and no cache may re-resolve bindings ad hoc. Acceptance: every census leaf is an explicit blocker; changing weight, bias, state generation, RNG sequence, target/device authority, or publication scope changes the exact affected key; irrelevant address/order changes do not; state/random/IO/device paths never reuse a pure key; replay and persistence round-trip the binding digest; old rows without it reject or migrate explicitly; fresh census is empty and full Maki/native gates are green. Files: MODEL-CAD-V2-PLAN.md, STATUS.md, final manifest/public-signature wiring only. Verify: key mutation matrix from every leaf, replay/fresh-process tests, Maki/full native gates. Depends on the cache-identity census and every leaf it adds here.
