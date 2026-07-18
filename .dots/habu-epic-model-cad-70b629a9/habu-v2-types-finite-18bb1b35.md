---
title: "V2 types: finite CAD capability effects design"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-11T10:25:02.714102+02:00\""
---

Problem: MODEL-CAD-V2-PLAN.md R8 requires rewrite/fusion/recompute/cache legality to distinguish pure, parameter-read, state-write, random, IO, device, atomic, collective, allocation, and publication effects; ordinary stack effects alone do not state these semantic reorder constraints. Fix: specify a finite sealed static op-schema row, checker propagation, mandatory Maki registration, explicit capability tokens, planner legality, and a separate runtime resolver; cache owners consume sealed completeness-proven domain projections from the full binding set, not ad hoc filters or an indiscriminate global digest. Do not design a general ambient effect calculus or embed runtime values in schemas. Acceptance: multiple parameter/state bindings compose canonically with stable site paths; random/stateful duplication and atomic reorder reject; pure analysis needs no IO/device token; publication requires one-shot authority; every relevant runtime parameter/capability-controlled input changes or disables cache identity and every omission has a tested irrelevance proof. Implementation leaves include habu-define-finite-cad-0bdf52ad, habu-seal-cad-effect-49cac404, checker/registry/capability/planner dots, habu-resolve-runtime-cad-2864336f, habu-census-cad-effect-3240237b, habu-define-complete-cad-90a9945c, and habu-key-caches-by-fddcea19. Files: MODEL-CAD-V2-PLAN.md, docs/effects.md, tracker decomposition only. Verify: effect census, adversarial static/runtime/projection mutation matrix, dot dependency lint.

Claim: agent=tfinite workspace=.jj-ws/fable-tfinite (design phase only: docs/effects.md + plan alignment + tracker decomposition; no engine/lib code)
