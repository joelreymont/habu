---
title: "V2 types: finite CAD capability effects design"
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T10:25:02.714102+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md R8 requires rewrite/fusion/recompute/cache legality to distinguish pure, parameter-read, state-write, random, IO, device, atomic, collective, allocation, and publication effects; ordinary stack effects alone do not state these semantic reorder constraints. Fix: specify a finite sealed static op-schema row, checker propagation, mandatory Maki registration, explicit capability tokens, planner legality, and a separate runtime resolver whose canonical semantic binding digest enters every cache/promotion key; do not design a general ambient effect calculus or embed runtime values in schemas. Acceptance: multiple parameter/state bindings compose canonically; random/stateful duplication and atomic reorder reject; pure analysis needs no IO/device token; publication requires one-shot authority; every runtime parameter/capability-controlled input changes or disables cache identity. Implementation leaves include habu-define-finite-cad-0bdf52ad, habu-seal-cad-effect-49cac404, checker/registry/capability/planner dots, habu-resolve-runtime-cad-2864336f, habu-census-cad-effect-3240237b, and habu-key-caches-by-fddcea19. Files: MODEL-CAD-V2-PLAN.md, docs/effects.md, tracker decomposition only. Verify: effect census, adversarial static/runtime mutation matrix, dot dependency lint.
