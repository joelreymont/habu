---
title: "V2 types: finite CAD capability effects design"
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T10:25:02.714102+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:369-397 requires rewrite/fusion/recompute/cache legality to distinguish pure, parameter-read, state-write, random, IO, device, atomic, collective, allocation, and publication effects; ordinary stack effects alone do not state these semantic reorder constraints. This is a bounded design/probe dot under 30 minutes. Fix: inventory current no-return/control/linear/capability metadata and specify the smallest finite op-schema effect row plus explicit capability tokens; do not design a general ambient effect calculus. Acceptance: design shows random/stateful duplication rejects, atomic reorder rejects, pure analysis needs no IO/device token, and publication requires authority; split implementation dots. Files: MODEL-CAD-V2-PLAN.md:369-397, docs/effects.md, src/core/checker.f, maki/op-registry.f, maki/fusion-plan.f. Verify: effect census and minimal negative checker fixtures.
