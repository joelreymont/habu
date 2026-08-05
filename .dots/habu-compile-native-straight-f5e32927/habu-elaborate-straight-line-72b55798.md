---
title: Elaborate straight-line HIR
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:57:03.015570+02:00\""
blocks:
  - habu-define-straight-line-87e7f8a5
---

Full context: Wave 2 requires real checked colon-body source to elaborate through registered HIR builder operations without direct machine emission. Implement literals, selected arithmetic primitives, stack renames, and return against the frozen schema. Acceptance: SQUARE and arithmetic/stack examples freeze valid HIR; unmodeled immediate/primitive and checker-binding mutations reject. Dependency: straight-line HIR schema.

Claim: agent=elaborate workspace=.jj-ws/habu-elaborate-straight-line-72b55798
