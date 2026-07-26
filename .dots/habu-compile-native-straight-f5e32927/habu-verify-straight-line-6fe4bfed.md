---
title: Verify straight-line SIR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:03.035116+02:00"
blocks:
  - habu-convert-stack-to-6c547119
---

Full context: design sections 7.3-7.4 require an independent SIR verifier before any optimization or lowering. Validate definitions, types, effects, uses, ownership, source bindings, terminator, and schema. Acceptance: one hostile mutation per invariant rejects with location; valid SQUARE and arithmetic chains pass. Dependency: stack-to-SSA conversion.
