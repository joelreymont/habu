---
title: Factor field schema validator
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T12:12:06.142339+02:00"
blocks:
  - habu-harden-field-tokens-e855dc36
---

Style review finding: PF-NODE-KIND? at src/core/type-family.f:945 is a roughly 60-line recursive dispatcher mixing PARAM/CON/PTR/QUOT/APP classification and validation, violating docs/forth.md small checked-word/factor rules and obscuring fail-closed branches. Fix: split each schema kind into a named uppercase checked helper with real typed stack effect, isolate root-range and recursive-child validation, retain one shallow dispatcher, preserve exact results and error ownership, and add/retain branch-focused negative fixtures. Acceptance: no giant/multiconcern word, typed-local-diff-lint green, type-family and declaration suites unchanged green, fixpoint/full gate green. Files: src/core/type-family.f:945-1011, test/type-family-suite.f.
