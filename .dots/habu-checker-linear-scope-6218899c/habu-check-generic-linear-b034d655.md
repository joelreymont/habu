---
title: Check generic linear owner scopes
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:49:06.223548+02:00"
blocks:
  - habu-prove-catch-restores-2f368434
---

Problem: ordinary catch is stack-preserving, so it cannot express a body that transforms a linear owner to an arbitrary success row while cleanup consumes the original owner only on throw. Required interface: LINEAR-SCOPE:WITH has effect ( R a [ R a -- S ] [ R a -- R ] -- S ). R may contain arbitrary typed values, including other linear owners, and must remain the identical type row on every exceptional edge and after cleanup. a is one concrete whole-bundle linear owner, including a compound STRUCTURE or ENUM whose family is classified linear; a raw row variable or several unrelated owners cannot satisfy it. Its checker effect is attached to the resolved symbol identity, so qualified and using-imported calls are identical; token-string matching is forbidden. The body may return any S normally, but every catchable exceptional row must equal R a. The cleanup quotation must consume exactly a, preserve R, and have no catchable throw; process no-return remains allowed. Reject non-linear a, wrong rows, hidden owner duplication, and cleanup that returns or replaces the owner. Owner: checker effect and focused fixtures only; no runtime body. Dependency: habu-prove-catch-restores-2f368434. Acceptance: checked fixtures prove scalar and compound owners, unrelated linears in R, success transfer, success consumption, throw cleanup eligibility, nesting, imports, and every forbidden shape through CHECK!/bin/hb.
