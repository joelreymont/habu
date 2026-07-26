---
title: Implement generic linear owner scopes
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:49:21.319845+02:00"
blocks:
  - habu-check-generic-linear-b034d655
---

Problem: callers hand-write catch, stack restoration, cleanup, and rethrow around linear owners, and current one-off scope helpers park ownership in process-global cells. Required result: add package LINEAR-SCOPE with public WITH implementing the checker-frozen effect ( R a [ R a -- S ] [ R a -- R ] -- S ). Store both quotation capabilities in call-local state, execute the body under native catch, return S unchanged on success, and on catchable throw run cleanup over the restored R a then rethrow the original code. No process-global owner or quotation cells, no swallowed cleanup code, no fallback, and no owner copying. The checker guarantees cleanup has no catchable throw; a process no-return cleanup is allowed. Owner: one checked Habu module, its load registration, trust inventory only for the irreducible row-polymorphic runtime boundary, and focused tests. Dependency: habu-check-generic-linear-b034d655. Acceptance: a synthetic DEFLINEAR owner with a live counter proves normal transfer, normal consumption, body throw cleanup exactly once, nested/reentrant calls, original throw preservation, and zero final owners through the production module load; package and typed-local gates pass.
