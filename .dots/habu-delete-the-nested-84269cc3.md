---
title: Delete the nested verification window guards
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T07:38:49.502635+01:00"
blocks:
  - habu-delete-the-trusted-42b30edd
---

Problem: E-VERIFY-WINDOW at src/habu/verify-source.f:4326 (a composition inside a composition) and src/core/checker.f NESTED-REJECT (~:14293) are run-time guards no checked program reaches under the kept-quotation rule: the only public routes into an open window run a kept quotation (THEN-ACTION, the ON-CANDIDATE defer) whose types state no facts. A TRUSTED: body still reaches :4326 (/tmp/jerry-q2d/f1/cn-trusted.f). Acceptance: both guards and the code only they reach are deleted once TRUSTED: is gone; PENDING-REJECT (checker.f ~:13803) stays, test/load-reject-diag-test.f TEST-REPLAY-TASK covers it. Depends: habu-delete-the-trusted-42b30edd. Claim: unassigned.
