---
title: Decide bignum strategy or cap 100% claim explicitly
status: open
priority: 3
issue-type: task
created-at: "2026-03-07T19:32:55.828895+01:00"
blocks:
  - habu-audit-remaining-numeric-0f3ca69d
---

Runtime numeric representation and arithmetic primitives. Root cause: true 100% Maxima parity may be impossible without unbounded integers. Fix: either implement a real bignum strategy or explicitly cap the parity claim and document the blocker. Why: final hard-cutover closure item for the numeric tower question.
