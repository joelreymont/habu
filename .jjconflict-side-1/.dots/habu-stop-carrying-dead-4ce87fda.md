---
title: Stop carrying dead locals across ordinary calls
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T02:43:19.831040+02:00"
---

The root over-approximation the sel-tail landing left in place, named as its non-goal: elaborate.f marks a local as travelling per DEFINITION (LCROSS? = some call can reach some mention), and CALL-OPERANDS+ hands every travelling local across EVERY call - paying a store and a load at ordinary sites where the local is dead after the call. The tail case is now handled structurally in select.f (TAIL-DEAD-CK); the general case is per-site liveness for crossing locals. This changes every call site's bytes tree-wide - the judge board's next scalar prize; re-pin the baseline with the landing and report both gaps per touched row. Files: src/compiler/native/elaborate.f. Depends: none technically; measure against the judge board.
