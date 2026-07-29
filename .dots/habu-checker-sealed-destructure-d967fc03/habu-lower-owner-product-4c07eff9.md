---
title: Lower owner product construction
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:53:42.745487+02:00"
blocks:
  - habu-check-owner-product-b610e7ef
---

Problem: a checked construct FAMILY product form has no native or hosted lowering. Result: reuse the existing product layout: field cells are already on the stack, so lowering emits only the same declared-width padding operation required by public MAKE, with no call, allocation, lookup, owner cell, branch, proof token, or code-size heuristic. Preserve field order, nominal roles, interpreter/JIT/AOT parity, wide layouts, and existing sum/enum construct lowering. Missing checker certification cannot reach lowering. Owner: existing construct and product-layout lowering seam only. Production red: a certified owner product cannot execute. Acceptance: an internal owner construction and the prior public MAKE fixture yield byte-identical product cells; nested, wide, linear, JIT, AOT, and native fixpoint cases pass; swapped/dropped roles reject; compiler and exact diff gates pass. Claim: unassigned.
