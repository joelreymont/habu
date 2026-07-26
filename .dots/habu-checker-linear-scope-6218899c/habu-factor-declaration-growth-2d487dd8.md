---
title: Factor declaration growth throw rows
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T21:39:46.814091+02:00"
blocks:
  - habu-rename-declaration-pkg-68e009c5
---

Problem: once every throw in a quotation is required to expose one exceptional
data and return row, `DECL-TX:GROW-TABLE` fails certification because chained
size conversions execute while unrelated table and size operands remain on the
data stack.

Required result: after `habu-rename-declaration-pkg-68e009c5` lands, factor
`GROW-TABLE` so `TABLE@`, `NEXT-CAP`, each `ROWS>BYTES` conversion, the allocator
lookup, and allocator execution occur through named typed locals with no
unrelated transient operand live at a fallible call. Preserve the exact growth
algorithm, allocator callback interface, table publication order, capacity
errors, and rollback behavior. Do not weaken `THROW-EDGE`, add catches, make
`ROWS>BYTES` total, or move validation to runtime guards.

Owner: package `DECL-TX` in `src/core/decl-tx.f` and its existing generated
transaction suite only. Acceptance: the unmodified transaction suite passes.
With only the frozen `THROW-EDGE` all-row unification applied as a temporary
checker candidate, the real engine suite must certify `GROW-TABLE` and proceed
past its former `ROWS>BYTES` rejection. If that run exposes a different caller,
stop and report it instead of expanding this leaf. Typed-local and package diff
gates pass.
