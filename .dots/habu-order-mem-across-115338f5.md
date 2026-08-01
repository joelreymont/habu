---
title: Order memory across blocks in the native chain
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T11:58:25.964772+02:00"
---

The elaborator threads ONE memory-order value per definition and mints it (hir.mem) at the first memory word it meets, in whatever block that is. A definition whose store is in one arm of a branch and whose load is after the join therefore builds a token that does not dominate its use, and IR-VERIFY refuses it by name at freeze. Wanted: the order crosses an edge the way every other live value does - as a block argument of the token type - so a loop body with a store (BYTE-SUM) elaborates. Depends on the cell-width memory leaf (habu-compile-mem-access-64ae47d3).
