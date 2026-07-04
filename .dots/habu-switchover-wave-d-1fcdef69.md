---
title: "Switchover wave D: ptxir-node to PRODUCT"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.012591+02:00"
---

docs/census-switchover.md sections 3+5 wave D. lib/ptx/ir.f:18 ptxir-node (only production VALUE-RECORD) to PRODUCT (or the item-15 compat layer per census-tfam-15 R8 decision); rewrite PTXIR-NODE-DROP (5 raw drops ir.f:79-80) and PTXIR-NODE-DUP-RAW (:82-88) to one layout dup/drop under item 12. GPU tile/acc/gridctx stay TK-CELL width-1 (never layout families). DEPENDS: items 12, 15 (incl. R8 decision).
