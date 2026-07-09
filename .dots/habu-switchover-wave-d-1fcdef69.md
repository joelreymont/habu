---
title: "Switchover wave D: ptxir-node to PRODUCT"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.012591+02:00"
---

docs/census-switchover.md sections 3+5 wave D. lib/ptx/ir.f:18 ptxir-node (only production VALUE-RECORD) to PRODUCT (or the item-15 compat layer per census-tfam-15 R8 decision); rewrite PTXIR-NODE-DROP (5 raw drops ir.f:79-80) and PTXIR-NODE-DUP-RAW (:82-88) to one layout dup/drop under item 12. GPU tile/acc/gridctx stay TK-CELL width-1 (never layout families). DEPENDS: items 12, 15 (incl. R8 decision).

R8 DECIDED (TFAM 15, 2026-07-10, docs/type-families.md 9.4): VALUE-RECORD stays a typed compat layer with touchable field<> cells; ptxir-node deliberately stays on it until a consumer needs product-typed IR rows (maki has none today, census-tfam-15 C6). Items 12+15 are landed, so this dot is UNBLOCKED whenever wanted. Recipe with the landed surface: PRODUCT ptxir-node 0 with 5 n fields; >PTXIR-NODE / PTXIR-NODE> become PTXIR-NODE:MAKE / PTXIR-NODE:UNMAKE; PTXIR-NODE-DROP becomes one layout drop, PTXIR-NODE-DUP-RAW one layout dup; the physical PTXIR-NODES array (ir.f:20-49) is untouched (storage is separate from the on-stack bundle type). Acceptance: by-value construct/destructure fixtures + no size regression (WIDTH(product)=5 = VREC parity, docs 18) + ptx-stdlib gate slice green + engine-suite VREC fixtures unaffected.
