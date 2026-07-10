---
title: "Switchover wave D: ptxir-node to PRODUCT"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.012591+02:00"
---

docs/census-switchover.md sections 3+5 wave D. lib/ptx/ir.f:18 ptxir-node (only production VALUE-RECORD) to PRODUCT (or the item-15 compat layer per census-tfam-15 R8 decision); rewrite PTXIR-NODE-DROP (5 raw drops ir.f:79-80) and PTXIR-NODE-DUP-RAW (:82-88) to one layout dup/drop under item 12. GPU tile/acc/gridctx stay TK-CELL width-1 (never layout families). DEPENDS: items 12, 15 (incl. R8 decision).

R8 DECIDED (TFAM 15, 2026-07-10, docs/type-families.md 9.4): VALUE-RECORD stays a typed compat layer with touchable field<> cells; ptxir-node deliberately stays on it until a consumer needs product-typed IR rows (maki has none today, census-tfam-15 C6). Items 12+15 are landed, so this dot is UNBLOCKED whenever wanted. Recipe with the landed surface: PRODUCT ptxir-node 0 with 5 n fields; >PTXIR-NODE / PTXIR-NODE> become PTXIR-NODE:MAKE / PTXIR-NODE:UNMAKE; PTXIR-NODE-DROP becomes one layout drop, PTXIR-NODE-DUP-RAW one layout dup; the physical PTXIR-NODES array (ir.f:20-49) is untouched (storage is separate from the on-stack bundle type). Acceptance: by-value construct/destructure fixtures + no size regression (WIDTH(product)=5 = VREC parity, docs 18) + ptx-stdlib gate slice green + engine-suite VREC fixtures unaffected.

## SLICE 1 — LANDED (ptxir-node -> PRODUCT, whole node surface)

Executed exactly per the R8 recipe. AUDIT FIRST: ptxir-node was the only
production VALUE-RECORD, and every ptxir-node-typed word lives in lib/ptx/ir.f —
the bundle type never escapes the file (consumers use the id-based API), so the
"first slice" is the complete node migration. Radius: 1 file + its test.

CAPABILITY PROOF (before committing): a standalone 5-field `PRODUCT p5node 0`
fixture declared, MAKE/UNMAKE certified, item-12 layout dup/drop on the 5-cell
bundle ran correctly, and a wrong-arity MAKE rejected (CHECK-QUIET-CANDIDATE! 0).
Layout-S2 is NOT needed: ptxir-node's five fields are plain `n` cells; the S2
tier only gates layout-family-typed fields.

Changes (lib/ptx/ir.f): VALUE-RECORD decl → `PRODUCT ptxir-node 0` with FIELD
op/a/b/val/live n; `>PTXIR-NODE` / `PTXIR-NODE>` became thin wrappers over
`PTXIR--NODE:MAKE` / `PTXIR--NODE:UNMAKE` (NOTE the DOUBLE HYPHEN: hyphenated
family names escape `-` as `--` in the derived constructor package, same as
map's SLOT--STATE:*; the docs §? "raw hyphen concatenation" rule); the wrappers
keep every in-file caller (WRITE/MATCH?/FIND/INTERN) textually unchanged.
PTXIR-NODE-DROP: 5 raw drops → ONE layout drop. PTXIR-NODE-DUP: unpack+rebuild →
ONE layout dup. PTXIR-NODE-DUP-RAW: `MAKE dup`. The physical PTXIR-NODES array +
PTXIR-REC structure + all id-based accessors untouched (storage is separate from
the on-stack bundle type). WIDTH(product)=5 = VREC parity — no size regression
(same cells on the stack; MAKE/UNMAKE are physical no-ops).

Acceptance evidence: PTXIRT-NODE-PRODUCT fixture (by-value MAKE→UNMAKE roundtrip
field-by-field, NODE-DUP and DUP-RAW both-copies-sum, DROP shape) + the whole
ptx suite green focused (ir/opt-ir/opt/ad tests) + ptx-stdlib slice inside
test/run.f + engine-suite VREC fixtures (point/rect/box/hdl) unaffected — they
stay on VALUE-RECORD by design (R8: VREC remains the engine compat layer).
VALUE-RECORD now has ZERO production users; remaining uses are engine/decl-suite
fixtures only.
