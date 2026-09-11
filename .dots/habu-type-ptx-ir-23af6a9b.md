---
title: Type PTX IR semantics
status: open
priority: 2
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
  - habu-libs-migrate-ptx-1071a2e6
created-at: "2026-07-19T21:06:28.360817+02:00"
---

lib/ptx/ir.f:9-16 defines seven expression operators and an absent-child marker as raw integers. Its ptxir-node PRODUCT declares op/a/b/val/live all as n (:21-27), the physical record repeats those untyped fields (:29-36), and PTXIR-WRITE-RAW/FIND-RAW/INTERN traffic six undifferentiated cells (:101-150). PTXIR-OP@, child accessors, node ids, literal values, and the -1 sentinel are all n; PTXIR-MARK and PTXIR-RENDER repeat raw equality/case dispatch (:217-224, :249-260). The checker accepts operator/node/child/value swaps, raw construction can mint unknown operators, and the renderer has no exhaustive static proof. After the owned PTX IR STRUCTURE migration and layout-kinded field capability, declare a package-owned operator ENUM, a nominal node-id, option<node-id> child fields, n value, and bool live. Store the declared typed record directly in LAYOUT-BUFFER or the unified typed STRUCTURE storage; delete PTXIR-NONE, PTXIR-WRITE-RAW, PTXIR-MATCH-RAW?, and raw pointer-field access. Make public node builders/accessors carry the semantic types and render/mark through exhaustive MATCH. Preserve canonical value numbering, node ordering, hashes, DCE, rendered kernel text, zero-allocation behavior, and exact PTX output. Add checked negatives for op/node/value/child swaps and raw/foreign enum construction, exhaustive mutation coverage for every operator and arity, malformed optional-child rejection at construction, plus CODELEN/JIT/DATA before/after measurements with no unexplained growth. Files: lib/ptx/ir.f, ir-test.f, ad-ir.f and direct consumers. Depends: habu-libs-migrate-ptx-1071a2e6 and habu-checker-capability-layout-4e7f1f03. Ownership: PTX IR semantic fields/storage only; generic STRUCTURE syntax stays with those dependencies.
