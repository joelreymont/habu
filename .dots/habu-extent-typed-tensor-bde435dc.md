---
title: "Extent-typed tensor accessors (TENSOR:/EXTENT:)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T13:19:18.326332+02:00\""
---

docs/golden-syntax.md candidate B (recommended). Defining words TENSOR:/ITENSOR:/EXTENT: with accessor sigs carrying nominal extent roles idx<#M>; loop induction binds to iterated extent. Rides MISSING.md Foundation A (declarable integer roles) — coordinate with habu-habu-quirk-fixes lineage; maki side ~30-line defining word + signature emission. First user: gathered-GEMM golden. After Foundation A lands (checker prerequisite).

SUBSTRATE (decided, docs/extent-substrate.md; destruction-reviewed): TFAM families. EXTENT: #M mints a package-scoped arity-0 TFAM cell family; idx is an arity-1 TFAM family; TENSOR:/ITENSOR: accessor sigs carry idx<extent-tail>. Two extents distinct + flip reject rides TFAM parametric identity unification (proven, test/extent-substrate-probe.f). The index-to-n crossing (offsets/arithmetic) is the idx family's responsibility, NOT the extent's — extents are phantom type args with no runtime value. Typo protection is free (undeclared tail = unknown-signature reject). MANGLING (#M -> tail): must satisfy TDECL-RESERVED? (sumtype.f:152-160) — no single-letter tails, no atom prefixes (extent-/space-/mask-/block-/geom-/parity-/align-), no builtin/CT-role names, no control/keyword tokens; e.g. extm/extb class tails. Product #B*#T is BTC-7's remit (habu-extent-role-product-8e364885) — do not preclude it: arity-2 product family rides the same parametric unification. Legacy extent-* atoms coexist during transition; migration tracked by habu-migrate-extent-atom-d1dc3611.

Claim: agent=tensor-opus workspace=.jj-ws/habu-extent-typed-tensor-bde435dc
