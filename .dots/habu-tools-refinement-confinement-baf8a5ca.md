---
title: "tools: refinement-confinement lint (owner-file-only TRUSTED mints)"
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-13T10:31:33.787498+02:00\\\"\""
---

Plan-vs-code audit gap (2026-07-13, goal e93371de): the *-REFINE / RAW>* TRUSTED refinement pairs (tensor.f DIM/ROWS/COLS/SPACE-REFINE, model-ir RAW>NODE/SLOT/REF, fusion-plan RAW>RGN, tensor-value RAW>TENSOR, onnx IMP-*) are package-private but reopen-callable: any new maki file can call ROWS-REFINE bare and forge CAD-KIND:rows from unvalidated n with NO gate failing. Zero cross-file callers today - by convention only; no lint scans refinement call sites (trust-lint pins rows to TRUSTED.md; checked-boundary-lint forbids broad set-check; maki-dep-lint is the habu<-maki fence). FIX: an inventory-driven call-site confinement lint - for every TRUSTED row classified as a refinement mint, assert its callers are inside the owning file (allowlist for documented exceptions like PLAN-GATHER-ROWS-class shape-algebra words), red fixture proving it fires, wire into the lint gate. INTERIM enforcement only: the principled endpoint is tfam's TVK-RAW checker capability (habu-nominal-storage-raw-a3430ef2) which closes the mint at unification; retire the lint's mint-class when that lands (note in the lint header). The PROJECTION direction (family->n) stays per-site-classified (the raw-audit's boundary table) - lint scope is the REFINE/mint direction only.
