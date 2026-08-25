---
title: "EPIC: hard-cut native codegen onto production"
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.485563+02:00"
---

The hard cut: the checked HIR/native chain becomes the sole colon compiler; the old emitter, NMIGRATE, NREACH, the replacement log, and the CODE-RECLAIM bridge watchers are deleted. No compatibility versions, fallback dispatch, adapters, or dual compilation. Canonical review: /tmp/REVIEW-CODEGEN-1.md (CG-01..CG-32; copies on spark:/tmp).

DONE: tip repairs (CG-03..06); safety phase — TFX/SVX single-writer rewind (CG-23), reserved-register derivation + NZCV truth (CG-13/14), transactional builder + unconditional context retirement with Storage.v proofs (CG-07/08), hash-index liveness + indexed imported lookup (CG-25/26); the bulk atomic publisher (CG-15/16/18); deletions — codec stack, arena rollback, substring assertions, comparison scaffolding (CG-10/12/30-part/31/32); measurement harness with clang column + chain self-baseline; first optimization (literal CSE, 8 rows improved).

REMAINING, in order: CG-22 (habu-seal-the-declaration-7183177e, primary surface declaration-transaction.f), CG-24 (habu-own-pkg-state-acf7086c), then the cut itself (habu-cut-colon-compilation-a5aa3f1f — prerequisites listed there), the deletions (habu-delete-the-old-679cfd35), proofs-to-dispatch (CG-02, habu-tie-instruction-proofs-fe3bef68), the post-cut harness collapse (habu-collapse-the-old-63b152cd). Optimization dots continue in parallel, each measured on the harness with both-gaps reporting.
