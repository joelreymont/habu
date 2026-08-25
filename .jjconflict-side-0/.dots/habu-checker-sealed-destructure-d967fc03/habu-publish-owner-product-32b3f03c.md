---
title: Publish owner product surface
status: closed
priority: 2
issue-type: task
created-at: "2026-07-29T20:53:42.833583+02:00"
closed-at: "2026-08-02T22:32:09.621638+02:00"
close-reason: "Retired without landing: the STRUCTURE-MAKE owner-product surface was the CONSTRUCT-owner substrate cut from the GPT-2 milestone. Owner-only construction proceeds via a private structure declaration inside the sealed owner package per habu-seal-validated-model-4361ff22, whose checkpoint stop-rule replaces this dot; no consumer of the generator flag exists."
blocks:
  - habu-lower-owner-product-4c07eff9
---

Problem: STRUCTURE-MAKE always publishes both MAKE and UNMAKE for a public product. Result: the shared product plan selects one exact surface before its existing single preflight/evaluation. Unflagged products register and render MAKE then UNMAKE unchanged. Owner products register one UNMAKE row with its existing semantic ordinal, set the family range to that row, and render UNMAKE plus only explicitly requested equality/hash words. Replay registers the identical selected metadata and renders no words. SM-REQUIRE-UNGENERATED continues to check both names. No MAKE XREF, checker signature, dictionary entry, protection row, or hidden symbol exists for an owner product. Field accessors keep their existing visibility and the declaring package uses construct FAMILY. Add no hidden MAKE, forwarding word, trusted mint, destructor restriction, package table, runtime guard, or compatibility symbol. Owner: src/core/structure-make.f and the existing shared product plan selector only. Production red: SM-EMIT-ROWS and TDECL-PROD-WORDS currently register and render both rows unconditionally. Acceptance: XREF and real lookup find MAKE only for unflagged products; both policies publish UNMAKE; owner selection remains atomic under second-word/evaluator failure; live/replay/JIT/AOT/fixpoint generation is deterministic; structure-make, structure-declaration, generated-declaration transaction, package, native, and exact diff gates pass. Claim: unassigned.
