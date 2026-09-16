---
title: Support generic value parameters instantiated with closed products
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-14T14:12:28.625536+03:00\""
closed-at: "2026-09-16T14:34:52.061982+03:00"
close-reason: "superseded by habu-campaign-c3-the-a2477c89: Residue: a scalar type variable still cannot bind a closed product at a generic call boundary, so Tender's APPLY-PROOF shape is refused."
---

Tender checked reproducer: PRODUCT proof with two n fields; APPLY-PROOF ( a [ a -- bool ] -- bool ) execute; a concrete proof and [: BOTH ;] reject at APPLY-PROOF on frozen source9d0a44a3/hb24d8075c and rebuilt hb8abab834. Reproducer ~/.cache/tender/verification/independent-role-review/question-proof-min.f. Current LAYOUT-BLOCK? deliberately prevents scalar type variables binding hidden product fields at generic call boundaries; removing the refusal alone would misstate the fixed native cell window. Design and implement whole-value width instantiation or specialization with checked call metadata and code generation, preserving nominal identity, grouped input/results and higher-order quotation variance. Cover two distinct widths, saved prefixes, both tiers, and wrong-family/borrow negatives. Existing row-polymorphic APPLY-PROOF ( R [ R -- bool ] -- bool ) execute passes the concrete two-cell callback plus a saved scalar and depth assertions on hb8abab834 in AOT; this is supported stack-row composition, not general single-value polymorphism. Do not disable checking or classify the scalar-variable refusal as a new soundness bug. Review found during Tender coordination 2026-09-14; performance work remains deferred.
