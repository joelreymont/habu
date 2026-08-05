---
title: Cut colon compilation onto the checked chain
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.611694+02:00"
---

CG-01, phase 4 of the hard cut: make checked HIR plus the native pipeline the sole compiler for normal colon definitions. habu2.f:7020 COMPILE-EMIT:EM-COMPILE is the single production entry (verified, no drift); route it and bootstrap through the chain, prove self-hosting to a byte-identical fixpoint, run every gate on the sole path, then delete the old emitter and bridges (habu-delete-the-old-679cfd35). An opt-in hook is not completion. Reconcile with habu-cut-over-staged-070d68c8 / habu-self-host-staged-520ab588.

PREREQUISITES (scout-verified 2026-08-05/06):
1. Data-word addresses (habu-resolve-a-data-a1c8067f) — the one hard capability blocker; lane in flight.
2. Spill rewrite loop into production: migrate-era EMITTED never rewrites (fits-or-refuses; survivable only while the old emitter is the fallback). The pipeline runs allocate -> if spills planned, A64SPILL:REWRITE -> re-validate -> emit. This is CG-28's requirement concretely.
3. Pressure retry: attempt normally; on E-A64RA-SPILL re-elaborate with the CROSS-L split flag (proven one-liner; evidence in habu-split-call-crossed-6eda1613) and re-run. The publisher's VALIDATE/COMMIT split already gives refusal-moves-nothing; the pipeline must keep compile-attempt separate from publish so retries are free.
4. Two whole-tree probes before finalizing order: any definition certified under CNUM-OVERFLOW:TRAP reaching hir arithmetic (E-A64SEL-TRAP refuses it today), and any use of to/^ on typed locals (dialect refuses; corpus-based verdicts do not cover the tree).
5. Seed: derive by transitive closure from the chain's entry (ir/* before native/*; NREACH is not in the closure — decide the seed by closure, not directory).

The dispatch path that reads routine records concurrently must acquire START (LDAR) — the publisher writes with release; the acquire half is unexercised until this lands (noted in habu-re-express-the-13d7558c).
