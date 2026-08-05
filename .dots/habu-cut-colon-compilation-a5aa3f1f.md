---
title: Cut colon compilation onto the checked chain
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.611694+02:00"
---

CG-01, phase 4 of the hard cut. src/habu/habu2.f:7020 still calls COMPILE-EMIT:EM-COMPILE; tools/bootstrap.sh:69-103 loads no src/compiler modules; strings bin/hb contains none of NMIGRATE/NREACH/IR-CTX/A64SEL/NPUB. Make checked HIR plus the native pipeline the sole compiler for normal colon definitions: complete every language capability the current tree requires (see the open capability dots: spill support habu-spill-for-real-791367e4, multi-word calls habu-let-the-chain-f28711ba, typed-local rebinding habu-rebind-a-typed-b2a3e369, local address habu-take-the-addr-18a38b4f, trapping arithmetic habu-lower-trapping-arithmetic-5f514ffe, data-word addresses habu-resolve-a-data-a1c8067f), route bootstrap and fixpoint through it, prove self-hosting to a byte-identical fixpoint, and run every gate (language, AOT, image, REPL, debugger, profiler, maki, inference) on the sole path. An opt-in migration hook is not completion. Reconcile with habu-cut-over-staged-070d68c8 and habu-self-host-staged-520ab588 — extend or close those rather than duplicating. Blocked by the bulk-window publisher dot and the phase-2 safety dots.

ADDED REQUIREMENT (2026-08-05, from habu-split-call-crossed-6eda1613 generation 5): the sole-compiler pipeline must support a pressure retry — attempt normally; on E-A64RA-SPILL re-elaborate with the call-crossing split directive (the one-line CROSS-L suppression, proven: flag on compiles CALL-PRESSURE with bit-identical answers) and re-run the chain. The bridge could not host this (evaluation and publication fused in EV); the cut's pipeline must separate compile-attempt from publish so a refused attempt costs nothing. Five generations of design evidence live in the split leaf.
