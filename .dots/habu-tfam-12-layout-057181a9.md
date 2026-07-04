---
title: "TFAM 12: layout-aware stack ops + width-aware lowering"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.939694+02:00"
---

PLAN.md item 12. Logical widths for all stack prims (dup/drop/swap/over/nip/rot/-rot/tuck/2dup/2drop/2swap/2over), JIT shuffles + spilled fallbacks, VOP*/VCMP/VUN/FOP one-cell lowering, return-stack transfers, locals, ?dup rejection, constant/depth/.s, interpret mode, nested evaluate/catch-throw/run-in-stack frame metadata; width facts reach native+Gforth emitters BEFORE emission; hidden fields cannot bind ordinary effects/quotations/combinators/control predicates; possibly-linear layout copies reject until TFAM 11. Gate 17h. Depends: TFAM 7.

Constant follow-up (from TFAM 5 const-b89c90f0): native C-CONSTANT + verify-source RECORD-DEFINER? + public-signatures PS-MAYBE-TRUST-DEFINER + all-errors CA-ADD-SUPPORT-CONSTANT all narrow a layout-family constant value to one-cell `-- a` (native accepts the constant, layout USE fails downstream). When this item makes `constant` reject (or multi-cell shape-carry) layout values at the value-pop, remove the `-- a` boundary comments at those sites and flip the parity fixtures `const-layout-narrow` (tools/check-all-errors-test.f) + PST-TEST-CONST-LAYOUT (tools/public-signatures-test.f) from "layout USE rejected" to accepted/shape-carried.
