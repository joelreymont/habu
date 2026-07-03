---
title: "TFAM 12: layout-aware stack ops + width-aware lowering"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.939694+02:00"
---

PLAN.md item 12. Logical widths for all stack prims (dup/drop/swap/over/nip/rot/-rot/tuck/2dup/2drop/2swap/2over), JIT shuffles + spilled fallbacks, VOP*/VCMP/VUN/FOP one-cell lowering, return-stack transfers, locals, ?dup rejection, constant/depth/.s, interpret mode, nested evaluate/catch-throw/run-in-stack frame metadata; width facts reach native+Gforth emitters BEFORE emission; hidden fields cannot bind ordinary effects/quotations/combinators/control predicates; possibly-linear layout copies reject until TFAM 11. Gate 17h. Depends: TFAM 7.
