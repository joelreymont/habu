---
title: Widen the tier 0 register pools
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T11:26:08.312918+02:00"
---

Joel 2026-10-26. The tier 0 integer pool is 13 registers (x9-x15, x29, x21-x25; src/habu/regalloc.f:25-26) and the float pool 8 (d8-d15, FRALL). Two widenings: (1) free part of x0-x8 for the integer pool: primitive bodies hard-code them as scratch and syscall arguments, so this follows step 2 (primitives operate on pool-assigned registers); (2) add d16-d31 to the float pool after checking nothing else uses them (d0-d7 stay primitive scratch). Do it with the join allocator (T0-04 minus aliases), which keeps values live across joins and raises register pressure; straight-line code does not need it. Also establish what x27 holds. Measure on zz and tender before and after.

Claim: jerry. x27 holds NDICT (layout.f:6), so it stays out of the pool. Landed bbf36f61 "Add x6-x8 to the tier 0 integer pool" (16 registers) and 513d0aa7 "Add d16-d31 to the tier 0 float pool" (24; FRALL derived from #FPOOL; P-FPRESS uses 23 terms, the first count that exhausts the pool). Gate (~/.cache/tmp/jerry-gate2/): two-gen converged; full suite 507/512 at load 31; lsp-boundary, check-drivers, check-verify-batch, lsp-batch and aot-seed-metadata pass alone (alone/). Measurement against the fusion snapshot: brief ~/.cache/tmp/jerry-jmeasure-brief.md, results ~/.cache/tmp/jerry-jmeasure/.
