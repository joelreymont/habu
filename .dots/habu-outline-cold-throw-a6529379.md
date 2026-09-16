---
title: Outline cold throw paths at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:38:23.439803+03:00"
---

Problem: docs/compiler-measurements.md (2026-09-16) shows tier 1 emits 1,521 more four-instruction MOVZ/MOVK address stencils than tier 0 over 1,772 words (24,336 bytes, the whole of the 3.9 percent code-size regression and more), and every word that grew has them on a NEVER-TAKEN throw path laid out inline between the hot test and the epilogue (ARRAY:A-LEN 60 to 96 bytes with 15 instructions of inline throw; SOURCE-QPATH-CHECK 48 to 112 with 60 cold bytes). Acceptance: tier 1 lays out the cold arm of a guard (a throw, a die, an error-code push) after the word epilogue or in a shared per-module cold block, so the hot path falls through; relocatable literals that only the cold arm needs are materialised there; the census tools (tools/tier-census.f + tier-census-join.f) show the tier-1 byte total at or below tier 0 on the same corpus with the bl and frame-traffic wins intact; the three side-by-side words in the doc are re-dumped with tools/tier-dump.f and the doc updated; tier-bench numbers unchanged or better; byte fixpoint; full gate green. Files: src/compiler/native/select.f, emit.f, a64ir.f (block layout), test/compiler/*.f, docs/compiler-measurements.md. Verify: the census pair; tools/native-build.f fixpoint; test/run.f. Depends: none (habu-addr-data-cells-d232781e removes the DATA-cell stencils separately; this dot is about placement and the remaining literal stencils, which may use a per-module literal pool or ADRP/ADD, a format the AOT capture path must accept, see src/habu/aot-lib.f relocation). Ownership: tier-1 layout. Claim: unassigned. Source: measurement lane, ranked fix 1 and 2.
