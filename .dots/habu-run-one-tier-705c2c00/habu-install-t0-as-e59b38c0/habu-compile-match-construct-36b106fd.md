---
title: Compile MATCH, construct and is in T0
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T21:50:37.138119+02:00"
---

A3b. The arm64 corpus census (jerry-t0-l1/.out/a64-census.md sec. 3) refuses 2714 distinct bodies E-T0-UNSUPPORTED: MATCH 1987, construct 579, is 84, match 10, ;match. Tier 1 elaborates them in src/compiler/native/elaborate.f 2009-2260 (MATCH-SCAN, MSCAN-*), 3137 MATCH-TRAP, over NFAM (family.f) and NDICT:MATCH-*. T0 compiles the same semantics through the seam's structured control. No fallback: every corpus body using them must compile under T0. Base: A2.

Split: A3b1 MATCH + construct with checker token-ordinal counting (jerry-a3b qtrktsss, E-T0-MATCH -9606); A3b2 `is` (jerry-a3b2 sskqllnx, E-T0-DEFER -9607). Both on movnmywp, running in parallel; proofs end to end in test/t0-install.f plus tier 1's own MATCH / is tests run under T0.
