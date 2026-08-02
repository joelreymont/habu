---
title: Compile float comparison and branching
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-02T23:35:03.377700+02:00\""
---

Second float compiler leaf, against the corpus3 survey contract. Scope: f< f> f= f0< f0= as flag-producing words (FCMP + CSET + the flag SUB, matching the engine's exact sequence per the survey) AND fused into branches (the cmpbr machinery generalizes - a float compare feeding a brz becomes FCMP + B.cond; work out the condition table for the five words including the NaN fact: every comparison is FALSE on NaN, so the fused condition must be the one that FALLS THROUGH on unordered - MI/GT/EQ are all NaN-false, verify each against the survey's probes and pin by execution with NaN operands). Acceptance: RELU-F, MAX-F and FROUND gap rows retire bit-exact - RELU-F's pinned inputs include NaN and -0.0 so the NaN-takes-the-else-arm fact is executed, FROUND covers f>s in the corpus (closing the scalar leaf's stated coverage gap). Mutations: fused float condition inverted (execution on NaN input - the arm swap shows), unordered handling wrong (NaN input kill), CSET condition wrong per word, flag not all-bits-set. Blocks on nothing - the scalar leaf landed.

Claim: agent=fcmplane workspace=.jj-ws/habu-compile-float-comparison-9a65655f
