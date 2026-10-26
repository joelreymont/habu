---
title: Fuse tier 0 compares with branches
status: closed
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:18.237762+02:00"
closed-at: "2026-10-26T19:56:38.172131+02:00"
close-reason: landed and measured
---

Experiment step after step 2. VCMP-ENTRY (jit.f:516-525) emits cmp;cset;neg then cbz: fuse compare+branch into cmp;b.cond, and use compare-immediate (LVBINIPREP, not LVBINPREP) for literal operands. zz 18%, tender 25% of the tier gap. Measure as step 2.

Design: ~/.cache/tmp/jerry-fuse-design.md (adopted). Claim: jerry.

Lanes (master 5d046858):
- A, compare-immediate in VCMP-ENTRY: workspace .jj-ws.noindex/jerry-fusecmpi, handoff ~/.cache/tmp/jerry-fusecmpi-handoff.md.
- B, LVFUSE compare+branch peephole: workspace .jj-ws.noindex/jerry-fuse, handoff ~/.cache/tmp/jerry-fuse-handoff.md.
Settled before launch: LCFPOP clobbers x16 (frame it across LVRECON/LCFPOP); LCEMIT grows the band when CP-8 falls below PROT:WLO, so LVFUSE needs no band guard.

Landed: 47fb81ee "Compare tier 0 registers with literal immediates" and 56f8098b "Fuse tier 0 compares into conditional branches"; Fable reviews ACCEPT; two-gen byte-identical on each; full suites ran under external load (66-216), and every red row passed alone or reproduced on the unmodified base.
Measurement lane: jerry, worker over .jj-ws.noindex/jerry-fmbase (5d046858) and jerry-fmcand (fusion snapshot 864df4c2); brief ~/.cache/tmp/jerry-fmeasure-brief.md; results ~/.cache/tmp/jerry-fmeasure/; handoff ~/.cache/tmp/jerry-fmeasure-handoff.md.

Result (engine 5d046858 vs fusion 864df4c2 tree, user CPU medians, alternating reps, load 58->8; outputs identical across base t0, cand t0, cand t1; artifacts ~/.cache/tmp/jerry-fmeasure/): tier-bench kernels 18352 -> 17746 us (-3.3%, t1 5023; closes 4.5% of the t0->t1 gap), branch 8919 -> 8364 (-6%, closes 7.5%); dots ls x300 run 1.55 -> 1.56 s (t1 1.25), tree 3.29 -> 3.33 (t1 2.85), no measurable change; compile-floor tier 0 trivial least 32 us on both. Code shape: SPLIT-LINES cset/neg/cbz 3/3/3 -> 0/0/0, b.cond 0 -> 3, 98 -> 91 instructions. Not measured: zz (main.f requires lib/*.f, now src/lib/) and tender (zen src/document/cfb.f:28 uses VEC-HEADER-BYTES, private in package VEC since the typed VEC API). Retirement goal not met: tier 0 run is 1.17-1.24x tier 1 on dots, 3.5x on the kernels.
