---
title: Fuse tier 0 compares with branches
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T11:28:18.237762+02:00"
---

Experiment step after step 2. VCMP-ENTRY (jit.f:516-525) emits cmp;cset;neg then cbz: fuse compare+branch into cmp;b.cond, and use compare-immediate (LVBINIPREP, not LVBINPREP) for literal operands. zz 18%, tender 25% of the tier gap. Measure as step 2.

Design: ~/.cache/tmp/jerry-fuse-design.md (adopted). Claim: jerry.

Lanes (master 5d046858):
- A, compare-immediate in VCMP-ENTRY: workspace .jj-ws.noindex/jerry-fusecmpi, handoff ~/.cache/tmp/jerry-fusecmpi-handoff.md.
- B, LVFUSE compare+branch peephole: workspace .jj-ws.noindex/jerry-fuse, handoff ~/.cache/tmp/jerry-fuse-handoff.md.
Settled before launch: LCFPOP clobbers x16 (frame it across LVRECON/LCFPOP); LCEMIT grows the band when CP-8 falls below PROT:WLO, so LVFUSE needs no band guard.

Landed: 47fb81ee "Compare tier 0 registers with literal immediates" and 56f8098b "Fuse tier 0 compares into conditional branches"; Fable reviews ACCEPT; two-gen byte-identical on each; full suites ran under external load (66-216), and every red row passed alone or reproduced on the unmodified base.
Measurement lane: jerry, worker over .jj-ws.noindex/jerry-fmbase (5d046858) and jerry-fmcand (fusion snapshot 864df4c2); brief ~/.cache/tmp/jerry-fmeasure-brief.md; results ~/.cache/tmp/jerry-fmeasure/; handoff ~/.cache/tmp/jerry-fmeasure-handoff.md.
