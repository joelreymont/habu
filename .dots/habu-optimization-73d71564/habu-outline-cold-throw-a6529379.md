---
title: Outline cold throw paths at tier 1
status: closed
priority: 2
issue-type: task
created-at: "2026-09-16T16:38:23.439803+03:00"
closed-at: "2026-09-30T12:34:06.389063+02:00"
close-reason: "Tier 1 is 157,272 B against tier 0's 175,832 B on the 13-file corpus (docs/compiler-measurements.md, Current size comparison). Against section 2's ratios every compiler benchmark holds; by the tier-0/tier-1 ratio five of six hold, and lines' 2.76x against 3.06x is the residual recorded in the doc."
---

Claim: agent=heron workspace=.jj-ws/tier-remeasure (taken over from alder by
agreement, confirmed by Joel). The terminal-call slice is separate from
the cold-block layout slice: only a resolved global `throw` or `die` whose entry
is inside the running engine's text may omit the returned-callee diagnostic.
The 2,265-word census finds 432 such throw sites and nine die sites. `0 throw`
unwinds to catch; package and replacement globals keep ordinary call semantics.

Problem: docs/compiler-measurements.md (2026-09-16) shows tier 1 emits 1,521 more four-instruction MOVZ/MOVK address stencils than tier 0 over 1,772 words (24,336 bytes, the whole of the 3.9 percent code-size regression and more), and every word that grew has them on a NEVER-TAKEN throw path laid out inline between the hot test and the epilogue (ARRAY:A-LEN 60 to 96 bytes with 15 instructions of inline throw; SOURCE-QPATH-CHECK 48 to 112 with 60 cold bytes). Acceptance: tier 1 lays out the cold arm of a guard (a throw, a die, an error-code push) after the word epilogue or in a shared per-module cold block, so the hot path falls through; relocatable literals that only the cold arm needs are materialised there; the census tools (tools/tier-census.f + tier-census-join.f) show the tier-1 byte total at or below tier 0 on the same corpus with the bl and frame-traffic wins intact; the three side-by-side words in the doc are re-dumped with tools/tier-dump.f and the doc updated; tier-bench numbers unchanged or better; byte fixpoint; full gate green. Files: src/compiler/native/select.f, emit.f, a64ir.f (block layout), test/compiler/*.f, docs/compiler-measurements.md. Verify: the census pair; tools/native-build.f fixpoint; test/run.f. Depends: none (habu-addr-data-cells-d232781e removes the DATA-cell stencils separately; this dot is about placement and the remaining literal stencils, which may use a per-module literal pool or ADRP/ADD, a format the AOT capture path must accept, see src/habu/aot-lib.f relocation). Ownership: tier-1 layout. Both slices landed: 1ce939b7 placement, b62ae1d2 terminal lowering, fa6e7443 capture reachability. Source: measurement lane, ranked fix 1 and 2.

Initial revalidation used c12b6e82. The historical 3.9% and word count are not
current measurements; error paths are cold candidates, not proven never taken.
Measure the current corpus and guard bodies before changing block order. Moving
a block alone does not remove its instructions: report eliminated branches and
remaining literal costs separately. Do not claim the tier-1 <= tier-0 target is
met merely because the normal path improves.

The capture companion bbf14cd4 fixes a false continuation after the fatal
engine primitive; it is separate from the emitter change. A cold error path
at a word's end otherwise retains the next private word. Its native-build
generations match and its measured build-time cost is flat. The emitter
prototype's census saves 972 bytes on c12b6e82 without changing calls, frame
traffic or literal counts; one four-byte growth occurs on an invalid-tag
path. The remaining literal cost and the tier-1 versus tier-0 total still
need work, so this dot stays open.

Remeasured on TV plus the opcode fixture and capture companion: 2,265 words,
tier 0 unchanged at 168,484 B, tier 1 186,880 -> 185,908 B (-972 B).
Calls, frame loads/stores and literal counts are unchanged. BUILD:STEP-RC@
alone grows 4 B on its invalid-tag path. Baked AOT code falls 4,356 B;
file size stays 3,539,136 B. Gen2 == gen3, SHA256
ed6ea4d59d15a8f93e8510baa5a4fba6de2974780bd632de3aa4d2e6d7695063.
The 21 focused compiler runs, float/fmath row and capture compact fixture
pass. Quiet runtime ABBA and Hazel's full landing gate remain pending.

The address-site probe counts 776 owned literal sites among the 1,903 DATA
sites. 519 carry only 16 distinct non-return fallback messages: 432 name
throw and nine name die. ARRAY:A-LEN's fallback alone occupies 40 B after
its throw call. The engine BTHROW has no ordinary return path, including
for code zero. Eliminating the fallback must authenticate the captured
primitive target, not a spelling; ordinary, qualified and redefined words
retain their own control contract. This next reduction is not implemented
by the layout change. Evidence and fresh dumps live in
`~/.cache/habu/cold-throw/source-TV/`.

## ARM64 campaign slice (design revision 3, §3.13)

Only the measured target remains. The campaign baseline on master aa478d2b
(macOS M2 Max, engine gen 2 SHA-256
95ddf031a95d1fc15b24dbb17661c30e21da76e297ea15283cd9146f88f7517d,
2,460,535 bytes) runs the 13-file corpus over 2,336 words: tier 0 175,832 B,
tier 1 157,272 B (10.5% smaller); bl 11,763 -> 5,416; ldr via sp
5,250 -> 3,756; str via sp 3,285 -> 2,370. The byte target is met. Evidence:
~/.cache/tmp/heron-arm64/baseline/BASELINE.md.

Acceptance to close: on current master, the tier pair rerun and matching the
target; ARRAY:A-LEN, SOURCE-QPATH-CHECK and DECODE-LEAD re-dumped with
tools/tier-dump.f at both tiers; tools/tier-bench.f at both tiers with load
average quoted; docs/compiler-measurements.md "Current size comparison" and
"Verdict" rewritten from the numbers. Files: docs/compiler-measurements.md and
this dot. Later campaign slices report their own tier-1 deltas; they do not
reopen this dot.
