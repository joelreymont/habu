---
title: Elide identity moves of the data-stack pointer
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T16:38:23.446409+03:00"
---

Problem: tier 1 emits `sub x19,x19,#16` immediately followed by `add x19,x19,#16` with nothing between (measured 2026-09-16 in SOURCE-QPATH-CHECK, docs/compiler-measurements.md), the data-stack twin of the frame identity copy that MB-IDENTITY-COPY? already removes for spill slots; the census shows sp loads and stores barely moved between tiers (6,483 to 6,324) because stack-pointer adjustments are planned per block edge and not cancelled across them. Acceptance: the planner cancels an adjustment of x19 that is undone before any use of the stack memory it exposed, and merges adjacent adjustments into one; a fixture pins the shape (two consecutive words each consuming and producing the same depth); the census pair shows the sp-adjust count drop and no benchmark regression; byte fixpoint; full gate green. Files: src/compiler/native/regalloc.f, spill.f, select.f, test/compiler/*.f. Verify: tools/tier-census.f before and after; tools/tier-bench.f; tools/native-build.f fixpoint; test/run.f. Depends: habu-inline-trivial-engine-922133ca lands first (same emitter region). Ownership: tier-1 register planner. Claim: agent=alder workspace=.jj-ws/alder-dstack-adjust, based on 31bcf100. Source: measurement lane, ranked fix 3.

Hazel released this lane after 31bcf100; the stated 922133ca prerequisite is
already closed. Reproduce on the current engine before editing the planner;
the acceptance above is unchanged.

Implemented in the released emit.f seam on 551b133d: one per-operation plan
combines adjacent signed x19 adjustments inside a block. Calls, transfers and
all other operations remain barriers; the accepted IR is unchanged. Layout and
writing read the same plan. An unencodable sum keeps its original moves.

Measured with the faa44fcc baseline and the private candidate: 2,265-word
tier-1 corpus 200,396 -> 194,492 code bytes; x19 adjustments 5,429 -> 3,953;
all other census columns unchanged, no word grows. Tier-0 reports are identical.
The equal/unequal-move regressions fail before and pass after; runtime results,
sentinels and both branch arms pass. Seven focused registry rows pass. Private
gen2 == gen3, SHA256 e1bf7f256609db32064c480873fdc98ca895329b0b3a5de68fb51564975fc514;
engine 3,670,208 -> 3,604,672 bytes, AOT code 1,765,120 -> 1,718,536 bytes.
Two tier-bench ABBA pairs show no measured regression: medians of the four
reported medians differ by less than 0.2% in every row. The compile-floor ABBA
reports trivial-t1 377-431 us before and 371-386 us after (200 definitions
certified each run). Load was 2.20-4.50, with one Odin job throughout and a
Tender child appearing in the final process snapshot; no engine build or full
gate overlapped. Hazel's chain owns the full gate.
Reproduction sources and logs: ~/.cache/habu/dstack-adjust/source-31bcf100/.
