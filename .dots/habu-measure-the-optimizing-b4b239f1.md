---
title: Measure the optimizing tier against tier 0 and the pre-IR compiler
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-16T12:15:08.672866+03:00\""
---

Problem: Joel asks whether the IR compiler with its optimization passes (src/compiler/ir, src/compiler/native: regalloc, spill, coalescer) pays off; the 2026-09-16 build profile says its output is bloated today (15 percent dead reload/store pairs, uninlined one-instruction primitives, unfolded constant predicates, cold throw paths inline; RD@ is 202 instructions for four loads and three compares) and the prefix still compiles at tier 0 because tier 1 costs about 60 ms per word. Nobody has a number for code bytes or run time per tier against a fixed corpus, nor against the pre-IR stencil compiler. Acceptance: a tools/ census that compiles a fixed corpus (src/core plus lib/string.f, lib/json-read.f and the lib/task.f words) at tier 0 and tier 1 and reports bytes per word, instructions per word, and BL-to-primitive counts, plus a benchmark of five representative words (a string search, a JSON parse, a wide transfer, a MATCH dispatch, an arithmetic loop) timed per tier; the same census run on the last pre-IR engine that still builds (find the revision; if none builds, say so and measure tier 0 as the baseline); results in LESSONS.md with the verdict and the next three fixes ranked by measured bytes and time; rerun after habu-elide-same-slot-443de377 and habu-inline-trivial-engine-922133ca land. Files: a new tools/tier-census.f and tools/tier-bench.f, LESSONS.md. Verify: the tools through bin/hb. Depends: habu-elide-same-slot-443de377, habu-inline-trivial-engine-922133ca (measure before and after). Ownership: hazel line. Claim: agent=hazel-measure-tier workspace=.jj-ws/hazel-measure-tier.
Evidence 2026-09-16 (72f0d103, docs/compiler-measurements.md): tier 1 runs
1.1x-9.3x faster than tier 0 on six real benchmarks, halves bl count, costs
+3.9 percent bytes (1,521 extra MOVZ/MOVK stencils on cold throw paths, 24,336
bytes) and 23.8x compile time per word; tier 0 IS the pre-IR compiler; the
self-build is 3,566 prefix words at tier 0 + 1,808 assembled at tier 1 and
refuses tier 0 by design. Stays open for the rerun after inline-prims and the
outlining land (tools/tier-census.f, tier-census-join.f, tier-bench.f,
tier-dump.f are the instruments); fixes tracked as habu-addr-data-cells-d232781e,
the outline-cold-throw dot and the x19 identity dot filed today.
