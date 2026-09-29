---
title: Measure tier-1 latency on spark
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.843548+03:00"
---

Problem: tier-1 compile is 2.51 ms/word against 0.105 at tier 0 (`docs/compiler-measurements.md:270-281`; 1,772 words in 3.88 s); the x86 engine is tier-1-only, so its gate is expected within ~2x of ARM64 wall time but unmeasured. G4b repeats the measurement on the ThinkPad.
Acceptance: on spark, `time` five heavy suites with and without a `1 set-tier` prefix; recorded in `docs/compiler-measurements.md`.
Files: `docs/compiler-measurements.md`.
Verify: spark: the five suites timed at both tiers.
Depends: none.
Route: direct (documentation no build loads; the Alder route exists for files a macOS build loads).
Ownership: krait (Intel lane).
Claim: unassigned.
