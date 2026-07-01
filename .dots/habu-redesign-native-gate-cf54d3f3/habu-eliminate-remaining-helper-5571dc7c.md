---
title: Eliminate remaining helper and inner hb launches
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-30T23:24:33.517886+02:00\""
---

Problem: Mac hot test suite still reports helper-spawn around 69, inner-hb around 13, inner-hb-stdin around 5, boundary around 18. This keeps suite-body time high even after parent-load removal. Fix: inventory every helper spawn with test name and subject, replace non-boundary launches with resident APIs, batch exact candidate launches by subject, and keep only explicit process/PTY/CLI boundary tests. Acceptance: helper-spawn <=25, inner-hb + inner-hb-stdin <=8, boundary <=12 on Mac hot profile, with docs/gate.md updated if target changes.

Progress: macos-arm64-12x2 hot proof on 2026-06-30 reports helper-spawn=54,
inner-hb=11, inner-hb-stdin=5, and boundary=16. Keep open: all counters remain
above the target even though the suite body is now under the hot wall target.

2026-07-01 post-warm-launcher removal proof: macos-arm64-12x2 hot full suite
passed at 30016ms internal / 32.23s wall with helper-spawn=38, inner-hb=6,
inner-hb-stdin=4, boundary=10. Boundary is now below target, but helper-spawn
and inner-hb+inner-hb-stdin remain above acceptance; continue by inventorying
the 38 helper spawns and batching/removing non-boundary launches.

2026-07-01 resident setup proof: macos-arm64-12x2 hot direct suite passed at
26311ms internal / 28.66s wall with helper-spawn=30, inner-hb=1,
inner-hb-stdin=4, boundary=5. Helper-spawn remains above acceptance; keep this
dot open for true helper elimination.

2026-07-01 preserved-stats proof: direct hot suite with `HB_TMP` preserved passed
at 24471ms internal / 26.79s wall. `gate-stats.tsv` shows helper-spawn=30,
inner-hb=1, inner-hb-stdin=4, boundary=5. The named boundaries are now exactly
divide/modulo traps, profiler long dictionary names, repair diagnostic hints,
and unsafe-evaluate publication. Remaining helper work is inventorying generic
`bin/hb` helpers by owned test before removing or reclassifying only non-boundary
launches.

2026-07-01 check-all-errors argv smoke cleanup: `tools/check-all-errors-test.f`
no longer loads `tools/warm-run.f` or launches a child `bin/hb` for its CLI
smoke; it uses `ARGV-MOCK` plus `CHECK-ALL-ERRORS-FILE` and restores script argv
after the test. Focused proof: `cli-smoke` dropped from 1876ms to 3ms. Full
macos-arm64-12x2 persistent-cache suite passed at 24117ms internal / 26.48s
wall with helper-spawn=30, inner-hb=1, inner-hb-stdin=4, boundary=5,
warm-build=0, warm-sig=0, warm-snap=0. Helper-spawn remains above the <=25
target, so keep open.
