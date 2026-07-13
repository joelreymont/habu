---
title: "Core bootstrap: resize source prefix arena"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T18:40:14.505036+02:00\""
---

Checker registry migration exhausts the composite IBUFSZ 0x180000 arena:
the measured stage2 source is 1,036,134 bytes and the cold-prefix sources are
544,174 bytes, totaling 1,580,308 bytes, 7,444 bytes above the 1,572,864-byte
cap. `install --force` emits `bin/hb refresh OK`, then the later stage exits 74
with `hb: source prefix buffer full`; the binary replacement is therefore not
a proven install. Enlarge the capacity owner in src/habu/layout.f and its
bootstrap/cg/forth.fs mirror with explicit measured headroom, keep mirrors
equal, derive the oversize regression input from IBUFSZ instead of the stale
0x1A0000 literal, add a near-capacity positive and cap-plus-one labeled-negative
regression, and prove bootstrap/fixpoint byte parity. Do not shrink record
assertions or bypass the fail-closed buffer. Apply the same measured rule to
the stage2 and maker source arenas: the 1,036,134-byte stage2 source has only
12,442 bytes below S2-SOURCE-CAP/MK-SOURCE-CAP 0x100000, so src/habu/stage2.f,
src/habu/maker.f, and the stale 0x100000 read ceiling in
tools/build-fixpoint-test.f are in scope. Require at least 25 percent headroom
over each measured composite, round to the next power of two, and keep one
named policy across native, recovery, stage2, maker, and tests. Discover the
effective successful and first-failing runtime source sizes with bounded
exponential plus binary probes; do not encode IBUFSZ+1 because the EOF probe
makes the effective boundary lower. Add sole-token owner/mirror parity and
document the three distinct arenas in docs/debugging.md. Gforth recovery and
native fixpoint parity are required; do not claim cross-host DDC parity while
habu-ddc-cross-check-16562dae remains open.

Claim: agent=capacity_impl workspace=.jj-ws/type-dsl-capacity
