---
title: Read the definer sites through SITES
status: closed
priority: 2
issue-type: task
created-at: "2026-10-02T12:33:46.972037+03:00"
closed-at: "2026-10-02T12:58:10.000000+03:00"
close-reason: "Readers walk SITES:EACH-IN-SPAN with a per-target literal arm; spark before/after: stripped-image, stripped-address (+x86 MOVABS case), hb-build-stripped(-cells) and 6 linker rows green with equal logs, the stripped-image subject links byte-identical (97ce063d), rebuilt engine = base 31d3fab3 (not baked)."
---

Problem: `aot-closure.f` `REC-CELL-SITE?`/`REC-CELL-BELOW`/`DATA-CELL-OWNER` (683-723, and the same bitmap walk at ~1090, ~1189) and `aot-lib.f:987` walk four-byte words with the ARM64 reader `ADDRESS-SITE?`, so they cannot read x86 definer sites. Split from habu-compile-definer-bodies-1292d049 (I7) by the I7 design (lead correction on I7).
Acceptance: those readers walk `SITES:EACH-IN-SPAN` with `SNAP-RELOC:SITE-ADDR` and decode per target (`ADDRESS-CARRIER:CHAIN-SIZE`/`CHAIN-VALUE` on ARM64, `MOVABS-SITE?`/`MOVABSV` on x86); the same owners and refusals as today.
Files: `src/habu/aot-closure.f`, `src/habu/aot-lib.f`.
Verify: spark: `stripped-image`, `stripped-address`, `hb-build-stripped-cells`, `hb-build-stripped` rows; rebuild if baked; gate.
Depends: none. Parallel with I9a; off the critical path.
Route: Alder (shared: src/habu/aot-closure.f, src/habu/aot-lib.f).
Ownership: krait (Intel lane).
Claim: unassigned.
