---
title: Rematerialize the loop-invariant loads
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T17:03:15.160104+02:00"
---

PRESSURE-LOOP road, from the spill lane's measurement: its 14 live values are pure loop-invariant loads off one base, so rematerialising them in place needs no frame slot and no new memory order. Probe this road FIRST (the lane's probe shape at /private/tmp/claude-501/spill-probe-final.f) before any frame work; if remat closes the row, the deep redesign (habu-spill-from-a-4145325c) stays unneeded for the corpus. Purity must come from the IR's own facts, not assumption. Acceptance: PRESSURE-LOOP compiles, answers bit-for-bit, validator extended to check remat correctness (a remat'd value equals the load it replaces — differential test), no other row moves, both-gaps reported, deliberate re-pin.

Consolidation (2026-08-05): shares one remat design and lane with habu-rematerialize-constants-cdce9a24 — constants land first, these loads second on the same machinery.

SHARED-MACHINERY NOTE (2026-08-05, remat). The consolidation block above is
still right that one remat design serves both leaves, but the constants leaf's
premise was corrected before any code was written and this leaf inherits the
correction: see habu-rematerialize-constants-cdce9a24. In short, the allocator
runs on the MACHINE module, so there is no operand-free constant there - a
literal is a tied movz/movk chain that forms one register class - and re-emission
is only cheaper than a spill when the chain is one instruction. The shared
machinery is therefore not "re-emit the defining op" but "re-emit the class's
defining CHAIN, when that costs no more than the reload it replaces", plus the
CL-SLOT split described in that dot (a class can be evicted without holding a
slot, which five sites in regalloc.f currently cannot express).

WHAT THAT MEANS FOR THE LOADS. A pure loop-invariant load is a single
instruction, so on the cost test it is a candidate wherever a constant chain of
length one is. The purity question is untouched and remains this leaf's own: the
IR has to say the load is pure and that its base and memory order are unchanged
across the re-emission site, from its own facts. If it cannot, this leaf stays
refused with the missing fact named, per its acceptance. That question was NOT
investigated in this lane.

NOT BUILT. Nothing in src/ or test/ changed for this leaf.

Claim: agent=remat workspace=.jj-ws/habu-fold-constants-and-cbe4e25e
