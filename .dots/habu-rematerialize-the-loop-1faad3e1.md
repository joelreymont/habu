---
title: Rematerialize the loop-invariant loads
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:03:15.160104+02:00"
---

PRESSURE-LOOP road, from the spill lane's measurement: its 14 live values are pure loop-invariant loads off one base, so rematerialising them in place needs no frame slot and no new memory order. Probe this road FIRST (the lane's probe shape at /private/tmp/claude-501/spill-probe-final.f) before any frame work; if remat closes the row, the deep redesign (habu-spill-from-a-4145325c) stays unneeded for the corpus. Purity must come from the IR's own facts, not assumption. Acceptance: PRESSURE-LOOP compiles, answers bit-for-bit, validator extended to check remat correctness (a remat'd value equals the load it replaces — differential test), no other row moves, both-gaps reported, deliberate re-pin.

Consolidation (2026-08-05): shares one remat design and lane with habu-rematerialize-constants-cdce9a24 — constants land first, these loads second on the same machinery.
