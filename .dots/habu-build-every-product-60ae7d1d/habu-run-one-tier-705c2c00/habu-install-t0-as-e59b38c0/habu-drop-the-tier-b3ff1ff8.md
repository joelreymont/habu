---
title: "Drop the tier-1 toggle from engine-suite's ES-CHAR row"
status: closed
priority: 3
issue-type: task
created-at: "2026-10-29T04:30:09.958572+02:00"
closed-at: "2026-11-04T11:48:49.013399+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. engine-suite.f:587-589 tier-1 toggle exists for the JIT's char lookup; goes with the JIT (verified in tree)."
---

test/engine-suite.f package ES-CHAR-N selects tier 1 around ES-CHAR-LIT because master's JIT compiles a body char by dictionary lookup. Once T0 is the arm64 default compiler (A6) and the JIT is gone (L8), remove the toggle so the row runs at the default tier. Workspace: none yet.
