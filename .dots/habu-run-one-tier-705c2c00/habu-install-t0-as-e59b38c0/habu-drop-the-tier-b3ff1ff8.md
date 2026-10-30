---
title: "Drop the tier-1 toggle from engine-suite's ES-CHAR row"
status: open
priority: 3
issue-type: task
created-at: "2026-10-29T04:30:09.958572+02:00"
---

test/engine-suite.f package ES-CHAR-N selects tier 1 around ES-CHAR-LIT because master's JIT compiles a body char by dictionary lookup. Once T0 is the arm64 default compiler (A6) and the JIT is gone (L8), remove the toggle so the row runs at the default tier. Workspace: none yet.
