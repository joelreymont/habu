---
title: Compile while repeat and else
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T09:45:19.004865+02:00"
---

Survey finding: begin/while/repeat outnumbers begin/until 9-to-1 in real code (689 while vs 76 until) and the corpus only covers until - the biggest coverage inversion; else has 747 uses and zero corpus coverage. Extend the elaborator's control table: while (a mid-loop conditional exit to the loop's join - the skeleton pre-pass learns its block-creation rule, the two-derivations check extends), repeat (branch back to the header), else (the second arm - both arms hand the join their values as block args, the machinery exists). Exemplars from the survey: COUNT-CHAR (lib/string.f:103, while/repeat byte scan, needs NOTHING else), MAX-DIM (maki/tensor.f:76, the minimal else). Acceptance: both compile from source through the engine's reader and execute identically. Do after the comparison leaf (same files).
