---
title: Depth-guard dies need child-process fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T14:03:32.408185+02:00"
---

Flagged by single-prefix-4, never dotted (my miss): DEV-SNAPSHOT-RESET's depth guard has no negative fixture - the same gap its two siblings RBF-SNAP-RESET and TDECL-SCRATCH-SNAPSHOT-RESET carry. All three die, so proving them needs a child-process fixture none has. One fixture shape serves all three (open the guarded state, invoke the reset in a child, pin the die sentence + rc); mutation = guard dropped, silent corruption instead.
