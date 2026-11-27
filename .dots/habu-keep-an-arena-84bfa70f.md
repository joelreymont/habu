---
title: "Keep an arena's MARK exact after a shrink"
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T21:04:10.872212+01:00"
---

Problem: src/lib/arena.f SELF-RESIZE shrinks the newest block in place by lowering the top, also below a MARK taken before; a chunk mapped afterwards records a start below that mark, so CUT keeps it and zero-fills from inside its header (new arena, 64-byte block, MARK 160, shrink by 16, ALLOC 20000, CUT: MARK answers 16), and an ALLOC-DISPOSE made after the shrink is skipped by CUT. MARK and CUT are public. Acceptance: CUT restores the arena to its MARK after any shrink; arena-test.f covers it and a child arena made before and after a mark. Depends: none.
