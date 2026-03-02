---
title: Fix mforma blocker generically
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.170448+02:00"
blocks:
  - habu-close-generic-slot-37ee1f83
---

Problem: current mforma or DEFUN-MACLISP blocker still prevents clean load completion. Acceptance: the real failing form is proved, fixed in Habu generically, and locked with regression coverage. Files: ../maxima/src/mforma.lisp and the Habu subsystem proven by RCA. Verify: failing form repro before/after plus clean-load continuation. Blockers: habu-close-generic-slot-37ee1f83; also depends on habu-canonicalize-fn-and-46da8146, habu-make-long-loads-339fbed4, and habu-add-maxima-reader-192066c9.
