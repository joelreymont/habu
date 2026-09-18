---
title: Keep sort cursors local to each invocation
status: closed
priority: 2
issue-type: task
created-at: "2026-09-18T19:52:45.436946+03:00"
closed-at: "2026-09-18T21:19:14.068051+03:00"
close-reason: implemented by alder (aff2caa8 Keep heapsort cursors local across comparator calls), reviewed, chained CC (gen2==gen3, full suite 449/449 on fCC3), integrated at fba695f8
---

Problem: lib/sort.f:42-59 keeps HS-NODE and HS-I in shared globals while invoking an arbitrary comparator. A comparator that sorts a separate two-cell array overwrites the outer cursor. Reproduced: outer [5,2,9,1,7], inner [2,1], comparator sorts inner with < then compares its own operands with <; result is [9,2,5,1,7]. This is the still-open comparator residue from archived habu-lib-minor-defects-10a9e6d5, which was superseded by Campaign C5, now isolated as one actionable child. Acceptance: carry cursor state per invocation, preserving it across comparator calls; regression nests sorting of a separate array inside a comparator and asserts the complete sorted outer and inner arrays. Files: lib/sort.f and lib/sort-test.f. Verify: focused sort suite through bin/hb at both tiers. Ownership: sort library. Claim: unassigned.
