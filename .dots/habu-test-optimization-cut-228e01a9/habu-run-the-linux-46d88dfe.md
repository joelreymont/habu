---
title: Measure the Linux process-tree scans
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T16:51:16.762665+02:00"
---

Problem: src/lib/process-tree.f CPU-LINUX (:619) scans all of /proc at least twice per reading, per budgeted gate slot, once a second: about ten full scans a second at gate start with five build rows (lane 369 cd2ec4d0, review 383). src/lib/process-tree-test.f and test/capture-tree-test.f run in the suite on spark. Acceptance: the scans' share of a suite run on spark is measured, and a reading that costs more than one scan is cut to one. Files: src/lib/process-tree.f. Depends: none. Ownership: that path. Claim: unassigned.
