---
title: Measure the Linux process-tree scans
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T16:51:16.762665+02:00"
---

Problem: src/lib/process-tree.f CPU-LINUX (:616) scans all of /proc at least twice per reading, per budgeted gate slot, once a second: about ten full scans a second at gate start with five build rows (lane 369 cd2ec4d0, review 383). src/lib/process-tree-test.f, test/gate-pool-test.f's CPU-budget cases and test/capture-tree-test.f run in the suite on spark. Acceptance: the scans' share of a suite run on spark is measured, and a reading that costs more than one scan is cut to one. Files: src/lib/process-tree.f. Depends: none. Ownership: that path. Claim: unassigned.

On master efef8267's x86-64 engine (joel@pulsar-ua-106, full suite), the rows gate-signal, check-signal and capture-tree hung with no CPU for 26 minutes: each row's process (test/gate-signal-test.f, test/check-signal-test.f, test/capture-tree-test.f) sat in open() of a FIFO (wchan wait_for_partner, fds 0-2 only), beside a pool child running `bin/hb --load test/run.f` in poll with one pipe open. The pool keeps no clock on a row, so the suite waited until the lead sent the three rows SIGTERM.
