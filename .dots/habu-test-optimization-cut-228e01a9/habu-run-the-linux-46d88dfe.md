---
title: Run the Linux proc-maps path on a Linux kernel
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T16:51:16.762665+02:00"
---

Problem: src/habu/proc-maps.f has a Linux path that was read, never run, when 3295ca0c changed process-map handling at capture; test/proc-maps.f ran on macOS only. Acceptance: test/proc-maps.f and the capture rows that use the process map run green on a Linux kernel, and so do test/gate-signal-test.f, test/gate-pool-test.f and test/gate-pool-orphan-test.f (lib/process-tree.f has a /proc arm that was read, never run), or the defect they show is fixed. Files: src/habu/proc-maps.f, test/proc-maps.f, lib/process-tree.f. Verify: those rows on Linux. Depends: a Linux host. Blocked 2026-09-30: no Docker daemon, no VM tool, and the ssh host zed does not resolve from this machine because Tailscale is logged out here; `tailscale up` needs the user. Ownership: that path. Claim: unassigned.

Also on the Linux host (lane 369 cd2ec4d0, review 383): lib/process-tree.f CPU-LINUX scans all of /proc at least twice per reading, per budgeted gate slot, once a second (about ten full scans a second at gate start with five build rows): measure it and run lib/process-tree-test.f, test/gate-pool-test.f's CPU-budget cases and test/capture-tree-test.f there.
