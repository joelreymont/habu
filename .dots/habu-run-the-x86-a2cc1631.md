---
title: Run the x86 kernel images to their status
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:02:43.663235+01:00"
---

Problem: the x86-64 harness suites build kernel images and check only their headers; run on pulsar, control, eval-closed, eval-range and skel exit 139 (skel jumps into zero padding at rip 0x4025c8), source-unit 22, floorrec 21, crash-region 24, against their headers' 76, 0, 76, 3, 0, 0, 0. No test runs them on x86-64. Acceptance: each image's cause is found and fixed, and a test runs each image on an x86-64 host and checks its status. Depends: Switch on the x86 boot-harness suites. Claim: unassigned.
