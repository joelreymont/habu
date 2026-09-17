---
title: "Read the forge harness's pty to a marker under a deadline"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T13:24:25.046247+03:00\""
---

Problem: test/aot-data-span-forge.f says it mirrors test/proc-pty.f and still carries the defect the pty lane removed from proc-pty.f (a2789b38): 500 constant PTY-EXPECT-MAX-POLLS bounds a wait by a count of reads rather than by the clock, a truncating '4096 RN @ - read' loses bytes when the buffer fills, and its MFD-READ-READY? shape has no hang-up signal, so a dead child burns the whole budget. Acceptance: the same fix as proc-pty.f: waits end on the marker, the hang-up or an absolute monotonic deadline through lib/process.f PROC-DEADLINE-AT / PROC-LEFT-MS, a full buffer keeps its tail, a hang-up ends the wait at once; ten consecutive green runs with the load stated. Files: test/aot-data-span-forge.f. Verify: the suite that registers it; ten runs. Depends: none. Ownership: process/pty tests. Claim: agent=hazel-forge-pty workspace=.jj-ws/hazel-forge-pty.
