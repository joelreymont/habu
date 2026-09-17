---
title: Give REJECT in the pty harness a positive barrier
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T13:24:25.048720+03:00"
---

Problem: REJECT in test/proc-pty.f asserts the absence of a marker over whatever happened to have been read, with nothing guaranteeing the falsifying output had arrived: a REJECT after a drain that returned on its first quiet poll proves nothing (pty lane, 2026-09-17). Acceptance: every absence claim follows a positive barrier, a marker the child emits after the point where the rejected text could have appeared, so the REJECT reads a buffer that provably contains the child's answer; a fixture shows a REJECT that used to pass vacuously now waits for the barrier. Files: test/proc-pty.f. Verify: the fixture; test/runtime-regression-test.f. Depends: none. Ownership: process/pty tests. Claim: unassigned.
