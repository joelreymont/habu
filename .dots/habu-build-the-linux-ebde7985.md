---
title: Build the linux-aarch64 engine in the test suite
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:53:58.634760+02:00"
---

Problem: no test cross-builds the engine for linux-aarch64 (only test/compiler/target-resolve.f names the target), so task.f:1821 broke that build unseen. Acceptance: a test builds the linux-aarch64 engine from macOS and fails with task.f:1821 reverted. Depends: none. Claim: unassigned.
