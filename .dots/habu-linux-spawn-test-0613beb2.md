---
title: "Linux: spawn-test fails one assert at master"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:39:16.951341+02:00"
---

Measured 2026-11-05 on spark (Linux arm64) with the linux-aarch64 engine cross-built from master 54a0304c (spark:/tmp/carl-linux/base): bin/hb --load src/lib/spawn-test.f rc 1, log 'F2 / TFAIL assert 2 / assert: expected true got false / test: 1 failure(s)'; macOS passes. Problem: unknown; hunt first: name the failing case and whether the cross-built engine, src/os/linux or spawn.f is wrong. Acceptance: spawn-test rc 0 on spark. Files: unknown until the hunt. Verify: spawn-test on spark. Depends: none. Ownership: carl. Claim: unassigned.
