---
title: "Linux: ws-test dies with an uncaught throw at master"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:39:16.938926+02:00"
---

Measured 2026-11-05 on spark (Linux arm64) with a linux-aarch64 engine cross-built on macOS from master 54a0304c (bin/hb --load tools/native-build.f -- <out>/hb --target linux-aarch64; tree and engine at spark:/tmp/carl-linux/base): bin/hb --load src/lib/net/ws-test.f exits 67 (UNCAUGHT-RC) after ~10 s with no output at all; macOS passes (rc 0, 16 s). Spark's installed Nov 1 engine cannot load master's aio.f, so no native Linux engine was compared. Problem: unknown; hunt first: reduce to the failing case, find whether the cross-built engine, the io_uring AIO host or ws.f is wrong. Acceptance: ws-test rc 0 on spark. Files: unknown until the hunt. Verify: ws-test on spark. Depends: none. Ownership: carl. Claim: unassigned.
