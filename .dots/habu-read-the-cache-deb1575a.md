---
title: Read the cache line size from CTR_EL0
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.926337+01:00"
---

Problem: src/habu/habu1.f EMIT-FLUSH cleans and invalidates with a fixed 64-byte stride, so a CPU with 32-byte lines (QEMU's default) keeps alternate lines stale; docs/bootstrap.md:47-49 works around it with -cpu neoverse-n1. Acceptance: the stride comes from CTR_EL0's DminLine/IminLine, checked on Linux and macOS arm64, and the bootstrap.md sentence is deleted. Depends: a macOS arm64 host to check an EL0 read of CTR_EL0. Claim: unassigned.
