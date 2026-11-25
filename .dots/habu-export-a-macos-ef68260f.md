---
title: Export a macOS engine without a crash
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:10:26.238750+02:00"
---

Problem: bin/hb --load tools/hb-build.f -- --engine --target macos-aarch64 -o OUT exits 139 (SIGSEGV) on Linux aarch64, seen 2026-11-25; not yet traced, nor run on master. Acceptance: the command exits 0 and writes an engine that the macOS target's tests accept. Depends: none. Claim: unassigned.
