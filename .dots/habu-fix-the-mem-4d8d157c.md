---
title: "Fix the memory test's RBT-RELEASE-FIRST crash on Linux arm64"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T13:20:19.977445+02:00"
---

Problem: on Linux arm64 (spark) the memory suite's RBT-RELEASE-FIRST (src/lib/memory-test.f:287, run as a child program `package MEMT RBT-RELEASE-FIRST ;package`, :346) dies with SIGSEGV (habu-crash regs, sig 0xb), then the suite reports a timeout, -2502. Red at master bf05b860 and at the aiocancel lane alike (spark:/tmp/carl-linux/cmp-base2.log :27-37, cmp-aiocancel.log). Passes on macOS. Acceptance: a hunt reduces it to the declaration, checker, compiler or runtime layer that is wrong; the fix there; the memory suite passes on Linux arm64. Files: src/lib/memory.f, src/lib/memory-test.f (hunt decides). Verify: on spark, a Linux engine cross-built from master (`bin/hb --load tools/native-build.f -- <out>/hb --target linux-aarch64`), then `bin/hb --load src/lib/memory-test.f`. Depends: none. Ownership: carl. Claim: unassigned.
