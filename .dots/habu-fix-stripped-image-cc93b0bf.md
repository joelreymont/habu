---
title: "Fix stripped-image's MEM:UNMAP exit on Linux arm64"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T13:20:19.992599+02:00"
---

Problem: on Linux arm64 (spark) test/stripped-image.f:92 fails "FAIL: stripped image exit through MEM:UNMAP": the stripped image exits rc 74 (unmapped) where the test expects 71. Red at master bf05b860 and at the aiocancel lane alike (spark:/tmp/carl-linux/cmp-base2.log :100-106). Passes on macOS. Stripped images are built by tier 1 (tools/aot-build.f), a path tier 1 still owns (DECISIONS "tier 1 bugs before Wasm moves to T0"). Acceptance: the cause located and fixed at its layer; stripped-image passes on Linux arm64. Files: hunt decides. Verify: on spark with a Linux engine cross-built from master, the stripped-image suite. Depends: none. Ownership: carl. Claim: unassigned.
