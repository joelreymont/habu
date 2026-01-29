---
title: Fix internal run time fallback
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:51.461364+01:00"
---

Context: src/runtime/primitives/io.zig:961-966; cause: on clock_gettime error returns wall time; fix: return error or use monotonic process cpu fallback; deps: habu-fix-fileexists-probefile-e17125d4; verification: add internal-run-time test, run zig build test --filter io
