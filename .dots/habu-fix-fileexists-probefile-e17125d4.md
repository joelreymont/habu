---
title: Fix fileExists/probeFile errors
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:43.712441+01:00"
---

Context: src/runtime/primitives/io.zig:902-931; cause: swallows IO errors as false; fix: return error or distinguish missing vs error; deps: none; verification: add IO error test, run zig build test --filter io
