---
title: Fix pathname version parsing
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:39.824514+01:00"
---

Context: src/runtime/primitives/pathname.zig:225-234; cause: parseInt failure coerced to 0; fix: treat invalid version as error or :unspecific; deps: none; verification: add pathname version parse test, run zig build test --filter pathname
