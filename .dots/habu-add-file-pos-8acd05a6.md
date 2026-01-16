---
title: Add file-position primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:28.570027+02:00"
---

src/runtime/primitives/io.zig: Implement file-position
- file-position: get/set stream position
- 0-arg: return current position
- 1-arg: seek to position (or :start/:end)
- Return position or success boolean
- Add tests for seeking behavior
- Est: 20 min
