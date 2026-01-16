---
title: "Add #P pathname reader macro"
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:52.542282+02:00"
---

src/reader/parser.zig: Implement pathname reader
- Depends on: dot (Pathname object, parse-namestring)
- #P"path/to/file": read pathname
- Expand to (parse-namestring "path/to/file")
- Add tests for pathname reading
- Est: 15 min
