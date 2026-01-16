---
title: Add apropos primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:30.084424+02:00"
---

src/runtime/primitives/symbol.zig: Implement symbol search
- apropos: print symbols matching string (interactively)
- apropos-list: return list of matching symbols
- Search all packages for symbol names containing substring
- Support optional package argument
- Add tests for pattern matching
- Est: 25 min
