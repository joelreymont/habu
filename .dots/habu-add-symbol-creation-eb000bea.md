---
title: Add symbol creation primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:29.178161+02:00"
---

src/runtime/primitives/symbol.zig: Implement symbol creation
- make-symbol: create uninterned symbol
- copy-symbol: copy symbol (optionally copy plist/value/function)
- gentemp: generate temporary interned symbol (unique name)
- Update gensym to support optional prefix argument
- Add tests for symbol uniqueness
- Est: 25 min
