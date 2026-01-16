---
title: Add FASL serialization format
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:56.178475+02:00"
---

src/compiler/fasl.zig: Implement FASL writer
- Depends on: dot (compile-file design)
- Design binary format: magic, version, sections
- Serialize bytecode, constants, symbols, packages
- Handle circular references in constant pool
- Add tests for roundtrip serialization
- Est: 30 min
