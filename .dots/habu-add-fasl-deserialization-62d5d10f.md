---
title: Add FASL deserialization
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:56.483417+02:00"
---

src/compiler/fasl.zig: Implement FASL reader
- Depends on: dot (FASL serialization)
- Read binary format sections
- Reconstruct objects from serialized form
- Intern symbols in appropriate packages
- Add tests for loading FASL files
- Est: 30 min
