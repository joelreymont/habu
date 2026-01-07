---
title: Implement Habu native FASL format
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-05T15:28:58.785124+02:00"
closed-at: "2025-12-05T15:49:36.310423+02:00"
close-reason: ""
---

Design and implement native FASL (Fast Load) format for Habu.

FASL contains:
- Header (magic bytes, version, target arch)
- Function table (name, arity, code offset, code size)
- ARM64 machine code section
- Constant pool (strings, numbers, quoted data)
- Relocation table (external symbol references)
- Metadata (source locations, documentation)

Binary format should be:
- Position-independent where possible
- Quick to load and relocate
- Compact but not aggressively compressed

This is foundational for separate compilation and practical development.
