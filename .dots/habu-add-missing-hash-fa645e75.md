---
title: Add missing hash table primitives
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-18T06:24:37.659745+02:00\""
---

Files: src/runtime/primitives/hash.zig
hash-table-rehash-size, hash-table-rehash-threshold, hash-table-size are marked ✓ in stdlib.habu.
Check if they're actual implementations or stubs returning dummy values.
Add real implementations if needed.
Verify: zig build test passes.
Est: 30min
