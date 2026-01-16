---
title: Add maphash primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:25.540498+02:00"
---

src/runtime/primitives/hash.zig: Implement maphash function
- maphash: call function on each key-value pair
- Iterate over all buckets and entries
- Call function with (key, value) for each entry
- Return nil
- Add test verifying all pairs visited
- Est: 15 min
