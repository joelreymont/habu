---
title: Add clrhash primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:25.236262+02:00"
---

src/runtime/primitives/hash.zig: Implement clrhash function
- clrhash: remove all entries from hash table
- Reset count to 0
- Clear all buckets
- Return the hash table
- Add test verifying table emptied
- Est: 10 min
