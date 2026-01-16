---
title: Add hash table query primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:24.934949+02:00"
---

src/runtime/primitives/hash.zig: Implement hash table queries
- hash-table-count: number of entries
- hash-table-rehash-size: growth increment (stub: return 1.5)
- hash-table-rehash-threshold: load factor (stub: return 0.75)
- hash-table-size: current capacity
- hash-table-test: test function (eq/eql/equal)
- hash-table-p: predicate
- Add tests for all query functions
- Est: 20 min
