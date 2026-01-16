---
title: Add LOOP for/being/hash-keys clauses
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:49.508506+02:00"
---

lib/stdlib.habu: Implement hash table iteration
- Depends on: dot (LOOP design, with-hash-table-iterator)
- for key being hash-keys of table: iterate keys
- for value being hash-values of table: iterate values
- for key being hash-keys of table using (hash-value v): parallel iteration
- Use with-hash-table-iterator internally
- Add tests for hash table iteration
- Est: 30 min
