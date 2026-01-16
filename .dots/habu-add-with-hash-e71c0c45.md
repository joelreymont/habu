---
title: Add with-hash-table-iterator macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:26.146169+02:00"
---

lib/stdlib.habu: Implement hash table iterator macro
- Depends on: dot (maphash primitive)
- with-hash-table-iterator: bind iterator function
- Expand to closure over maphash
- Iterator returns (has-entry-p key value)
- Add test iterating over table
- Est: 20 min
