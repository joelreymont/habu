---
title: Add sxhash primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:25.844106+02:00"
---

src/runtime/primitives/hash.zig: Implement sxhash function
- sxhash: compute hash code for any object
- Use existing hash implementation
- Return non-negative fixnum
- Same object always returns same hash (within session)
- Add tests for various object types
- Est: 15 min
