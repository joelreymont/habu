---
title: Fix 4 failing VM tests
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T09:13:52.902009+02:00"
---

src/interp/vm.zig: hash table (2 tests), loop tests (2 tests). Stack underflow in hash table tests. Type mismatch in loop tests. Tests at lines 6190-6280. Est: 30min
