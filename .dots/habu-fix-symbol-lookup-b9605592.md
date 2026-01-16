---
title: Fix symbol lookup string compare
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:07:39.839831+02:00"
---

src/runtime/primitives/package.zig:78 - Uses string compares + full table scan for symbol lookup, O(n) + rule violation. Compare interned symbols, use hashed lookup. Medium severity.
