---
title: Update symbol interning for packages
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:15.601335+02:00"
---

src/runtime/heap.zig: Modify intern() to accept package parameter. Store symbols in package-specific table, not global. Update all intern() call sites. Dependencies: habu-implement-list-all-4d3a8a03. Verify: symbols scoped to packages.
