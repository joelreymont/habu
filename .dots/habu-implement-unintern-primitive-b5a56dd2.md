---
title: Implement unintern primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:04.168679+02:00"
---

src/runtime/primitives/package.zig: Add unintern_symbol(symbol, package). Remove from symbol table. Dependencies: habu-implement-unexport-primitive-c2ccee8c. Verify: (unintern 'foo).
