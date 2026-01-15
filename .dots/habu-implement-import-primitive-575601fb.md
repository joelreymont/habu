---
title: Implement import primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:36.076426+02:00"
---

src/runtime/primitives/package.zig: Add import_symbols(symbols, package). Add external symbols to package's internal table. Dependencies: habu-implement-export-primitive-6d9056b8. Verify: (import 'foo) imports foo.
