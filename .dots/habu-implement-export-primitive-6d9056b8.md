---
title: Implement export primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:30.611558+02:00"
---

src/runtime/primitives/package.zig: Add export_symbols(symbols, package). Add symbols to package export-list. Handle conflicts. Dependencies: habu-implement-find-symbol-a4ec7567. Verify: (export 'foo) makes foo external.
