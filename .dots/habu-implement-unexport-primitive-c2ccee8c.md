---
title: Implement unexport primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:59.637140+02:00"
---

src/runtime/primitives/package.zig: Add unexport_symbols(symbols, package). Remove from export-list. Dependencies: habu-implement-unuse-pkg-6dbb75dc. Verify: (unexport 'foo).
