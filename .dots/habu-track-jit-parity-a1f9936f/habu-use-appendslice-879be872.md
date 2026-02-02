---
title: Use appendSlice
status: open
priority: 2
issue-type: task
created-at: "2026-02-02T22:28:30.107793+01:00"
blocks:
  - habu-remove-backup-files-8399dce0
---

Context: src/compiler/passes/ir_types.zig:93-147; cause: repeated append for known items; fix: precompute children + appendSlice; deps: habu-remove-backup-files-8399dce0; verification: zig build test
