---
title: Remove manual chunk management
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-16T16:15:56.208110+02:00\""
---

Files: src/bytecode/emit.zig, src/interp/vm.zig
Delete any chunk registry, free(), or manual lifetime code.
Rely solely on GC for chunk lifetime.
Dependencies: habu-add-chunk-gc-ecb265fc
Verification: no manual chunk free, GC manages lifetime
