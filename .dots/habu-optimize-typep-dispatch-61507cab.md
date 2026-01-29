---
title: Optimize typep dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:10.311755+01:00"
---

Context: src/runtime/primitives/type.zig:22-80; cause: repeated heap.intern calls and string-name dispatch; fix: use BuiltinSymbols/type table keyed by symbol identity; deps: habu-remove-repl-str-6305c9d5; verification: add typep performance/regression test, run zig build test --filter type
