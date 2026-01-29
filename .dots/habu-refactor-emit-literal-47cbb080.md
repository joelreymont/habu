---
title: Refactor emit literal type dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:56.113098+01:00"
---

Context: src/bytecode/emit.zig:1186-1206; cause: if/else type predicate chain; fix: switch on Value.typeKind() with exhaustive cases; deps: none; verification: run zig build test --filter emit
