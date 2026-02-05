---
title: Add macro env boxed object
status: open
priority: 2
issue-type: task
created-at: "2026-02-05T21:43:27.839084+01:00"
blocks:
  - habu-fix-repl-macro-e4f601e0
---

Context: src/runtime/objects.zig:272-840, src/runtime/gc.zig, src/interp/repl.zig:1668-1765, src/compiler/compile.zig:2580-2760; cause: &environment currently nil/opaque; fix: add MacroEnv boxed object with macro/symbol-macro tables, allocate/pass when &environment present; deps: habu-fix-repl-macro-e4f601e0; verification: macro test asserts environment object is non-nil and pass-through stable.
