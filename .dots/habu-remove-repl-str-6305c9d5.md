---
title: Remove REPL string fallback dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:05:06.618786+01:00"
---

Context: src/interp/repl.zig:1271-1480; cause: string fallback for defmacro/defpackage/eval-when keywords; fix: ensure builtins always initialized and use identity-only comparisons; deps: habu-replace-desugar-str-924d8a5c; verification: add eval-when/defmacro REPL tests, run zig build test --filter repl
