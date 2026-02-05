---
title: Fix REPL macro lambda keywords
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-05T21:43:23.750843+01:00\""
closed-at: "2026-02-05T22:07:17.464352+01:00"
close-reason: Already implemented in prior REPL macro keyword commit
---

Context: src/interp/repl.zig:1277-1765, src/compiler/compile.zig:5703-5785; cause: REPL defmacro closures ignore &whole/&environment; fix: parse markers in handleDefmacro, store flags, update callMacro to prepend whole form and environment object; deps: none; verification: REPL test defmacro with (&whole w &environment e) sees non-nil bindings.
