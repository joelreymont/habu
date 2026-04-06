---
title: Fix pregexp matcher throw
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-06T08:17:02.683760+02:00\""
closed-at: "2026-04-06T08:26:20.015444+02:00"
close-reason: "done: nested labels now inherit outer boxed fn bindings; tiny pregexp repro returns ((0 . 3))"
---

Problem: `pregexp:pregexp-match-positions` throws `UnhandledThrow` / `TYPE-ERROR nil` under Habu even on simple inputs like `(\"abc\", \"abc\")`. This is the proven blocker underneath `MAXIMA::INITIALIZE-RUNTIME-GLOBALS` browser setup in `../maxima/src/init-cl.lisp`. Need reduce the matcher control-flow shape and fix the underlying Habu compiler/VM bug generically. Files: `../maxima/src/pregexp.lisp`, `src/compiler/compile.zig`, `src/interp/vm.zig`. Acceptance: loading `pregexp.lisp` then `(pregexp:pregexp-match-positions \"abc\" \"abc\")` returns a match structure instead of throwing; `initialize-runtime-globals` then advances past browser setup.
