---
title: Fix LOOP macro returning 0 instead of executing body
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T09:27:10.042367+02:00"
---

src/tests/loop_tests.zig: LOOP test expects 15 but gets 0. LOOP compiles and runs but body doesn't execute. Test uses builtin LOOP from compiler. Check bytecode generation or IR compilation.
