---
title: eval-when not implemented - compile-time vs load-time evaluation unclear
status: closed
priority: 2
issue-type: task
created-at: "2026-01-01T10:34:23.622622+02:00"
closed-at: "2026-01-01T11:17:13.879847+02:00"
close-reason: "Fully implemented. Supports :compile-toplevel, :execute, :load-toplevel. REPL correctly evaluates :compile-toplevel forms at compile-time and returns progn for :execute forms to run at runtime. Works correctly for all three situations including combinations."
---

File: src/compiler/compile.zig (eval-when symbol exists), implementation missing

Issue: eval-when special form is declared in Builtins (line 129) but not implemented.

CL Spec:
eval-when controls when forms are evaluated:
  (eval-when (:compile-toplevel :load-toplevel :execute) forms...)
  
Situations:
- :compile-toplevel - when compiling file
- :load-toplevel - when loading compiled file  
- :execute - when evaluated (REPL)

Current: Symbol exists but no compileEvalWhen function

Impact:
- Cannot control macro expansion timing
- Cannot have compile-time side effects
- Blocks proper file compilation workflow

Fix:
1. Add compileEvalWhen in compile.zig
2. Check situation flags
3. Execute forms conditionally based on compile/load/eval context
4. Need to track compilation context (compiling file vs REPL)

Medium priority - mainly affects file compilation.

Files: src/compiler/compile.zig:129
