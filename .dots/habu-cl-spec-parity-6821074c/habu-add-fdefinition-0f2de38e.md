---
title: Add fdefinition
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T12:15:47.494904+01:00"
---

docs/cl-symbols.md:1079 marks fdefinition missing; lib/stdlib.habu:1063 setf macro expands (setf (fdefinition sym) ...) but fdefinition fn absent. Root cause: no accessor for function binding. Fix: implement fdefinition in lib/stdlib.habu (and/or vm opcode) to mirror symbol-function semantics + correct errors; add tests for (fdefinition 'name) and (setf (fdefinition 'name) ...) via %set-symbol-function.
