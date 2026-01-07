---
title: destructuring-bind macro
status: closed
priority: 1
issue-type: task
created-at: "2025-12-29T16:04:50.481351+02:00"
closed-at: "2025-12-29T17:52:19.145245+02:00"
close-reason: "Completed: destructuring-bind in stdlib.habu:472"
---

Implement destructuring-bind for pattern matching in bindings.
Location: stdlib.habu (new macro)
Syntax: (destructuring-bind (a b &rest c) expr body...)
Needs: recursive tree-walking to generate let bindings
Example: (destructuring-bind (x (y z)) '(1 (2 3)) (+ x y z)) => 6
Required by: loop macro, defmacro improvements, many CL macros
