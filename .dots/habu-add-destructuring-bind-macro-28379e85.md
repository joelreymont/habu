---
title: Add destructuring-bind macro
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:24:33.091247+02:00"
---

Missing from stdlib. Add to lib/stdlib.habu.
Pattern: (destructuring-bind (a b c) expr body...) -> nested let bindings
Use car/cadr/caddr to destructure. Support nested patterns.
Test: (destructuring-bind (a b c) (list 1 2 3) (+ a b c)) => 6
