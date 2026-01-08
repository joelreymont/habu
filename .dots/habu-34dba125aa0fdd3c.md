---
title: Fix stdlib type annotation compilation
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T09:14:52.614731+02:00"
---

File: lib/stdlib.habu - Many functions have type annotations like (defun reduce ((f closure) (lst list)) ...) which cause CompileError when stdlib is loaded. Functions without type annotations work fine. This breaks core functions like reduce, foldl, every, filter, etc. Need to investigate why typed parameters fail to compile during stdlib load but work in user code.
