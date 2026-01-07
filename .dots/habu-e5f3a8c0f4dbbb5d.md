---
title: Fix defclass - constructor in wrong namespace
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-07T22:41:51.825221+02:00\""
---

File: src/compiler/compile.zig:4285, 4633, 4687 - generateStructConstructor/Predicate/Accessor/Copier use builder.define which sets VALUE cell, not FUNCTION cell. Need to use setf+symbol-function or create builder.defun. Test: (defclass person () (name age)) then (fboundp 'make-person) returns nil but make-person evaluates to closure. Root cause: define creates variable binding, not function binding. Fix: Change generators to set function cell like defun does.
