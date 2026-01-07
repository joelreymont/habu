---
title: Fix defclass constructor - make-class-name fails at runtime
status: active
priority: 1
issue-type: task
created-at: "\"2026-01-07T22:21:06.946875+02:00\""
---

Location: src/compiler/compile.zig:4840-4843 (generateStructConstructor). Issue: Calling make-person or using make-instance causes RuntimeError, even though fboundp shows function exists. Constructor is defined but something is wrong with its implementation. Need to debug why the generated lambda fails at runtime.
