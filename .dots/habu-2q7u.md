---
title: Fix symbol interning for Stage 1 self-compilation
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-06T09:17:55.504329+02:00"
closed-at: "2025-12-06T19:36:31.238752+02:00"
close-reason: ""
---

Stage 1 reader, compiler work. Codegen fails because symbols read at runtime (e.g. :x0) aren't eq to symbols compiled into the binary. The native reader creates new symbol objects that don't match compiled-in keywords. Need consistent symbol interning between compile-time and runtime.
