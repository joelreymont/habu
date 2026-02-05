---
title: Add copy-structure
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"\\\\\\\"2026-02-05T12:16:17.067378+01:00\\\\\\\"\\\"\""
closed-at: "2026-02-05T17:05:00.305295+01:00"
close-reason: Implemented copy-structure (IR/op/vm/jit/tests); zig build test
---

docs/cl-symbols.md:1071 marks copy-structure missing. Root cause: defstruct copier generation uses copy-seq but CL requires COPY-STRUCTURE for structure objects. Fix: implement copy-structure primitive for structure vectors (retain type tag slot0, shallow copy rest) in src/runtime/primitives/vector.zig or clos.zig; add tests for defstruct instances.
