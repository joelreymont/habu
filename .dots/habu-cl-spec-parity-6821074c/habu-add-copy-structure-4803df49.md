---
title: Add copy-structure
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-05T12:16:17.067378+01:00\\\"\""
closed-at: "2026-02-05T17:00:39.526162+01:00"
close-reason: Implemented COPY-STRUCTURE end-to-end
---

docs/cl-symbols.md:1071 marks copy-structure missing. Root cause: defstruct copier generation uses copy-seq but CL requires COPY-STRUCTURE for structure objects. Fix: implement copy-structure primitive for structure vectors (retain type tag slot0, shallow copy rest) in src/runtime/primitives/vector.zig or clos.zig; add tests for defstruct instances.
