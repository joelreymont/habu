---
title: Add function-lambda-expression
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"\\\\\\\"2026-02-05T12:16:12.432059+01:00\\\\\\\"\\\"\""
closed-at: "2026-02-05T19:07:17.197077+01:00"
close-reason: Add function-lambda-expression
---

docs/cl-symbols.md:1084 marks function-lambda-expression missing. Root cause: closures/chunks lack introspection API. Fix: implement (function-lambda-expression fn) in src/runtime/primitives/clos.zig or new primitives/function.zig returning (values lambda-form closure-p name) per CLHS; plumb original lambda form into Closure/Chunk metadata; add tests.
