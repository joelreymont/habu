---
title: "Add equalp + hash :test"
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T12:11:13.894504+01:00"
---

docs/cl-symbols.md:1078 marks equalp missing; src/runtime/objects.zig:313 HashTest lacks equalp; src/compiler/compile.zig:11941 compileMakeHash rejects equalp; src/interp/vm.zig: hashTableGet/Set + hashKeyEqualWithTest lack equalp. Root cause: equalp never implemented, but referenced by docs and primitives/hash.zig. Fix: add HashTest.equalp, extend IR/opcode/test parsing, implement valueEqualp + hashValueEqualp (case-insensitive strings/chars, numeric coercions, deep arrays/vectors), update hash table ops + make-hash-table parsing, add tests for (make-hash-table :test 'equalp) and nested structures.
