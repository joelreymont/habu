---
title: Add allow-other-keys metadata
status: open
priority: 2
issue-type: task
created-at: "2026-02-05T21:43:14.701299+01:00"
---

Context: src/compiler/compile.zig:2882-3165, src/compiler/ir.zig:98,1056, src/bytecode/emit.zig:1327-1375, src/runtime/objects.zig:772; cause: &allow-other-keys not parsed/stored; fix: parse in compileLambdaCore, add allow_other_keys+allowed_keywords to IR+Chunk, emit metadata, update heap alloc+GC scan; deps: none; verification: emit/VM tests assert allow_other_keys and keyword list.
