---
title: Update Closure to store Chunk Value
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:15:45.251450+02:00"
---

Files: src/runtime/objects.zig, src/bytecode/emit.zig
Change Closure.chunk from raw pointer to Value.
Update all Closure creation code to use Chunk Value.
Update bytecode emit to allocate Chunk as GC object.
Dependencies: habu-design-chunk-gc-573e1aef
Verification: closures compile, chunks allocated on heap
