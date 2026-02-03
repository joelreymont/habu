---
title: Fix VM GC chunks
status: open
priority: 1
issue-type: task
created-at: "2026-02-03T10:40:04.234971+01:00"
blocks:
  - habu-fix-gc-scan-26c89ff5
---

src/interp/vm.zig:662-863: collectGarbage roots constants but not chunk objects; never updates self.chunk, Frame.chunk, or chunk_pool pointers; also mutates state even if heap.collectGarbage errors (no try). Fix: root chunks as Values (current + frames + chunk_pool); after GC, update pointers from relocated Values; delete redundant constant rooting/update. Add regression: run bytecode with nested lambdas + small heap, trigger GC twice, ensure return continues + make_closure works. Verification: zig build test.
