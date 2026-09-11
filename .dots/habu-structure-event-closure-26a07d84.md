---
title: Structure event closure graph
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:33:19.610252+02:00"
---

tools/event-closure-lib.f:35-40,51-75,106-159 stores path offset/length, DFS state, recursion stack, and output order in five raw arrays; EC-FIND returns -1. DFS state is intended as 0 unseen, 1 visiting, 2 done, but EC-EXPAND treats every nonzero value as already visited, so an unknown/corrupt state silently omits a dependency from load order and downstream cache/check keys. Define nominal closure-node-id, ENUM visit-state, STRUCTURE closure-node with path span+state, one typed row buffer, and typed node-id buffers for DFS/order. Return option<node-id> from lookup; expand through exhaustive MATCH that distinguishes unseen/visiting/done and cannot accept another tag. Preflight pool+row capacity and append path/row atomically. Preserve breadth-first BUILD set, depth-first post-order LOAD-ORDER, include/require/provided semantics, cycles, dedup, source paths, and discovery errors. Add checker negatives for id/span/state/order swaps, invalid-state impossibility, cycle and repeated-edge cases, exact breadth-first set/post-order goldens, edit-sensitive content/cache keys, injected capacity/arena rollback, and canaries. Measure source/JIT/DATA/CODELEN, closure storage, build/order throughput and key generation before/after. Files: tools/event-closure-lib.f and focused consumers/tests. Verify event-closure/source-discovery/check/build-cache/fixpoint suites, typed-local diff, type/package/host/dot and full native gates. Ownership: closure graph representation/visit semantics only.
