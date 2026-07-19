---
title: Harden CUDA consumer lifecycle
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T20:46:08.234573+02:00"
---

examples/kernel-consumer/main.zig:172,180,191,193 silently discards cuDevicePrimaryCtxRelease, cuModuleUnload, and cuMemFree_v2 results in defers. This violates the repository error rule and can report success after cleanup failure; the two buffer defers also make partial-allocation cleanup ordering implicit. Lines 106 and 118 use catch unreachable for fixed-width hash rendering without expressing or proving the exact-width invariant. Lines 229-234 validate only y[n-1], so a kernel that computes one endpoint correctly and corrupts the other 1023 elements passes the canonical example. Refactor CUDA resources into small owned wrappers whose deinit path records and returns the first cleanup error when the main operation succeeded, while preserving the primary failure and attaching/reporting any cleanup failure when both occur; prove partial acquisition releases exactly the acquired resources once in reverse order. Replace catch unreachable with exact-size hex encoding whose type/length proves capacity. Validate every SAXPY output element, including n=0/1 and values that discriminate indexing/stride errors, with an appropriate float comparison for the stated operation. Add injected-driver lifecycle tests for each acquisition and cleanup failure, double-failure precedence, exact call order, and full-output corruption at first/middle/last positions. Files: examples/kernel-consumer/main.zig and focused consumer tests. Depends: habu-make-kernel-example-d6434bd6 for a compile-testable harness. Ownership: CUDA resource/error lifecycle, digest formatting, and result verification only; no manifest ABI validation or artifact discovery.
