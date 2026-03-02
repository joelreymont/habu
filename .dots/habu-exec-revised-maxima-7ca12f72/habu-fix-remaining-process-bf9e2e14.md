---
title: Fix remaining process-level Maxima crashes
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-03-07T19:32:55.760474+01:00\\\"\""
blocks:
  - habu-adopt-canonical-test-a8a0cbe4
---

Files determined by first crashing test per file after Checkpoint A plumbing lands; likely src/interp/vm.zig plus affected Maxima callers. Root cause: remaining process aborts still block execution coverage. Fix: reproduce each first crash deterministically and convert it to ordinary fail/pass behavior. Why: execution coverage cannot progress while any target file still kills the process.
