---
title: Implement adaptive nursery sizing
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-20T08:55:19.457590+01:00\""
closed-at: "2026-02-20T10:56:35.316503+01:00"
close-reason: Complete adaptive nursery derive/runtime/benchmark phases
blocks:
  - habu-feed-gc-metrics-8a4ffc19
---

File: src/runtime/heap.zig:1, src/runtime/gc.zig:1; cause: static nursery sizing amplifies pause/throughput tradeoff; fix: dynamic nursery targets from survival/allocation rates; why: match SBCL/OCaml adaptive behavior.
