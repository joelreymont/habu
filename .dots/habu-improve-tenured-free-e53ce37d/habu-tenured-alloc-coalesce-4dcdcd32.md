---
title: "Tenured alloc: coalesce/split policy"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-20T08:58:56.375058+01:00\\\"\""
closed-at: "2026-02-20T14:48:49.190379+01:00"
close-reason: Added bounded best-fit coalesce and split policy
blocks:
  - habu-tenured-alloc-segregated-942b726a
---

src/runtime/heap.zig: implement bounded coalescing/splitting policy to reduce fragmentation.
