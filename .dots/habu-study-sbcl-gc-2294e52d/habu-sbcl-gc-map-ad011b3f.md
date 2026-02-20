---
title: "SBCL GC: map alloc/card paths"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-20T08:58:56.155546+01:00\\\"\""
closed-at: "2026-02-20T09:05:48.993067+01:00"
close-reason: Mapped SBCL alloc-region and card-scan techniques
blocks:
  - habu-sbcl-gc-map-03111566
---

docs/gc-architecture-comparison.md:1, /tmp/sbcl/src/runtime/gencgc.c: extract alloc-region, card-marking, and scan fast-path techniques for Habu runtime/gc.zig.
