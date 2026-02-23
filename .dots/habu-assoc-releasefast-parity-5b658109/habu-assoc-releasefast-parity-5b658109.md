---
title: Assoc ReleaseFast parity
status: active
priority: 2
issue-type: task
created-at: "\"2026-02-23T02:05:43.717464+01:00\""
---

bench-comp assoc in ReleaseFast remains below SBCL (Habu JIT ~5.23ms vs SBCL ~2.79ms, ~0.53x). sample shows jitAssoc dominates runtime. Next fix: reduce per-iteration helper overhead for assoc/mod loops without breaking generic CL semantics.
