---
title: Fix heap deinit error masking
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:06:04.745275+01:00"
---

Context: src/runtime/heap.zig:332; cause: ignore errors on seen.put; fix: propagate or handle allocator failure deterministically; deps: none; verification: add deinit test with failing allocator, run zig build test --filter heap
