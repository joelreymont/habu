---
title: Check JIT compile latency
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-04T06:59:33.231124+01:00\""
closed-at: "2026-02-04T07:00:24.763780+01:00"
close-reason: Add bench-check compile latency guard
---

bench/check.zig: add max compile_ns_per threshold and CLI flag; enforce jit compile time; deps: none; verification: zig build bench-check
