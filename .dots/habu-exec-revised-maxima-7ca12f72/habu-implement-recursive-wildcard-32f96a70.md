---
title: Implement recursive wildcard descent for Maxima share/** search
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-07T19:20:07.490620+01:00\\\"\""
closed-at: "2026-03-07T19:57:06.989008+01:00"
close-reason: done (rewrote src/runtime/primitives/io.zig listDirectory wildcard expansion to support recursive ** / wild-inferiors directory traversal and added focused recursive wildcard regression; validated with zig build test filters for listDirectory/pathname wildcard tests)
---

src/runtime/primitives/io.zig:2428-2455 and src/runtime/primitives/pathname.zig. Root cause: listDirectory only scans one directory level, so PLAN.md 3.1 share/** paths are inert. Fix: support effective recursive wildcard descent for nested Maxima share packages. Why: autoload and file_search cannot work from source tree without real share/** traversal.
