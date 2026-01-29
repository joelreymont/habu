---
title: Fix refine errors
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:03:06.689885+01:00"
---

Context: src/types/refinement.zig:45-119; cause: catch/orelse returns .unknown masking alloc/Z3 errors; fix: propagate errors via !RefineResult and make translateTerm fallible where needed; deps: none; verification: add test using failing allocator to assert error, run zig build test --filter refinement
