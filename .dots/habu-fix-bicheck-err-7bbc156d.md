---
title: Fix bicheck error masking
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:04:48.556396+01:00"
---

Context: src/types/bicheck.zig:425-478,606-607; cause: catch false/return default masks errors; fix: make convertible/isSubtype/makeOr error-propagating and thread errors to callers; deps: habu-fix-refine-errors-160e415d; verification: add failing-allocator test for propagate, run zig build test --filter bicheck
