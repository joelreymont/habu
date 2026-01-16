---
title: Fix remaining error masking sites
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:23:54.221616+02:00"
---

src/interp/vm.zig:1811,1836,2511 - Replace 'catch return error.*' with try:
1. Remove all catch clauses that replace errors
2. Propagate real errors with try
3. Update function signatures to !T if needed
Verification: Each error path returns correct error type
