---
title: Canonize file create mutate and directory ops
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.236422+02:00"
blocks:
  - habu-close-pathname-algebra-be27460c
---

Problem: open, create, rename, delete, ensure-directories-exist, and directory helpers do not share one canonical pathname and trusted-root contract. Acceptance: all file mutation and directory operations consume canonical pathname designators and fail closed. Files: src/runtime/primitives/io.zig, lib/stdlib.habu, VM file op entrypoints. Verify: focused filesystem op regressions under trusted-root policy. Blockers: habu-close-pathname-algebra-be27460c.
