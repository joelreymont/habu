---
title: Fix canonical structure types
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.146100+02:00"
blocks:
  - habu-separate-structure-70df2f68
---

Problem: structure type/class predicates are package-sensitive and collapse into generic vector or standard-object answers. Acceptance: class-of, type-of, typep, and subtypep use canonical COMMON-LISP structure symbols and lattice. Files: src/runtime/primitives/clos.zig, lib/stdlib.habu, runtime type predicates. Verify: structure-object and structure-class regressions under non-CL current packages. Blockers: habu-separate-structure-70df2f68.
