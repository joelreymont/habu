---
title: Fix canonical structure types
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.146100+02:00\""
closed-at: "2026-04-03T14:57:35.825546+02:00"
close-reason: "done: canonical COMMON-LISP structure/class symbols in type lattice; regression added for non-CL package behavior; zig build green; zig build test still blocked by existing baseline"
blocks:
  - habu-separate-structure-70df2f68
---

Problem: structure type/class predicates are package-sensitive and collapse into generic vector or standard-object answers. Acceptance: class-of, type-of, typep, and subtypep use canonical COMMON-LISP structure symbols and lattice. Files: src/runtime/primitives/clos.zig, lib/stdlib.habu, runtime type predicates. Verify: structure-object and structure-class regressions under non-CL current packages. Blockers: habu-separate-structure-70df2f68.
