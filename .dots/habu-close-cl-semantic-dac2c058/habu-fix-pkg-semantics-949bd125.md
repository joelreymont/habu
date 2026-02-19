---
title: Fix package semantics
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:16.839980+01:00\""
closed-at: "2026-02-19T23:46:44.602767+01:00"
close-reason: "covered by package primitive + integration regressions (src/runtime/primitives/package.zig:1497,1552; src/tests/integration.zig:4930)"
---

src/runtime/primitives/package.zig and src/compiler/compile.zig defpackage path. Cause: import/shadow/use semantics mismatches. Fix: strict CL package behavior with conflict/error conditions.
