---
title: Fix package semantics
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:16.839980+01:00"
---

src/runtime/primitives/package.zig and src/compiler/compile.zig defpackage path. Cause: import/shadow/use semantics mismatches. Fix: strict CL package behavior with conflict/error conditions.
