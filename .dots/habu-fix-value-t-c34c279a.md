---
title: Fix Value.t()/Value.nil() calls
status: open
priority: 2
issue-type: task
created-at: "2026-01-29T10:06:00.290189+01:00"
---

Context: src/runtime/primitives/clos.zig:285,327,471; cause: calling constants like functions; fix: use Value.t/Value.nil; deps: none; verification: run zig build test --filter clos
