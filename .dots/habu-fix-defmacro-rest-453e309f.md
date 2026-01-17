---
title: Fix defmacro &rest parameter bug
status: open
priority: 2
issue-type: task
created-at: "2026-01-17T10:41:50.694923+02:00"
---

src/compiler/compile.zig:4574 transformDestructuredParams, src/interp/repl.zig:1315 handleDefmacro: Macros with &rest params compile with has_rest=false instead of has_rest=true. The lambda chunk is created correctly but wrong chunk is used when creating closure. Investigate macro expansion or chunk indexing.
