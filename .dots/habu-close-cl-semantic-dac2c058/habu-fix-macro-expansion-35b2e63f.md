---
title: Fix macro expansion edges
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:16.844311+01:00\""
closed-at: "2026-02-19T23:47:07.240869+01:00"
close-reason: "expandMacro handles &whole/&environment in src/compiler/compile.zig:3391+ with regression src/tests/integration.zig:1208"
---

src/compiler/compile.zig expandMacro and lib/stdlib.habu macro helpers. Cause: macro lambda-list/env handling breaks large sources. Fix: spec-correct macro call/expansion semantics.
