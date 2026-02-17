---
title: Fix macro expansion edges
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:16.844311+01:00"
---

src/compiler/compile.zig expandMacro and lib/stdlib.habu macro helpers. Cause: macro lambda-list/env handling breaks large sources. Fix: spec-correct macro call/expansion semantics.
