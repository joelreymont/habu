---
title: Fix compiler type error masking
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:07:25.959638+02:00"
---

src/compiler/compile.zig:1429 - ctx.bind uses catch continue, parseTypeExpr failure returns, drops type errors. Use try ctx.bind, error on invalid type. Medium severity.
