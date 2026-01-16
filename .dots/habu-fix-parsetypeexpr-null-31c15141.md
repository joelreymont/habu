---
title: Fix parseTypeExpr null returns
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:07:30.525839+02:00"
---

src/compiler/compile.zig:5074 - parseTypeExpr returns null on invalid forms, invalid types ignored. Return error.InvalidTypeExpr. Medium severity.
