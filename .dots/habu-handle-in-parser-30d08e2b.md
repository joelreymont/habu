---
title: "Handle #+ #- in parser"
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T16:30:32.481932+02:00"
---

File: src/reader/parser.zig. On feature_present/absent token: read feature expr, eval, if false skip next form (parseExpr), if true parse normally. Handle nested.
