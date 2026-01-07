---
title: Pretty error formatting (Bebop-style with source context)
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-17T18:29:18.713508+02:00"
closed-at: "2025-12-17T18:44:54.862705+02:00"
close-reason: ""
---

Implement SourceContext for nice error messages with line/column, source snippets, and ^^^ markers. See bebop/compiler/src/error.rs for reference. Format: error: message → file:line:col | line content | underline
