---
title: Implement tagbody/go for explicit control flow
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-04T22:11:10.266426+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Low-level control flow primitive. (tagbody tag1 ... (go tag1) ...) allows arbitrary jumps within body. Many iteration macros expand to tagbody/go.
