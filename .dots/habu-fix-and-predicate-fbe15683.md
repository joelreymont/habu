---
title: Fix and + predicate + closure bug
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-17T08:44:57.361305+02:00\""
---

src/compiler/compile.zig: (and/or (predicate x) Y) where x is param or closure var causes CompileError. Occurrence typing bug. Blocks stdlib:729 destructuring-bind-impl, 822 char-whitespace-p, and many others. CRITICAL blocker
