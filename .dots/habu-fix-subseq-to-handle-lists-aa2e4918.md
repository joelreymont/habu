---
title: Fix subseq to handle lists
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:28:04.336301+02:00"
---

src/compiler/compile.zig:6803 compileSubseq only handles strings.
For lists, needs to use nth and cons to extract subsequence.
Either: detect list type and emit different IR, or remove builtin and use stdlib impl.
Test: (subseq '(a b c d e) 1 3) => (b c)
