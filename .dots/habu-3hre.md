---
title: Fix let-inside-cond codegen bug
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-06T19:14:16.883643+02:00"
closed-at: "2025-12-08T14:08:26.122474+02:00"
close-reason: ""
---

Native Habu compiler generates incorrect code for let expressions inside cond branches. This causes crashes (EXC_BAD_ACCESS) when running compile-expr-full which has this pattern. The comment at compiler.lisp:332 notes 'avoid let inside cond - causes crash'. This blocks Stage 1 self-compilation because compile-forms requires compile-expr-full.
