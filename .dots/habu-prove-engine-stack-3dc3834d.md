---
title: Prove engine-stack-debugger breakpoints persist
status: open
priority: 3
issue-type: task
created-at: "2026-10-23T17:33:29.573872+03:00"
---

Problem: test/engine-stack-debugger.f says calling the word twice proves the breakpoint stays planted, but it checks for one habu-bp: marker, so a one-shot breakpoint passes (g7 test audit lane).
Acceptance: the persistent cases require two markers; a one-shot breakpoint fails the suite.
Verification: the suite passes; a mutation making the breakpoint one-shot turns it red.
