---
title: Reject repeated qualifiers in changed Forth
status: closed
priority: 2
issue-type: task
created-at: "2026-07-28T01:30:50.439119+02:00"
closed-at: "2026-07-28T15:26:27.075318+02:00"
close-reason: "Rejected: repeated qualification is source style, not a correctness invariant. Do not add a source lint or commit gate; keep naming clear through package design and ordinary review."
---

Rejected scope: repeated package qualification is source style, not a correctness invariant. Do not add a lint or commit gate for it. Keep package calls readable through clear package design and ordinary review.
