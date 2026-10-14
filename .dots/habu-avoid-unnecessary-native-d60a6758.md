---
title: Avoid unnecessary native return frames
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T17:03:00.611053+03:00"
---

Otto measured8,852bytes of x30 prologue/epilogue code in Dots; estimated1–2KB savings unverified. Determine which native routines can omit return-address save/restore from actual call/control-flow/frame facts. Preserve calls, exceptions, recursion, wide locals and unwinding. Acceptance: same-source executed artifacts and measured code reduction with meaningful affected E2Es before implementation, native suite, convergence and Astra review.
