---
title: Delete the dead words and payload the audit found
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:25:10.800315+03:00"
---

Remove demonstrated dead source and payload after build/test work. The old
PFX-APPEND-ENGINE-SNAP-HOOK chain is already gone. SIGSCOPE! and SIGSCOPE-OFF
still have no callers, leaving the enabled SIG-SCOPE$ branch unreachable;
confirm their current consumers before the focused removal.

The indexed CHK-PTR-U8-SLOT, CHK-PTR-U8-SLOT@ and CHK-PTR-U8-SLOT!
helpers in tools/check-core.f also have no callers in the current tree.
Remove that small dead family after confirming source and generated uses;
the non-indexed CHK-PTR-U8@/! helpers are used and remain.

Do not require the retired textual zero-reference census/gate. Use the actual
call/retention paths for each proposed deletion, preserve public named behavior,
and measure the affected engine bytes. The wrej branch has a documented widened-
input guard; absence of a producer today alone does not justify removing that
boundary. Capture/codegen changes require matching native checks and convergence;
recovery checks apply when the seed/mirror path changes.
