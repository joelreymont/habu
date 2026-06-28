---
title: "B2: local-shadows-builtin error"
status: closed
priority: 1
issue-type: task
created-at: "\"2026-06-27T13:15:33.496641+02:00\""
closed-at: "2026-06-28T15:46:54.890017+02:00"
close-reason: "Landed gate-green (warm 120783ms<=160000ms, fixpoint, 0 non-budget). habu2.f C-LBRACE-DIE emits 'habu: local must be at word top, not in a loop/branch, quotation, or after exit' + exit 75, replacing the 3 raw bare-token traps in C-LBRACE-GUARDS (control-flow/quotation/dead-exit). TRUSTED.md row + 87-pin re-sync. Negative+positive fixture test/gate-diagnostics.f GDX-LOCAL-IN-LOOP (rc 75 + diagnostic; word-top local still compiles)."
---

Checker/compiler: when a {: :} local name collides with a built-in word (i/j/k loop indices, code, dup,...), error at the {: :} ('local X shadows built-in; rename') instead of emitting the bare token + exit 75 with no diagnostic. src/habu locals compiler. Pairs with C4.
