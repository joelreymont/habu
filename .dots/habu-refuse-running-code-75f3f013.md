---
title: Refuse running code in the open code unit
status: closed
priority: 3
issue-type: task
created-at: "2026-10-01T09:04:08.260168+03:00"
closed-at: "2026-10-01T10:07:49.629324+03:00"
close-reason: "Code window closes on evaluate return, REPL read, LEVALREC, LRBYE and LUNCAUGHT; tier-0 token reopens, immediates no longer reopen; outer-interpret 115 cases pass on rebuilt 4c9d9554 (gen2 equal); same-unit cases SIGSEGV 134 on e11c, immediate-semi case 134 on 6fde360a"
---

Problem: the engine head (`:`, `kernel:`, `trusted:`) leaves the PROT window over CP's 64K unit (PROT-PAGE-MAX) open until `;`. Code compiled earlier in that unit cannot execute meanwhile. Measured by the I5a lane on engine b4e05778: a program-code exit hook (`' W data-base EXIT-HOOK-CELL + !`) compiled in the same unit as a pending head crashes the engine at exit with SIGSEGV (rc 134) instead of running or refusing. `test/outer-interpret.f` HEAD-TIER-0 and PENDING work around it by moving CP two units on (`cp@ PROT-PAGE-MAX 2 * + cp!`).
Acceptance: find every path that can execute code in the open unit while a head is pending (exit hook at least; check `evaluate` of an earlier word, top-row hooks, the uncaught-throw reporter) and either close the window before the call or refuse with a named diagnostic; no SIGSEGV. The Habu definer route (`src/habu/definers.f`) behaves the same. Remove the CP-moving workaround from the two test cases.
Files: `src/habu/habu2.f` (exit path, head), `src/habu/definers.f`, `test/outer-interpret.f`.
Verify: an exit hook in the same unit as a pending head, on both routes; rebuild, chain, gate.
Ownership: krait (Intel lane).
