---
title: Refuse exporting an internal engine word
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T18:15:13.205813+03:00"
closed-at: "2026-10-01T08:21:06.197466+03:00"
close-reason: "C-EXPORT refuses a DNAME-INT source (rc 70, gate diagnosis); test/export-package.f and test/outer-interpret.f (82 cases, both routes) pass on the rebuilt engine, fail on b4e0."
---

Problem: the engine's `export` (`src/habu/habu2.f` C-EXPORT, 8748-8835) publishes an alias of a DNAME-INT source (an engine-internal COLON word, `layout.f:306-317`) and copies only the IMM, WIDE and MIN-IN bits (`habu2.f:8827-8831`), so the alias lacks DNAME-INT and the interpret gate (`hb: internal engine word: <token>`, `layout.f:313`) no longer stops it. Measured on engine `eff4ff42`: `package P public export DEFER-UNSET ;package` gives rc 0, and `P:DEFER-UNSET` then runs the internal word (`defer: unset execution vector`, rc 76). The Habu route (I8, `habu-move-pkgs-using-22f18b81`) refuses this case, so the two routes disagree on it until the engine does too.
Acceptance: C-EXPORT refuses a DNAME-INT source right after its lookup and before the checker call, with the interpret gate's `hb: internal engine word: <token>` on fd 2 and throw 70, publishing nothing. Data records (exempt from DNAME-INT, `layout.f:306-312`) still export: `42 constant K package P public export K ;package P:K .` prints 42.
Files: `src/habu/habu2.f` (C-EXPORT), the engine package or export suite that covers `export`, `test/outer-interpret.f` (the Habu-only internal-word export case becomes a both-routes case once I8 has landed).
Verify: the refused and accepted cases above through `hb --load`; rebuild on spark, five-generation chain, gate on the fixpoint.
Route: engine lane (baked `habu2.f`).
Ownership: krait (Intel lane).
