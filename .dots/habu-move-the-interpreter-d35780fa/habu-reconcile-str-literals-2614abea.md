---
title: "Reconcile string literals with allot's task guard"
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T13:18:38.591263+03:00"
closed-at: "2026-09-30T14:44:16+03:00"
close-reason: "Interpret literal copies take the task-live guard; TASK-LIVE cases agree on both routes on 06d8d4ab (base: engine printed hi, loop 79), gate 501/501 rc 0."
---

Problem: the Habu interpret loop (I4b, `src/habu/outer.f` ROOM) reserves string-literal data through `allot`, whose task-live guard (BALLOT, `habu1.f:1719`; B-TASK-LIVE-GUARD `habu1.f:1300-1303`) exits 79 with no output while any task runs. The engine's interpret copies (C-ISDQ, C-ICQ, C-EISDQ, C-EICQ, C-EIDOTQ) advance DP with DP-CHECK only. Demonstrated by I4b's review: prelude starts a spinning task; `s" hi" type cr` prints `hi` rc 0 on the engine and exits 79 silently in the Habu loop. `docs/threads.md:40` promises a remote REPL evaluates while tasks run, so once evaluate moves to the Habu loop a typed string literal would end the process.
Acceptance: one rule for data-space reservation by interpret-time literals while tasks are live, applied to both routes (either the engine's copies take the guard, or the loop reserves through a row that makes the same data-space check the engine does); the rule stated in `docs/forth.md` or `docs/threads.md`; a route-equality case in `test/outer-interpret.f` with a live task.
Files: `src/habu/outer.f`, and `src/habu/habu2.f`/`src/habu/prims.f` if the rule changes the engine; `test/outer-interpret.f`; the doc.
Verify: spark: the case through both routes; rebuild, chain, gate if the engine changes.
Depends: habu-interpret-literal-keywords-0fa50d62 (I4b).
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Design: the engine's copies take the guard. Every other data-space sink already exits 79 while a task is live on both kernels (`allot`, `align`, `,`, `c,`: `habu1.f:1719-1735`; x86 twins `kernel-x64.f:2148-2161`), and `evaluate` too (`habu1.f:1416-1417`); `docs/forth.md:917-919` lists interpret-mode literals as data-space sinks; commit 5837edd5d guarded `BALLOT`, `BCOMMA`, `BCCOMMA`, `B-EVAL` but omitted `C-ISDQ` and its twins. The alternative needs a new unguarded DP row on both kernels and keeps an accidental exemption alive.
- In `src/habu/habu2.f`, insert `B-TASK-LIVE-GUARD` (`habu1.f:1300`; clobbers only x9, free there) immediately before `14 DP-CHECK` at 4485 (C-ISDQ), 4502 (C-ICQ), 4524 (C-EISDQ), 4539 (C-EICQ), 4552 (C-EIDOTQ): after the scan, the consume and the 255-byte caps, before any byte is written; the same point where the Habu loop's `ROOM` calls `allot`, so both routes give 74/76 first, then silent 79, then the data-space check's 76. `C-IDOTQ` reserves nothing and stays unguarded. `outer.f` changes only by rewriting the note at 535-539. `prims.f` and `kernel-x64.f` do not change.
- Test (`test/outer-interpret.f`): a prelude used only by this case does `require lib/task.f` and defines `: OI-SPIN-BODY ( -- ) begin TASK:PAUSE false until ;` (`0 until` is refused, rc 70) and a starter `OI-SPIN`; the case itself calls `OI-SPIN` (a task started in the prelude would kill the load of `test/outer-loop-on.f` at its `package`). Cases: `OI-SPIN s" hi" type cr` gives rc 79 with empty stdout and stderr on both routes (before the change the engine gives rc 0 and prints `hi`: the failing check); control `OI-SPIN ." ok" cr` gives rc 0 and prints `ok`.
- Docs: `docs/forth.md:917-919` states that every data-space sink, these literals included, and `evaluate` exit $4F with no output while a task is live; correct `docs/threads.md:36-40` and `docs/genio.md:183-188` to say a REPL can neither evaluate nor define while a task is live (measured on `hb-master-3dc7`: `s" 7 . cr" evaluate` exits 79 with a spinning task live).
- Verify: spark, rebuild, five-generation chain, full gate.
- `habu-stop-top-level-93543435` edits `C-CHAR` right next to these hunks; one worker takes both, in two commits.
