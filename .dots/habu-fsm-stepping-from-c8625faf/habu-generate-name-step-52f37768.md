---
title: Generate NAME-STEP beside the runner
status: closed
priority: 2
issue-type: task
created-at: "2026-11-09T19:10:47.090579+02:00"
closed-at: "2026-11-10T07:05:54.426725+02:00"
close-reason: "landed; src/lib/fsm-test.f ends test: ok with the review's fold-ins"
---

Problem: src/lib/fsm.f:187 RUN-WORD emits only the looping runner NAME. A consumer that supplies one input per call dispatches over NAME/state itself (Tender src/admit.f:2048-2050; five dispatchers, 25 row-word calls) and a machine without a natural terminal state needs fake FINAL states that only store a phase.
Acceptance: FSM:DEFINE also emits `NAME-STEP ( ctx phase -- phase bool )`. From an IN state it runs SENSE, the selected ACTION and NEXT once each and returns the target phase and false, also when the target is a FINAL. From a FINAL it runs that state's action once (none for `-`) and returns the same phase and true. A throw from any callback propagates with no result pair left. No internal loop. A table with no FINAL loads and steps. The runner NAME keeps its effects and callback trace and runs a terminal action exactly once per run, including when started at a FINAL. One parser, one row representation and one arm emitter serve both words; no emitted arm ends in a literal after a possibly nonreturning callback. The hyphen suffix is deliberate: a state named step would collide with NAME/STEP. Visibility of NAME-STEP follows the declaration's package and section. Generator buffers are released on success and on failure.
Files: src/lib/fsm.f (RUN-WORD 187, IN-ROW 143, FINAL-ROW 159, GENERATE 194), src/lib/fsm-test.f.
Verify: `bin/hb src/lib/fsm-test.f` with cases added through the real loader: a self-loop steps once and returns false; active-to-final returns the final phase and false without running the final action; STEP at a FINAL runs it once and returns true; stepping it again runs it again; a no-final table steps; a throw from sense, action, next or a final action propagates its code; the existing FAILS (always-throwing) cases still load; a rejected definition followed by a valid one loads cleanly; the runner trace is unchanged on every existing case.
Depends: habu-hold-a-final-20c6c6f5 (same arm-emission design; settle both in one design).
Ownership: src/lib/fsm.f, src/lib/fsm-test.f. Approved 2026-11-09 by Joel ("Approve NAME-STEP (Recommended)"). Claim: agent=carl worker workspace=.jj-ws.noindex/carl-step.

Review (Opus code-reviewer, 2026-11-09, commit d4b81106 in carl-step): REVISE, no correctness defect; fold-ins at landing, after incremental builds work.
- F1 src/lib/fsm.f:164-167: the touched IN-ROW comment is four lines; replace with "\ Opens the state's row word and its arm in the step word; a row has at least" / "\ one line, so the token after SENSE, or after `( <inputs> )`, is its first."
- F2 src/lib/fsm-test.f:342 `SPIN-CELL @ 3 T=` cannot fail once line 341 passes: delete it.
- F3 src/lib/fsm-test.f:340-345: the SPIN-STEP, GATE-STEP and JOB-STEP throw cases repeat THROWS (189), FAILS (214) and TOTAL (252), which now throw through the step word: delete them; keep the PEEK case (338-339).
- F4 src/lib/fsm-test.f:283: replace with "\ No FINAL row: LAMP's runner never returns, so the test steps it."
- F5: close this dot in the landing commit.

Fold-ins F1-F4 applied at landing (2026-11-10); src/lib/fsm-test.f ends "test: ok" on the installed hb.
