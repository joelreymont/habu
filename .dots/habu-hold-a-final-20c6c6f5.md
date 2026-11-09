---
title: Hold a FINAL action to its ctx effect
status: open
priority: 3
issue-type: task
created-at: "2026-11-03T18:45:01.089590+02:00"
---

Problem: since 05113ade a FINAL row's run-word arm is `<final> OF construct <states> <final> over true swap <ACTION> ENDOF`, so the ACTION runs over `ctx state true ctx` and the checker no longer holds it to the documented `( <ctx> -- )` (docs/forth.md, State machines). A FINAL ACTION `( bool ptr n -- bool )` loads and can return false, so the run dispatches on the FINAL state again (probe deep.f below: loads, runs the action 3 times, `test: ok`; at fsm.f before 05113ade the checker rejected it, rc 70, `in hold: at 'REPEAT-END' expected: bool ptr n actual: ptr n ptr n`). The doc's "a word with the wrong effect" fails the load was already loose at 3b36b1cd for IN line actions: an action `( ptr n ptr n -- ptr n )` loads (probe twin.f, `test: ok`). Found by the Opus review of the FINAL fix, 2026-11-03 (Low).

Probes (were /tmp/phil-fsm-final-review/p/; text here): deep.f = a machine `DEFINE HOLD ( ptr n ) hold tick / IN go SENSE / once - >done / FINAL done REPEAT-END ;DEFINE` with `: REPEAT-END ( bool ptr n -- bool ) {: f:bool c:ptr :} 1 c +! c @ 3 >= f and ;`; twin.f = the same machine with line `once TWIN >done`, `: TWIN ( ptr n ptr n -- ptr n ) {: x:ptr y:ptr :} 1 x +! y ;` and `FINAL done -`.

Candidate (review, untested): DEFINE also writes `: <NAME>/<final> ( <ctx> -- ) <ACTION> ;` per FINAL row and calls it last in the arm, and an IN line's action likewise through a word with the declared `( <ctx> -- )` effect, as IN rows' `<NAME>/<state>` words already declare theirs. This adds generated words to the caller's package. Approved 2026-11-09 (DECISIONS.md, Language and checker): a generated NAME/<final> ( ctx -- ) word per FINAL row, called last in the arm; designed together with habu-generate-name-step-52f37768 under parent habu-fsm-stepping-from-c8625faf.

Acceptance: deep.f and twin.f fail the load (rc 70 under evaluate-closed) with the checker naming the action; every fsm-test.f case, including FAILS (a FINAL action that always throws), still passes; docs/forth.md states the generated words.

Verify: `HB_TMP=<tmp> bin/hb src/lib/fsm-test.f` with the two probes added as REJECTED cases; Zen's translation machine (src/jobs/worker.f translate-machine) still loads.
