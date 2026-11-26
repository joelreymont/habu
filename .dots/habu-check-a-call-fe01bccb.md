---
title: "Check a call to an FSM:DEFINE machine as the load does"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:18:07.229235+02:00"
---

Problem (mcall fix worker, confirmed by the lead on master 8f6ec543 with its installed engine): bin/hb --load tools/check.f -- src/lib/net/model.f is rc 67, 'hb: uncaught throw code -9660' (src/lib/fsm.f E-SYNTAX), and so is the reproducer (an FSM:DEFINE machine M1, then ': RUN1 ( n -- ) construct sst sa M1 ;'); bin/hb --load of the reproducer is rc 0. On the installed engine and master 204743ce, check of model.f is rc 0 with no output, and so is check of 'require src/lib/fsm.f using FSM ENUM sst sa sb ;ENUM ENUM sin go ;ENUM : SENSE1 ( n -- sin ) drop construct sin go ; DEFINE M1 ( n ) sst sin / IN sa SENSE1 / go - >sb / FINAL sb - / ;DEFINE ;using : RUN1 ( n -- ) construct sst sa M1 ;' (/ a line break), whose --load is rc 0; re-run on spark with the installed hb, both commands and that --load are still rc 0 with no output, and what stopped the throw is not known. FSM:DEFINE alone checks. So check cannot check a library the model-call clients load. Acceptance: the cause found and fixed in the layer that owns it; check of model.f and of the reproducer rc 0. Files: unknown until the hunt (src/lib/fsm.f, tools/check-core.f, src/habu/verify-source.f). Verify: the two commands; a case in src/lib/fsm.f's test. Depends: none. Ownership: check of FSM machines.

Commit A of the fix is master e3c91ae2 "Place a file's definitions without markers"; B1 and B2, the rest of it, are not started and their plan is lost. Claim: none.
