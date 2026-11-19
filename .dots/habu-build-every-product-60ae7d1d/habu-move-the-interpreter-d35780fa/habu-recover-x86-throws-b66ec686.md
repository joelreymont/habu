---
title: "Retire a rollback's records on x86-64 while a task is live"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.397075+03:00"
closed-at: "2026-11-19T13:51:46.612548+01:00"
---

Problem: the four rollbacks (src/core/generated-declaration-dictionary.f ROLLBACK, src/habu/interpreter.f FRAME-ROLLBACK-CODE, src/habu/linker.f FORGET-SPACE, src/core/checker.f's overlay ROLLBACK) ask src/core/checker.f XREF:RETIRE-IF-TASKS ( n -- bool ) first (FRAME-ROLLBACK-CODE calls XREF:RETIRE-ABOVE) and lower nothing when it retires; whether a throw out of an evaluate that defined something, or a rejected declaration, with a task live keeps the process running on x86-64 has not been run. Reproducer (arm64: prints 42 and 7, rc 0), loaded after test/interpreter-loop-on.f on one --load line: 'require src/lib/task.f' / 'require src/lib/test/wait.f' / 'TASK:MIN-STACK TASK:TASK EV-WORKER' / 'variable EV-READY' / ': EV-SPIN ( -- ) 1 EV-READY atomic! begin TASK:PAUSE again ;' / ': EV-GO ( -- ) ['] EV-SPIN EV-WORKER TASK:ACTIVATE  EV-READY 1 EV-WORKER TEST-WAIT:FOR-CELL ;' / 'EV-GO' / ': EV-RUN ( -- n ) [: s" : EV-X ( -- n ) 1 ; 42 throw" evaluate-closed ;] catch ;' / 'EV-RUN . 7 . cr'. Acceptance: XREF:RETIRE-IF-TASKS retires the records from n up and answers true while a task is live, else false, on x86-64 as on arm64; the reproducer prints 42 and 7 and exits 0 on x86-64. Depends: habu-lift-the-task-754c94f6; x86-64's turn. Claim: unassigned.
