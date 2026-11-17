---
title: Retire a rollback's records on x86-64 while a task is live
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.397075+03:00"
---

Problem: the four rollbacks (src/core/generated-declaration-dictionary.f ROLLBACK, src/habu/interpreter.f FRAME-ROLLBACK-CODE, src/habu/linker.f FORGET-SPACE, src/core/checker.f's overlay ROLLBACK) ask the primitive retire-from ( n -- bool ) first and lower nothing when it retires; src/habu/kernel-x64.f has no retire-from body, and its `cp!` and NDICT-SET-BODY exit 79 through TASK-LIVE-GUARD, so on x86-64 a throw out of an evaluate that defined something, or a rejected declaration, with a task live ends the process. arm64's body is src/habu/habu1.f BRETIREFROM over EM-DEF-OCC:LRETIRE. Reproducer (arm64: prints 42 and 7, rc 0), loaded after test/interpreter-loop-on.f on one --load line: 'require src/lib/task.f' / 'require src/lib/test/wait.f' / 'TASK:MIN-STACK TASK:TASK EV-WORKER' / 'variable EV-READY' / ': EV-SPIN ( -- ) 1 EV-READY atomic! begin TASK:PAUSE again ;' / ': EV-GO ( -- ) ['] EV-SPIN EV-WORKER TASK:ACTIVATE  EV-READY 1 EV-WORKER TEST-WAIT:FOR-CELL ;' / 'EV-GO' / ': EV-RUN ( -- n ) [: s" : EV-X ( -- n ) 1 ; 42 throw" evaluate-closed ;] catch ;' / 'EV-RUN . 7 . cr'. Acceptance: kernel-x64.f's retire-from retires the records from n up and answers true while a task is live, else false, as arm64's does; the reproducer prints 42 and 7 and exits 0 on x86-64. Depends: habu-lift-the-task-754c94f6; x86-64's turn. Claim: unassigned.
