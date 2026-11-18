---
title: "Build a program that undefines hb's word in an evaluated text"
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T23:40:35.189177+01:00"
---

Problem: built by the installed hb, a program file 's" undefine dup" evaluate' then ': MAIN ( -- ) 1 . cr ;' ends the build with E-MEMORY-FAULT in DICT-CELL@ (address 0x23fffe0), rc 110; 'undefine dup' written in the file, and an evaluated undefine of the file's own word, build and run. Inside LINKER:WITH's body (test/linker.f's BUILD), '[: s" undefine dup" evaluate-closed ;] catch' is rejected E-UNDEFINED: cannot find word 'undefine', rc 70. Acceptance: the program builds and prints 1, the body's evaluated undefine retires dup, and a test holds each. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-uh.
