---
title: reader keywords at end of input reuse the keyword as the name
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.214260+02:00"
---

Problem: at EOF EMIT-CREATE (habu2.f:3137, create/variable) and C-CONSTANT (3178) publish a record named 'create'/'variable'/'constant' (the latter two are keywords with no prim, so they land in the current wordlist); C-CHAR 3971 pushes the c of 'char', C-BCHAR 3976 bakes '[', C-TICK 3981/C-BTICK 3998 do nothing, C-POSTPONE 3668 dies rc 70 with the bare token; TRUSTED: (3229-3236) opens the band RW with PROT:LOPEN, hits LTOK x0=0 and returns WITHOUT PROT:LCLOSE - inside evaluate the caller resumes on RW pages (W^X fault, exit 134) and at a tty REPL the next line runs on RW pages; cast: and defer route through LCOMPILEDIE and close bands. Mirror has the same holes (forth.fs:3372,3399,3426,3441-3462). Acceptance: one shared 'reader keyword needs a name' die (the DEFER-DIAG:DIE-NO-NAME shape) used at every LTOK site, closing bands; a test per keyword with the keyword as the last token (exit code + diagnostic); mirror follows. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, test/. Verify: the tests; recovery gate. Depends: none. Ownership: reader. Claim: unassigned.
