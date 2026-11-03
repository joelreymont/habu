---
title: Name the file and line of an uncaught throw during a load
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T15:55:24.807338+02:00"
---

Problem: a throw nothing catches while hb --load runs a file's top level prints only 'hb: uncaught throw code <n>' (rc 67), with no file or line, whether it comes from the loaded file or a library it requires. Reproduced on the installed hb 2026-11-03: /tmp lib.f ': BOOM ( -- ) -9660 throw ; BOOM' required by main.f, and a main file with '-9660 throw' at top level: both print only 'hb: uncaught throw code -9660'. A checker refusal at load names its file and token (habu2.f LCOMPILEDIE's refusal tail reads the innermost open source path, include.f INCLUDE-EVALERR-CELL region). Found by phil while reviewing FSM (FSM:E-SYNTAX thrown from DEFINE at a library's load). Acceptance: an uncaught throw during a load prints the code plus the innermost open file's path and line (the same place format the refusal tail uses); exit status unchanged (67 or the code); a throw outside any load keeps today's line. Regression: an end-to-end row loading a file that requires a throwing library, asserting the path and line in stderr. Files: src/habu/stage2.f RUN / driver-io.f DRV-FAIL, src/core/include.f, src/habu/habu2.f LCOMPILEDIE (read). Verify: the new row; test/load-reject-diag-test.f. Depends: none. Ownership: the files above. Claim: unassigned.
