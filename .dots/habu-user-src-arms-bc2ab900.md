---
title: "User source arms the storage definers' window and mints a typed pointer"
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T19:25:35.107622+02:00"
---

Problem: user source reaches the checker's trust, rc 0 on the typed-children engine: `is` on an engine defer: ': SETIT ( -- ) ['] MYEVAL is TYPE-DECL:TDECL-EVAL-XT ;' loads and checks, since IS-TOK (src/core/checker.f) checks only the effect; with it in place, the next declaration sumtype.f evaluates through TDECL-EVAL-XT runs MYEVAL (not rerun since the layout buffer's window was deleted). TDECL-EVAL-XT (src/core/sumtype.f) is also the evaluator sumtype's own generators call; TDECL-NAME-PREFLIGHT-XT and TDECL-CAPACITY-PREFLIGHT-XT are untried.
Acceptance: the program above is rejected under --load and under tools/check.f, as a test; no word user source can call rebinds an engine defer.
Depends: none. Claim: unassigned.
