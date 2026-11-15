---
title: "User source arms the storage definers' window and mints a typed pointer"
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T19:25:35.107622+02:00"
---

Problem: user source reaches the checker's trust, each rc 0 on the typed-children engine:
A. `is` on an engine defer: ': SETIT ( -- ) ['] MYEVAL is TYPE-DECL:TDECL-EVAL-XT ;' loads and checks, since IS-TOK (src/core/checker.f) checks only the effect; with it in place, the next declaration sumtype.f evaluates through TDECL-EVAL-XT runs MYEVAL (not rerun since the layout buffer's window was deleted). TDECL-EVAL-XT (src/core/sumtype.f) is also the evaluator sumtype's own generators call; TDECL-NAME-PREFLIGHT-XT and TDECL-CAPACITY-PREFLIGHT-XT are untried.
B. A top-level row retypes a defined word: `: FOOB ( -- n ) 1 ;  s" FOOB" PE-OPEN PE-PTR-N PE-OUT PE-CLOSE  : T ( -- n ) FOOB @ ;` loads and checks (src/core/checker.f PE-OPEN :11679, PE-CLOSE :11701, global words); CHECK:PRIM-OPEN ... CHECK:PRIM-CLOSE (src/core/check-walk.f:3281-3296), which a build's checked code calls (src/arch/arm64/link-kernel.f:188-190), records a row without replacing a defined word's own record: the same program through it is rejected. The PE- words are the checker's own, which dot habu-take-a-word-01856558 makes private and strips. SOURCE-UNIT:USE is likewise reachable (not carried further).
Acceptance: each program above is rejected under --load and under tools/check.f, as a test; no word user source can call rebinds an engine defer.
Depends: none. Claim: unassigned.
