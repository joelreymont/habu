---
title: Postpone a private word without its name
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T23:00:56.799786+01:00"
---

Problem: postpone finds INTERPRETER's private `postponed` by name (src/core/checker.f POSTPONE-RUN-REC, REC-HOME-SYM, POSTPONED-REC; src/habu/interpreter.f POSTPONE-RUN through HANDLE-REC), so an hb with its private names stripped (habu-re-enable-hb-a2404c26) cannot postpone a private word. CHECKER-POSTPONE:CLEAR, :CUT and :ADD and their cells PP-P, PP-CAP, PP-N and PP-OWNER are public, so checked code binds a body token to another package's private word: `package RP private : SECRET ( -- n ) 42 ; ;package  ndict@ 1 - constant RP-IX  : RP-MAKE ( -- ) s" RP-C" s" ( -- n )" define-word s" SECRET" body-append 14 RP-IX CHECKER-POSTPONE:ADD postpone ; ;  RP-MAKE  RP-C . cr` prints 42 on the installed hb while `RP:SECRET` is E-UNDEFINED. Acceptance: `postponed` is an ordinary global primitive beside define-word, body-append and literal; a stripped hb postpones a private word with the postponed call the unstripped hb makes; the RP program is refused; CHECKER-POSTPONE and its cells are deleted; test/postpone-define.f passes. Depends: none. Claim: agent=jerry, design first (a Plan agent, read-only; no workspace yet).
