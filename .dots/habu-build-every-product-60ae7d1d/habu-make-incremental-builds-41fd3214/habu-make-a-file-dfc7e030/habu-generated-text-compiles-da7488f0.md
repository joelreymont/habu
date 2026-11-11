---
title: Generated text compiles through the typed loop on arm64
status: closed
priority: 2
issue-type: task
created-at: "2026-11-04T14:14:14.895152+02:00"
closed-at: "2026-11-04T19:43:38.297257+02:00"
close-reason: "landed: f7e597db 22ce649d ca0cb629 715f92ad 92b7dbcc on 3378e2d6. Proof at the tip candidate (/tmp/jerry-step1-land/hb, built by the installed hb, rc 0): compiler-shadow row (shadow.f then shadow-kinds.f) rc 0, 29 records ok, 0 stale; full suite ran 521 of 521, one red: pg (pg-kill-test REPORT-READ READY-MS 60 s expired under load, rc 67 -2502, the known case in habu-remove-the-test-6fc078db), which passes alone twice on the same candidate (5.3 s). Reviews: kinds-a, kinds-b, evalcell and the tip ACCEPT after folds."
---

Step 1 finding from kinds-a. TYPED-VARIABLE and DYNAMIC-BUFFER still throw -6616 under an arm64 shadow after create went through DEF-FIXED: src/core/layout-buffer.f LBUF-EVAL runs generated source through TYPE-DECL:TDECL-EVAL-XT = src/core/include.f INCLUDE-EVALUATE = the kernel's evaluate-closed, which on arm64 is the machine-code interpreter and never reads PROVIDED-XT:EVALUATE-CELL (layout.f PROVIDED-XT comment), so its colon compiles through tier 1, whose arm64 unplaced hook throws (src/arch/arm64/passes.f UNPLACED-UNSUPPORTED). INTERPRETER:INSTALL-EVALUATE in the test changes nothing. Design r2 section 5: the arm64 kernel reads EVALUATE-CELL, EVALUATE-RANGE-CELL, KEYWORD-CELL and CREATEP-CELL as x86-64's does (kernel-x64.f PROVIDED-ROW and its evaluate-closed row). Acceptance: shadow-kinds reports TYPED-VARIABLE and DYNAMIC-BUFFER ok. Blocks registering shadow-kinds.

Lane evalcell: habu1.f B-EVAL, B-EVAL-RANGE and B-EVAL-CLOSED dispatch through PROVIDED-XT cells when filled (zero keeps the machine-code body until the arm64 startup installs the typed loop); shadow-kinds installs INTERPRETER:INSTALL-EVALUATE. Workspace .jj-ws.noindex/jerry-evalcell on vslnprlw; brief /tmp/jerry-big/evalcell-brief.md. Needed before the cross-built application (step 5): its generated text must reach the typed loop.
