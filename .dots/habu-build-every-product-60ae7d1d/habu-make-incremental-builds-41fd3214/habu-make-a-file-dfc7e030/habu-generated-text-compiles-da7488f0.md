---
title: Generated text compiles through the typed loop on arm64
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T14:14:14.895152+02:00"
---

Step 1 finding from kinds-a. TYPED-VARIABLE and DYNAMIC-BUFFER still throw -6616 under an arm64 shadow after create went through DEF-FIXED: src/core/layout-buffer.f LBUF-EVAL runs generated source through TYPE-DECL:TDECL-EVAL-XT = src/core/include.f INCLUDE-EVALUATE = the kernel's evaluate-closed, which on arm64 is the machine-code interpreter and never reads PROVIDED-XT:EVALUATE-CELL (layout.f PROVIDED-XT comment), so its colon compiles through tier 1, whose arm64 unplaced hook throws (src/arch/arm64/passes.f UNPLACED-UNSUPPORTED). OUTER:INSTALL-EVALUATE in the test changes nothing. Design r2 section 5: the arm64 kernel reads EVALUATE-CELL, EVALUATE-RANGE-CELL, KEYWORD-CELL and CREATEP-CELL as x86-64's does (kernel-x64.f PROVIDED-ROW and its evaluate-closed row). Acceptance: shadow-kinds reports TYPED-VARIABLE and DYNAMIC-BUFFER ok. Blocks registering shadow-kinds.
