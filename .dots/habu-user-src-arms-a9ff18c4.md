---
title: "User source arms the field-projection window through TYPE-DECL:FIELD-ARM"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T20:13:39.539937+02:00"
---

Problem: TYPE-DECL:FIELD-ARM (src/core/sumtype.f:1288), which arms the field-projection window ARENA uses (src/core/arena.f:59), compiles and runs from user source, rc 0; the 'trusted-only effect' its comment names (src/core/checker.f:3257-3259) does not refuse it. Not carried to a forged pointer. TYPE-DECL also reopens from user source ('package TYPE-DECL' rc 0), which reaches its private words bare. Found by the storage-window design (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/storage-window-design.md, section 0 items 1 and 5). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: user source can neither call FIELD-ARM nor reopen TYPE-DECL (E-UNDEFINED or E-SEALED-PACKAGE), and ARENA still builds. Files: src/core/sumtype.f, src/core/arena.f, src/core/checker.f. Verify: both programs as refused cases in test/program-diagnostics-test.f, failing before. Depends: the design of habu-user-src-arms-bc2ab900 (the same seal). Ownership: TYPE-DECL's seal and FIELD-ARM. Claim: unassigned.
