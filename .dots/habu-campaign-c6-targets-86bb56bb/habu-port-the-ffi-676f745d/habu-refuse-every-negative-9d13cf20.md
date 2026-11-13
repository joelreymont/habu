---
title: "Refuse every negative wid in x86-64's record writers"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T01:29:13.866893+02:00"
---

Problem: src/habu/prims.f's def-open and alias-record comments (:852, :879 after L4 lands) say the writers refuse a negative wid. arm64 does: habu2.f REAL-WID, refuses wid < 0 (2 0 CMPI, C-LT). x86-64 does not: kernel-x64.f:5037 REAL-WID, refuses only WL:NAMESPACE and WL:RETIRED, so on x86-64 def-open and alias-record accept any other negative wid. A machine asymmetry (CLAUDE.md: a split to remove). Found by lane F's review of L4 (INTERPRETER:ALIAS-RECORD, PACKAGE-RECORD). Acceptance: one rule on both machines, a negative wid refused by def-open and alias-record, by the shared layer once x86-64 moves onto the one typed interpreter and linker, not by an x86 patch. Files: src/habu/kernel-x64.f REAL-WID, (or its replacement). Verify: a checked probe calling INTERPRETER:ALIAS-RECORD with wid -5 refused on x86-64 as on arm64. Depends: x86-64 work resumes only after arm64-to-arm64 works (Joel 2026-10-29). Ownership: unassigned. Claim: unassigned.
