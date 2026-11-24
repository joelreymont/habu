---
title: "Name the x86 code reserve's owner"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T07:57:02.103613+01:00"
---

Problem: src/habu/kernel-x64.f:2977 says 'CODE-RESERVE absorbs the rest', but no word CODE-RESERVE exists (CODE-SPACE:RESERVE in src/core/checker.f owns the arm64 reserve; src/habu/layout.f:56 holds REGION $4000 -). Acceptance: the comment names what absorbs the rest on x86-64, true of the code, when x86-64 work resumes. Depends: none. Claim: unassigned.
