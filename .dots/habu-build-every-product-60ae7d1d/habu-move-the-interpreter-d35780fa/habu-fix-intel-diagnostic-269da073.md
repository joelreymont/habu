---
title: Fix Intel diagnostic positions
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T13:58:07.634973+00:00"
---

Problem: on x86-64, test/diag-position-test.f (a checker diagnostic must locate its token in the checked file; suite diag-position, test/gate-stdlib-cases.f:46) was reported failing seven of its cases, among them one that gives E-UNDEFINED where E-MISMATCH is required. The report did not name the cases, so which cases fail on master, and why, is unknown. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix it in the code that stays, never in the machinery the parent replaces.
Acceptance: first, the failing cases on master's x86-64 engine are listed in this dot, each with its cause at file:line; then, with each fixed in the responsible code, test/diag-position-test.f unchanged exits 0 on x86-64 and still on arm64. Master's arm64 behaviour and tests win: fix x86-64, never rewrite a master test.
Depends: none.
Claim: unassigned.
