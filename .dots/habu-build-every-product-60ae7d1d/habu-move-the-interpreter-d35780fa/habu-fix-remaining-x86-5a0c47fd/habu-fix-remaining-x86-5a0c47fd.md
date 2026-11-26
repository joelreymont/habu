---
title: Split the x86-64 failures left once the x86-64 tests run again
status: closed
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.407093+03:00"
closed-at: "2026-11-26T13:52:41.487850+01:00"
---

Problem: no current list of x86-64 failures exists. Still to check from an old list: one definition `create 99 , if exit then does>` made x86-64 drop the created word when `exit` precedes `does>` (E-UNDEFINED 'UNPATCHED', at tier 0 and tier 1). It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces. Master's arm64 behaviour and tests win: fix x86-64, never rewrite a master test.
Acceptance: from a full x86-64 suite run, each failure has its own dot under this one, one per root cause, with its reproducer; the case above is filed or shown passing; then this dot closes.
Depends: none.
Claim: unassigned.
