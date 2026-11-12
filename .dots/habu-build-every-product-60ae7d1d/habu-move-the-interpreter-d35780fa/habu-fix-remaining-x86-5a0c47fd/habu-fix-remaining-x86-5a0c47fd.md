---
title: Split the x86-64 failures left once the x86-64 tests run again
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.407093+03:00"
blocks:
  - habu-re-enable-the-959502b5
---

Problem: the x86-64 tests are disabled until habu-re-enable-the-959502b5 re-enables them, so no current list of x86-64 failures exists. Still to check from an old list: one definition `create 99 , if exit then does>` made x86-64 drop the created word when `exit` precedes `does>` (E-UNDEFINED 'UNPATCHED', at tier 0 and tier 1). It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces. Master's arm64 behaviour and tests win: fix x86-64, never rewrite a master test.
Acceptance: from the first full x86-64 suite run after habu-re-enable-the-959502b5 lands, each failure that dot does not fix has its own dot under this one, one per root cause, with its reproducer; the case above is filed or shown passing; then this dot closes.
Depends: habu-re-enable-the-959502b5.
Claim: unassigned.
