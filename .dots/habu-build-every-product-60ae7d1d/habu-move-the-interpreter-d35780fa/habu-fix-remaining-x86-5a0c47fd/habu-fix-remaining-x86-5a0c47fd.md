---
title: Split the x86-64 failures left once the x86-64 tests run again
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.407093+03:00"
blocks:
  - habu-re-enable-the-959502b5
---

Problem: an old pulsar probe listed many x86-64 failures with several causes in this one dot; most of the tests it named are gone (test/outer-interpret.f in d4d9e6c6a4, tools/aot-code-map.f in 1d97c3f21c, and the rest of its list no longer exist as test files or suites), and the package-part exit 83 is fixed (habu-open-a-pkg-208310aa). The x86-64 tests are disabled until habu-re-enable-the-959502b5 re-enables them, so no current list exists. Still to check from the old list: one definition `create 99 , if exit then does>` made x86-64 drop the created word when `exit` precedes `does>` (E-UNDEFINED 'UNPATCHED', at tier 0 and tier 1); test/structure-opaque-e2e.f cases 3 and 5-7 (OWNER-XT); test/gate-pool.f case F40. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces. Master's arm64 behaviour and tests win: fix x86-64, never rewrite a master test.
Acceptance: from the first full x86-64 suite run after habu-re-enable-the-959502b5 lands, each failure that dot does not fix has its own dot under this one, one per root cause, with its reproducer; the three cases above are each filed or shown passing; then this dot closes.
Files: .dots only.
Verify: on pulsar, the x86-64 build and its full suite run; each failure in the run log maps to a filed dot.
Depends: habu-re-enable-the-959502b5.
Ownership: unassigned. Claim: unassigned.
