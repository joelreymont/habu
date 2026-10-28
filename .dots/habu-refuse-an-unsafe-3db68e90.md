---
title: Refuse an unsafe token after an earlier failure
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:39:18.092336+02:00"
---

Problem: REJECT-UNSAFE sets UNSAFE only while FAILSET is 0 (src/core/checker.f:21975-21977), and CHECK-VERDICT answers 0 only for UNSAFE and the other hard flags, else 1 when UNCK is set (:25386-25391). So a body whose earlier token set UNCK and pinned the failure, then names an unsafe token ('rwx-nowhere evaluate'), keeps verdict 1 (uncheckable, deferred to the run) instead of 0. tools/check.f still rejects that probe as E-UNDEFINED (/tmp/tim-dots/f15.f), so the hole is the verdict a verdict-1 consumer acts on. Acceptance: an unsafe token forces verdict 0 whatever pinned the first failure, keeping the first failure's diagnostic; a row with an uncheckable token before evaluate. Files: src/core/checker.f, a checker suite named by test/gate-stdlib-cases.f. Verify: the row. Depends: none. Ownership: checker. Claim: unassigned.
