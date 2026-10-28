---
title: Bound the lsp and prefix-provided rows by CPU
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:39:18.138408+02:00"
---

Problem: in the full gate at load average ~90, lsp-boundary and prefix-provided-unfilled report TIMEOUT-UNDER-LOAD and pass alone (prefix-provided-unfilled in 334 s). Rows are CPU-budgeted (test/suite-budget.f, closed dot habu-bound-long-suite-6d93453d), but these rows keep inner wall deadlines: tools/lsp-test-lib.f:620-621 CONVERSATION-MS 8 s and CHECKED-MS 60 s, test/prefix-provided-unfilled.f:29,88 BUILD-TIMEOUT-MS 600 s. Acceptance: each inner deadline is bounded by CPU use or by the row's hang guard, so load alone cannot fail the row; a saturated gate runs both rows to PASS. Files: tools/lsp-test-lib.f, test/prefix-provided-unfilled.f, test/suite-budget.f. Verify: the two rows under a saturated host. Depends: none. Ownership: test gate. Claim: unassigned.
