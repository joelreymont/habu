---
title: "The pre-pass passes a generates: row after a statement that may read source"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:21:52.900453+02:00"
---

Problem: a `generates:` row that sits after a statement whose run may read source (a defer set to a parsing word) is not checked by --verify-only, which exits 0; the row is reported only by the run, rc 67, in a JSON packet with no line or column. Master does the same for `generates: NOWHERE`. Found by the worker of habu-a-generates-row-fea48eb4 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/genrow-done.md, Outside this change); its review rules on whether it is so. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line why the pre-pass skips the row and why the run's packet has no place; then the row is refused with its place in every mode or the documented deferral says why not, and a test pins it. Files: src/habu/verify-source.f, src/core/checker.f. Verify: the worker's case in three modes. Depends: none. Ownership: the source pre-pass. Claim: unassigned.
