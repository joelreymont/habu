---
title: "One word for a data row that does not fit a step's input"
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T19:37:55.938945+02:00"
---

Problem: the rule 'a failed data-row unify names the mismatch while the pin is open' is written out at CHECKER-STEP-UNJ, RS->R and RS2->R in src/core/checker.f, with variants at two more sites; the defect of habu-a-read-of-a035fab4's second shape was one site that never had the line. The return row has one shared word per unify kind (RSUNI-JOIN, RSUNI-IN, RSUNI-COERCE). Seen by the review of 'Name the mismatch when >r or 2>r does not fit' (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/to-r-review.md, Low, with a suggested word STEP-FIT ( bool n n -- ), unbuilt). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: one word holds the capture and the OK update and each site calls it; no packet changes. Files: src/core/checker.f. Verify: test/diag-position-test.f and test/compiler/input-underflow-refusals.f rc 0; the packets of the review's probes unchanged. Depends: habu-a-read-of-a035fab4 landed. Ownership: the capture of a failed data-row unify. Claim: unassigned.
