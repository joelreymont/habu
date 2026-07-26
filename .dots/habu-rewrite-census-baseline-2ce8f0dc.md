---
title: Rewrite census baseline provenance comment
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T11:00:15.089719+02:00"
---

Program-level wording fix found by the A2 migration pathfinder: tools/enum-census-core.f lines 15-18 state the shipped baseline was recorded on the commit BEFORE the ENUM keyword moved. Every migration lane that re-records tools/enum-census-baseline.txt makes that sentence less true (new rows have no pre-cutover counterpart), and about fifty more lanes are coming. Behavior: rewrite the provenance comment to describe the baseline as a rolling before/after parity artifact re-recorded consciously by any change that adds, removes, or reshapes an ENUM site, with the review rule that a re-record must be accompanied by the exact expected row delta in the change's report. One wording fix, not fifty nibbles. Owner: tools/enum-census-core.f header prose. Acceptance: comment matches reality; enum-census suite untouched and green. Dependencies: none.
