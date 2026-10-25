---
title: Tier 0 and tier 1 disagree on a hookless does> clause the checker refuses
status: open
priority: 3
issue-type: task
created-at: "2026-10-25T08:43:30.948179+01:00"
---

A hookless does> clause whose signature the checker refuses runs at tier 0 (prints 7 2 1, after 6e61d5f7) and is refused at tier 1; tier 1 also prints the checker's report for hookless bodies where tier 0 prints none. One semantics for both: decide from the checker's rules whether the refusal or the run is right and fix the other tier. Found by the 3b8f48e2 lane review, 2026-10-25. Claim: unassigned.
