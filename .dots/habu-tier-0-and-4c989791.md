---
title: "Tier 0 and tier 1 disagree on a ( c ) comment inside {: :}"
status: open
priority: 3
issue-type: task
created-at: "2026-10-25T08:43:30.941258+01:00"
---

{: a ( c ) b :} stops tier 0 with 'interpret stack underdepth' while tier 1 runs the definition and prints 7. Decide from the checker's local-declaration rules which tier is wrong and fix that tier. Found by a jerry lane review, 2026-10-24. Claim: unassigned.
