---
title: "Tier 0 and tier 1 disagree on a ( c ) comment inside {: :}"
status: closed
priority: 3
issue-type: task
created-at: "2026-10-25T08:43:30.941258+01:00"
closed-at: "2026-11-10T21:52:35.329761+02:00"
close-reason: "done: `{: a ( c ) b :}` prints 7 at tier 0 and tier 1, rc 0, on master's engine"
---

{: a ( c ) b :} stops tier 0 with 'interpret stack underdepth' while tier 1 runs the definition and prints 7. Decide from the checker's local-declaration rules which tier is wrong and fix that tier. Found by a jerry lane review, 2026-10-24. Claim: unassigned.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.
