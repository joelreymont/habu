---
title: Fold the L1-fold review
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T19:58:44.710394+03:00"
closed-at: "2026-10-28T17:13:33.318540+02:00"
close-reason: landed 48953455 (data-rewound refusals, literal-pool growth refused); frame e2e pool-full stage and data-rewound rows gated
---

K1 capture refuses a segment reopening below the previous SEGMENT end (data-rewound); K2 POOL-FULL asserts the miss line; K3 LIT-ROW bounds literal rows by pool capacity; K4 POOL-FULL build cost; K5 redundant public. Brief brief-l1-fold2.md; workspace carl-l1 (worker running).
