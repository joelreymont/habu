---
title: Reclaim rewound startup code in a cold-loaded image
status: open
priority: 3
issue-type: task
created-at: "2026-10-29T17:56:53.148828+02:00"
---

COLD-LOAD:RUN rewinds names to CORE-PREFIX:FIRST-RECORD but keeps the old startup code and data, so a cold-loaded support image grows by ~3.5 MB of dead code (B1-L2 child: 15,037,424 bytes without cold-load, 18,534,032 with, cold-load before 1 set-tier). Decide with L4's support-cache measurement whether the rewind should also reclaim space, and what still points into the old code.
