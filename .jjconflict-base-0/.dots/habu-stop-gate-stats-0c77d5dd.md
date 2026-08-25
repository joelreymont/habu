---
title: Stop gate-stats echoing foreign capture paths
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T08:53:02.863477+02:00"
---

A gate run's gate-stats.tsv recorded process-exec rows pointing into another lane's old ln-gate capture root — the result cache or stats writer echoes stale paths across HB_TMP roots. Attribute and fix in the stats writer; misattributed stats will eventually misroute a debugging session. Found by the refuse-bisect lane 2026-08-06.
