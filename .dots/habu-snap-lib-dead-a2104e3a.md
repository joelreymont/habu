---
title: snap-lib dead-heap zeroing is now inert
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T22:34:33.320586+02:00"
---

single-prefix-2 (2026-08-17): the 4.53MB orphan the zeroing existed for is gone (single prefix load); its subject IMK-NDICT0 now sits at DATA-START and the zeroed span is empty; the residual post-mark orphan is 12,454B and uncovered. Re-aim the zeroing at the real residual span or retire it - decide by measuring what a snapshot actually carries of that 12KB.
