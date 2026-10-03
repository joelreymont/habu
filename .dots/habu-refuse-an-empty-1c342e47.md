---
title: Refuse an empty required file under --all-errors
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:18:22.306570+03:00"
---

check.f --all-errors on a subject that requires an empty file exits 67 with an uncaught -3200 (E-MEM-SIZE from CA-READ-SOURCE asking for 0 bytes); --load accepts the empty file. Found by the Fable review pdup-rev6 while landing 1ca23983.
