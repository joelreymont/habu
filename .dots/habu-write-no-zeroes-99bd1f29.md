---
title: Write no zeroes into fresh checker storage
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T19:08:36.705460+01:00"
---

Problem: a trivial program's checker storage peaks at 3.72 MB, 1.27 MB of it resident pages that hold only zeroes (TFAM and TYPE-DECL arenas, SR chunks, HIDX-ALLOC, TV-GROW), measured from /proc/PID/pagemap at exit. Acceptance: no checker page is resident that holds only zeroes, each site named with its resident bytes before and after. Depends: none.
