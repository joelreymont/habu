---
title: Win back short counted loops on Cortex-X925
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:28:51.610345+01:00"
---

Problem: holding the loop stack's address (LOOPSP-CELL) made 0 n 0 do i + loop about 12% slower at n=1 and 10% at n=2 on a Cortex-X925, and up to 28% slower at n=20 to 40 by placement (spark cpus 9 and 15, bare assembly of the two forms agrees), while n=80 to 400 and the Cortex-A725 gained; the microarchitectural cause is unknown. Acceptance: the cause is named from counters, and a nested loop on an X925 is no slower than with the counted form at any n from 1 to 1000. Depends: perf counters on spark. Claim: unassigned.
