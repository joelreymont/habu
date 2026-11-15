---
title: Reject an ended built local read in a text-finished body
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:02:34.771090+01:00"
closed-at: "2026-11-16T04:07:34.628486+01:00"
---

Problem: `BITEM:ENDED-TEXT-GO 1 + ;` (test/build-items.f:541) prints error[E-UNDEFINED] cannot find word `l0` with help to define `l0`, where the built local's block ended; master printed compiler: cannot compile BI-ENDED-TEXT at l0. Acceptance: the record names the ended built local, as BITEM:ENDED-GO's does; build-items passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
