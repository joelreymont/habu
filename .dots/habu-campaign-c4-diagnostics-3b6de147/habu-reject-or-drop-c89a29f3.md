---
title: Reject or drop the link band and map checks
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:15.572571+01:00"
closed-at: "2026-11-16T04:07:34.426880+01:00"
---

Problem: src/habu/link-band.f and link-map.f end a build with 'link: ...' prose and status 76 (die). Acceptance: each site a build reaches writes one record with status 70; each no build reaches is deleted. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
