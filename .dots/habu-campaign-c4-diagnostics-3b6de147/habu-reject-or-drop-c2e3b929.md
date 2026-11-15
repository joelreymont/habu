---
title: Reject or drop the link data checks
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:15.552363+01:00"
closed-at: "2026-11-16T04:07:34.438497+01:00"
---

Problem: src/habu/link-data.f and link-copy.f end a build with 'link: ...' prose and status 76 or 74 (die). Acceptance: each site a build reaches writes one record with status 70; each no build reaches is deleted. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
