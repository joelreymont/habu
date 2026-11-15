---
title: "Index the harvest's records by file and offset"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:40:21.628417+01:00"
closed-at: "2026-11-16T04:07:35.801818+01:00"
---

Problem: tools/source-discovery.f:256 SD-R-AT walks the file's record chain per declaration, so check.f's harvest is quadratic: 105 s on a 109644-definition file, during which check.f defers SIGTERM (hunt W85). Acceptance: SD-R-AT answers in constant time; check.f on /tmp/jerry-c4/probes/s109644.f ends in seconds. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
