---
title: Serve a kept-alive request with fewer thread switches
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:14:37.845448+01:00"
---

Problem: an HTTP server built on src/lib/net/http.f costs about 9 context switches per kept-alive request on both arm64 and x86-64 (Zen's server, measured 2026-11-26); where a switch costs 24 us of kernel time (WSL2 x86-64) Zen and Maki serve kept-alive GETs about 2x slower than on arm64. Acceptance: the switches per kept-alive request are attributed to their handoffs (path:line), the avoidable ones are gone, and a kept-alive GET loop shows the drop on both targets. Depends: none. Claim: unassigned.
