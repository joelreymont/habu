---
title: TCP and UDP tests end their waits by a clock
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.014591+01:00"
closed-at: "2026-11-13T21:29:13.847406+01:00"
---

Problem: TCP4:READABLE-WITHIN?, PENDING-WITHIN? and UDP4:RECEIVE have no no-deadline value; tcp4-test.f, udp4-test.f and test/http-accept-starved.f end waits by them. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wTCP.
