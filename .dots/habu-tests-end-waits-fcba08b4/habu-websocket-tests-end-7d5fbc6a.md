---
title: WebSocket tests end their waits by a clock
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.061049+01:00"
closed-at: "2026-11-13T21:29:13.999716+01:00"
---

Problem: src/lib/net/ws-test.f ends waits and fixture holds by deadlines. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wWS.
