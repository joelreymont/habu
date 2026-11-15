---
title: Serial tests end their waits by a clock
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.001801+01:00"
closed-at: "2026-11-13T21:29:13.823487+01:00"
---

Problem: SERIAL:READ and SERIAL:WRITE have no no-deadline value; src/lib/serial-test.f ends its waits by them. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wSERIAL.
