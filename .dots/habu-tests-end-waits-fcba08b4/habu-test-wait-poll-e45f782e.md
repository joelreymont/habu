---
title: "TEST-WAIT:POLL waits with no deadline"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:22.975268+01:00"
closed-at: "2026-11-13T21:29:13.978644+01:00"
---

Problem: TEST-WAIT:POLL (src/lib/test/wait.f) takes a deadline and name, and every caller (17 files) ends its wait by it. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wP.
