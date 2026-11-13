---
title: Test host.js rpc in Chrome against mcp.f
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T18:28:14.842766+02:00"
---

Problem: no test runs host.js's rpc turn record (record 10, answered by event 3) in Chrome against src/lib/mcp.f (H2a review F6); host-cli's --answer-rpc/--fail-rpc cover it only outside a browser. Acceptance: one case in an existing browser suite, run only with --chrome (Joel 2026-10-31), sends an rpc from a module page through host.js to an mcp.f server and checks the answer and a failure; it fails with host.js's rpc dispatch removed. Files: an existing browser suite. Verify: that suite with --chrome. Depends: H2 lands. Ownership: unassigned. Claim: unassigned.
