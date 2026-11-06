---
title: "Log MCP's refused and malformed replies"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:06:36.209928+02:00"
blocks:
  - habu-log-mcp-s-61343cb4
---

Problem: after the netlog lane, MCP's E-DENIED, E-RESPONSE and malformed-reply throws (src/lib/net/mcp.f:1482-1552 at review time) still leave no log line, while transfer failures are logged; Joel 2026-10-31: 'make sure model library logs failures, errors, etc. dumping the textual reason'. Acceptance: each of those throw sites writes one LOG:ERROR line naming the status or what was malformed, with no URL userinfo or query (HOST$'s rule); mcp-test.f checks one line per case through STDERR:CAPTURE. Files: src/lib/net/mcp.f, mcp-test.f. Verify: src/lib/net/mcp-test.f. Depends: netlog lane, outage lane (both touch RESULT$). Ownership: carl, worker lane. Claim: unassigned.
