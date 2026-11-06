---
title: "Log MCP's page-time-limit failure"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T16:57:03.712757+02:00"
---

Problem: src/lib/net/mcp.f LEFT (:1573-1578 at lane carl-mcplog konxtkpq) throws E-TRANSFER with no log line when TOOLS's pages use up the call's time limit, while curl's own timeout logs 'curl 28' through CURL-FAULT; Joel 2026-10-31 'make sure model library logs failures, errors, etc. dumping the textual reason'. Found by the review of konxtkpq. Acceptance: LEFT's throw writes one LOG:ERROR mcp line naming the time limit before E-TRANSFER; mcp-test.f's CLIENT-LIST-LATE-PAGE captures and checks it. Files: src/lib/net/mcp.f, src/lib/net/mcp-test.f. Verify: src/lib/net/mcp-test.f. Depends: habu-log-mcp-s-39c999fa. Ownership: those files. Claim: unassigned.
