---
title: MCP server does not check tools/call arguments against inputSchema
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:09:21.968034+02:00"
---

From phil (Zen), 2026-11-05. Problem: src/lib/net/mcp.f:684 RUN-TOOL hands the tool body its arguments without checking them against the inputSchema ADD-TOOL stored (:923 reads only that it is an object); MCP says a server should validate tool inputs, so each tool re-checks or trusts the client. Needs a design: what part of JSON Schema a Habu MCP server checks (Zen's and Maki's schemas are the evidence), where the checker lives (JSON package or MCP), and the error a refused call answers (-32602 Invalid params). New public words go to Joel. Files: src/lib/net/mcp.f, src/lib/json*.f. Verify: mcp-test.f. Depends: none. Ownership: carl. Claim: unassigned.
