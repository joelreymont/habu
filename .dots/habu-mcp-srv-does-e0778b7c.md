---
title: MCP server does not check tools/call arguments against inputSchema
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:09:21.968034+02:00"
---

Problem (from phil, Zen, 2026-11-05): src/lib/net/mcp.f:691 RUN-TOOL hands the tool body its arguments without checking them against the inputSchema ADD-TOOL stored (:930 reads only that it is an object); MCP says a server should validate tool inputs, so each tool re-checks or trusts the client. Needs a design: what part of JSON Schema a Habu MCP server checks (Zen's and Maki's schemas are the evidence), where the checker lives (JSON package or MCP), and the error a refused call answers (-32602 Invalid params). New public words go to Joel. Depends: none. Claim: unassigned, owner carl.
