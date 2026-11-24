---
title: "Send a tool's result as structuredContent alone"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-24T16:26:59.959749+01:00"
closed-at: "2026-11-24T16:36:18.613385+01:00"
---

Problem: CALL-BODY (src/lib/net/mcp.f:487) writes every tool result's JSON twice, as structuredContent and escaped as content's text block; ChatGPT answers carry each record twice (mark measured 1.6-1.9x the bytes SAP sent). Joel 2026-11-24: MCP:RESULT sends only structured content. Acceptance: RESULT and RESULT-META answer content [] plus structuredContent; RESULT-CONTENT's content is exactly its blocks; RESULT-ERROR unchanged; the mcp suite passes. Depends: none. Claim: carl workspace=.jj-ws.noindex/carl-mcpsc.
