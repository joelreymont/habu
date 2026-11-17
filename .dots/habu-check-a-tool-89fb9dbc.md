---
title: "Check a tool call's arguments against its input schema"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T22:43:20.700279+01:00"
---

Problem: src/lib/net/mcp.f:1187 ADD-TOOL checks only that inputSchema is an object and RUN-TOOL (mcp.f:840) hands the arguments to the body as sent, so a misspelt optional argument is ignored in silence (mark's request, /tmp/mark-carl-mcp-input.md). Acceptance: a tools/call whose arguments break the tool's inputSchema is answered as a tool error naming the member and the rule before the body runs; every keyword a registered input schema states is checked; a schema ADD-TOOL rejects, and every other E-SCHEMA, leaves with its text through REASON:THROW. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-mcpin-l1 .jj-ws.noindex/carl-mcpin-l2.
