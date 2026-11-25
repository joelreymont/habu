---
title: "MCP:LOG's line gives the message's milliseconds"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T12:11:17.797655+01:00"
---

Problem: MCP:LOG's info line (src/lib/net/mcp.f MESSAGE-LOGGED, HELD-LOGGED) is `req <id> <method> <target> -> <outcome>` with no time, so a message MCP:ANSWER answers shows no latency anywhere (tim 2026-11-25 for Maki). Acceptance: the line ends ` in <n> ms`: HANDLE from its request's start, ANSWER from its entry, a held call from its request's start to FINISH; the header says so; the mcp suite asserts it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-mcptime.
