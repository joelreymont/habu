---
title: Set the log level from HABU_LOG_LEVEL
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T11:31:43.564238+01:00"
---

Problem: LOG (src/lib/log.f) writes every level and has no debug; MCP:LOG writes tool arguments, which carry staff queries and document text, on its info line (src/lib/net/mcp.f MESSAGE-LOGGED). Joel 2026-11-24 asked that the log level be settable like other logging packages. Acceptance: LOG:DEBUG; HABU_LOG_LEVEL (error/warn/info/debug, unset or empty info, a bad value one warn line) read once per process; a LINE above it writes nothing; MCP:LOG's info line is req <id> <method> <target> -> <outcome> and the arguments go to a debug line; log and mcp suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-log.
