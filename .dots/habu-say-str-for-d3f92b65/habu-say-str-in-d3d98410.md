---
title: "Say str in MCP's and model's public rows"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T23:12:49.075059+01:00"
---

Problem: src/lib/net/mcp.f keeps text in span<u8> storage (SERVER-NAME, SERVER-VERSION, INSTRUCTIONS, TOOL-NAME, TOOL-TEXT, TOOL-OUTPUT, RESOURCE-URI, RESOURCE-TEXT, RESOURCE-PAGE, ID, METHOD, PARAMS, TEXT) through its private >SPAN (:227) and reads it back with SPAN:$, though a TYPED-VARIABLE or TYPED-BUFFER holds a str. Acceptance: each such field holds a str; >SPAN and its SPAN:$ reads are gone; the mcp suite passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-mcpfields.
