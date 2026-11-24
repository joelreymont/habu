---
title: "Hold a JSON-RPC message's parts as values"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.406603+01:00"
---

Problem: JSON-RPC's message stores ^u8/n pairs and >MESSAGE walks a cursor; Maki keeps a cursor table to feed it. Acceptance: message parts are JSON:value (params option), id, ID$ and METHOD? deleted, >MESSAGE parses once; mcp.f and tools/lsp-core.f follow. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-jrpc.
