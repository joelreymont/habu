---
title: Reject a JSON-RPC message that repeats a member
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T11:11:48.045999+01:00"
---

Problem: json-rpc.f's header says the last of a repeated member counts, MCP's FIELD? takes the first (phil, /tmp/phil-json-parse-cost.md item 2), and JSON:PARSE rejects any repeat: three readers of one text disagree. Acceptance: JSON-RPC:>MESSAGE answers INVALID-REQUEST for a body that repeats a member name (decoded) in any object, its header says so, and no Habu reader of a JSON-RPC body picks a copy. Depends: the cursorless JSON-RPC lane (carl-jrpc) lands first. Claim: agent=carl workspace=.jj-ws.noindex/carl-jrpc.
