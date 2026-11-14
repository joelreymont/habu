---
title: Wait for a key fetch already in flight
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T17:21:59.738652+01:00"
---

Problem: src/lib/net/bearer.f LOOKUP sends a worker that arrives while another's key-set fetch is in flight to answer from the set as it stands: an empty 503 on a fresh server, a 401 invalid_token after a key rotation. Zen, Tender and Maki on spark meet it (Codex drops its MCP server after the empty 503; /tmp/bob-bearer-503.md). Acceptance: a valid token that arrives during a fetch waits for that fetch and is admitted by the set it returns; bearer-test.f shows both cases. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-bearer.
