---
title: "A tool body's fault is answered -32603, not passed to the host"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T12:21:39.179931+01:00"
blocks:
  - habu-mcp-log-s-53cf3b52
---

Problem: src/lib/net/mcp.f FAULTED, BODY-FAULT and REPLY-HELD pass any code but an early E-INPUT to the host, so a tool body's fault gets no JSON-RPC response: HANDLE's request is answered by HTTP as a plain-text 500 (streamable HTTP requires a JSON-RPC response), and ANSWER's throw closes a view's WebSocket with 1011, dropping every call in flight on it (tim 2026-11-25 for Maki, /tmp/tim-habu-mcp-fault.md). Acceptance: every message HANDLE, ANSWER or FINISH takes that throws is answered with the JSON-RPC error -32603 whose message is the code with its declared name and text, on the same connection or vector; its log line is written once at error; HANDLE, ANSWER and FINISH no longer rethrow it; the mcp suite asserts it for HANDLE, ANSWER and a held call. Depends: habu-mcp-log-s-53cf3b52. Claim: unassigned.
