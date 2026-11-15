---
title: "Log MCP's and BEARER's remaining outside failures"
status: open
priority: 3
issue-type: task
created-at: "2026-11-06T13:06:47.381226+02:00"
---

Problem: the review of kpmxnwny (dot habu-log-mcp-s-61343cb4) found outside failures that still reach the caller with no line naming their cause, against card section 6: MCP's RESULT$ throws E-DENIED or E-RESPONSE for a 401, 403 or other non-2xx status with no line naming the status (src/lib/net/mcp.f:1482-1483); a JSON-RPC error answer throws silently (:1492); LEFT throws E-TRANSFER silently when time runs out between pages (:1516); BEARER's key-set fetch (FETCHED-LEN/TRANSFER/FETCH, src/lib/net/bearer.f:268-310) drops CURL:INIT's code (:305) and an unreachable, non-200 or truncated key endpoint becomes ADMIT?'s 503 or a 401 with no line (one line per failed fetch is bounded by REFETCH-MS, :44); a broken issuer key (bearer.f:547, through CRYPTO:RS256-VERIFY? and RS-JWK-CHECK, evp.f:834-842) shows only the HTTP server's generic throw line. ADMIT?'s 401s are the requester's input and stay silent. Acceptance: one line per failure naming the status, curl code or cause, never a secret (no URL userinfo or query, no token, no form); tests capture stderr for each. Files: src/lib/net/mcp.f, src/lib/net/bearer.f, their tests. Verify: mcp-test, bearer-test, test-issuer-test. Depends: habu-log-mcp-s-61343cb4. Ownership: carl. Claim: unassigned.


Merged from habu-log-mcp-s-bdb6b296 ("Log MCP's page-time-limit failure"), 2026-11-10:

Problem: src/lib/net/mcp.f LEFT (:1580-1585 at lane carl-mcplog konxtkpq) throws E-TRANSFER with no log line when TOOLS's pages use up the call's time limit, while curl's own timeout logs 'curl 28' through CURL-FAULT; Joel 2026-10-31 'make sure model library logs failures, errors, etc. dumping the textual reason'. Found by the review of konxtkpq. Acceptance: LEFT's throw writes one LOG:ERROR mcp line naming the time limit before E-TRANSFER; mcp-test.f's CLIENT-LIST-LATE-PAGE captures and checks it. Files: src/lib/net/mcp.f, src/lib/net/mcp-test.f. Verify: src/lib/net/mcp-test.f. Depends: habu-log-mcp-s-39c999fa. Ownership: those files. Claim: unassigned.
