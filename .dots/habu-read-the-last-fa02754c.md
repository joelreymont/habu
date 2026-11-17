---
title: Read the last copy of a repeated MCP params member
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T23:03:12.093551+01:00"
---

Problem: src/lib/net/mcp.f:235 FIELD? reads the first copy of a member repeated in a request's params (such as arguments or name), while src/lib/json-rpc.f:229-234 states that the last repeat counts, as JSON.parse does; a client and the server then read different arguments. Acceptance: FIELD? answers the last copy; an mcp-test case with a repeated arguments member runs the body with the last one and fails before the fix. Depends: none. Claim: unassigned.
