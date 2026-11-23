---
title: Grow a vector through one VEC word
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T17:35:44.894102+01:00"
---

Problem: three private copies grow a vector's count by n and answer the new bytes: src/lib/net/bearer.f:863 GROWN, src/lib/json/write.f:155 JW-TAKE and src/lib/net/mcp.f:1763 GROWN. Acceptance: one public VEC:EXTEND ( ^vector<t> n -- SPAN:span<t> ) in src/lib/vector.f replaces all three; the bearer, json-write and mcp suites pass. Depends: none. Claim: unassigned.
