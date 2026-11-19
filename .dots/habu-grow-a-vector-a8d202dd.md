---
title: Grow a vector through one VEC word
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T17:35:44.894102+01:00"
---

Problem: three private copies grow a vector's count by n and answer the new bytes: src/lib/net/bearer.f:850 GROWN, src/lib/json/write.f:167 JW-TAKE, and mcp.f GROWN in kevin's writer change lpvnyyot. Acceptance: one public VEC:EXTEND ( ^vector<t> n -- SPAN:span<t> ) in src/lib/vector.f replaces all three; the bearer, json-write and mcp suites pass. Depends: lpvnyyot on master. Claim: unassigned.
