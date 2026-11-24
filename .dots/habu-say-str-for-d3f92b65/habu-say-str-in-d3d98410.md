---
title: "Say str in MCP's and model's public rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:12:49.075059+01:00"
---

Problem: Census: /tmp/jerry-p1/rows/pubrows.py (public rows holding ^u8 n or ^u8 NUM:byte-len). src/lib/net/mcp.f INSTRUCTIONS!, VISIBLE!, ADD-TOOL, ADD-RESOURCE, RESULT, RESULT-META, RESULT-CONTENT, RESULT-ERROR, OPEN, TOKEN!, TOOLS, CALL and src/lib/net/model.f TEXT+, INPUT+, INCOMPLETE-REASON$, CONTENT$, CHAT, REPLAY, VECTOR@ take or give text as ^u8 n. Acceptance: each takes and gives str; callers in Habu move with it; consumer owners are told before the install. Depends: the split-str batches holding mcp.f and model.f. Claim: unassigned.
