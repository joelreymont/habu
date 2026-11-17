---
title: "Keep reader texts in Habu's JSON and XML callers"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T17:00:50.969960+01:00"
---

Problem: src/lib/net/model.f:1532-1535 CHECK-OBJECT and src/lib/net/mcp.f:967-969 SCHEMA-CODE turn a reader code into their own and drop its text; json-rpc.f:91-96, bearer.f:146-150, test-issuer.f:204-207 and tools/lsp-core.f:370 do not pass REASON:TEXT$ on; tools/gate-json-assert-core.f:84's E-JSON-READ-TRAILING branch can never run (STEP answers T-END or throws TRAILING, read.f:657-662). Acceptance: each logs or answers subject + REASON:TEXT$ + LOG:CODE+ (design R 3.1), the dead branch is gone. Depends: habu-say-where-the-52309279. Claim: unassigned.
