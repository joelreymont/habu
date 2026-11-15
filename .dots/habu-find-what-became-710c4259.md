---
title: "Find what became of two approved items: the check tool's number-name refusal and T0's callback request words"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:35:39.698331+02:00"
---

Problem: two rulings name work found in neither src nor a dot. (1) 2026-11-04: the check tool's work, the number-name refusal's second half included (tools/check.f refusing ': 42' and ': if' as loading does), waits until incremental builds are done; SOURCE-SYNTAX:COMPILE-KEYWORD? was not approved. (2) 2026-11-03: Joel named NDEFINITION:CALLBACK-REQUEST@ ( -- n n ) and CALLBACK-ENTRY! ( n -- ) for T0's callback request; neither word nor COMP-SHADOW-CELL exists, and CALLBACK-STUB@ is still in NDEFINITION (src/compiler/native/definition.f:21). Acceptance: each is traced to landed code, an open dot, or a ruling that replaced it; what is still owed gets its own dot. Files: tools/check.f, src/compiler/native/definition.f. Verify: path:line or dot id per item. Depends: none. Ownership: unassigned. Claim: unassigned.
