---
title: "Keep the token buffers' bytes when they grow"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T10:10:02.640280+01:00"
---

Problem: src/core/checker.f TOKBUF-GROW maps fresh FAILTK, TKF, NMB and SDEC buffers without copying, so a TOKBUF-ENSURE mid-scan wipes the definition's name: a built local of a 4550-byte type text (64 cells of ptr x17 u8) prints 'hook: non-certified definition: ^@^@...'. Callers work around it (the caveat above CLAUSE-NAME, the carry-over in check-walk.f CHECKER-NATIVE-DOES-REPORT). Acceptance: a grow keeps every buffer's bytes; the caveat and the carry-over are deleted; a test of a built local with a type text past the initial capacity names the definition. Depends: 47158f28. Claim: unassigned.
