---
title: "Convert every error constant to ERROR:CODE"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:05:56.796510+02:00"
---

Problem: Joel 2026-11-06 chose ERROR:CODE for every error constant, the text required. Acceptance: every minting 'n constant E-X' outside the cold-load prefix becomes 'n s" text" ERROR:CODE E-X' (910 in 90 files at design time: src/lib 793 with errors.f's own, src/core, src/habu 34, src/arch 23, src/compiler 9, src/os 1, tools 36, test 18; recount at dispatch); a same-line or preceding comment that states what the code means becomes the text, one that explains internals stays a comment; codes without one get a sentence from their throw sites. A text names the full thing that failed and the limit constant it met; a code thrown for different limits splits into one code per limit (E-HIR-CAP, src/lib/error.f:545, becomes three: a capacity argument out of range, src/compiler/native/hir-word.f:427 and :431; a table full, :436 and :440, the session table over SROW-CAP included; a session vocabulary over ORD-CAP, :1578). FIRST/LAST bounds and aliases ('E-X constant E-Y') stay constants. The 107 codes the cold-load prefix mints (tools/cold-load.f:188-229 files) stay constants. Campaign: slices of at most about 60 codes each, by file or error block, each a worker lane and one commit, created when the ERROR package lands. Verify per slice: the slice's own tests and tools/error-code-lint.f; a candidate and the full suite once per batch of slices touching baked files. Depends: the ERROR package. Ownership: carl. Claim: unassigned.
