---
title: "The load names no line when it refuses a +FIELD or FUNCTION: name"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:49:05.270295+02:00"
---

Problem: a refusal of the name a '+FIELD' or 'FUNCTION:' defines prints the file with no line under the load, where docs/forth.md:198 promises '<file>:<line>' and the check gives the place. With 'package SP public variable X seal-package ;package': 'begin-structure S9 8 +FIELD SP:X end-structure' loads 'E-SEALED-PACKAGE: SP:X at <file>' (check: 6:29, bytes 79-83); 'require src/lib/ffi-abi.f' then 'PROCESS-SYMBOLS FUNCTION: SP:X getpid ( -- n ) ;FUNCTION' the same (check: 7:27). Not the seal's alone: an unsealed or global duplicate prints 'duplicate definition: UP:X at <file>'. The same before the seal commit's second fold (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/seal-rereview2.md finding 3). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: where the two definers raise the refusal and what the place words hold then, as path:line; whether it is the cause habu-a-typed-storage-f57ce21c's second commit fixes (a refusal inside evaluated text reports the outer statement's place) or another. Acceptance: both programs and the duplicate print the name's line under the load. Files: named by the hunt. Verify: the programs in test/load-reject-diag-test.f. Depends: none; read habu-a-typed-storage-f57ce21c first. Ownership: the load's refusals. Claim: unassigned.
