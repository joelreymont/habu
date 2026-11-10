---
title: "A TRUSTED: definer head with a bad stored signature is refused on the clause's line"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T13:31:22.417464+02:00"
---

Problem: 'TRUSTED: MK1 ( n -- nosuch:fam )' / 'create ,' / 'does> ( -- n )' / '@ ;' prints 'ncomp: cannot compile MK1 at nosuch:fam in …/v1.f:3'; the token is on line 1 (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/ncomp/a/ v1 and v3). Cause (review of habu-a-stored-signature-8de80e37, finding A-1): DECLARE-ROW runs after CHECK-DOES-SPLIT, so M-SCAN (src/compiler/native/definition.f:193) and the checker's TBASE and SGA SGU (src/core/checker.f:27954) describe the does> clause; SGBAD-AT's CORE-STR= fails, CHECKED-OFF gives -1 and FAILB is 0, the clause's first byte. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the line names the head's token on the head's line; the head's row is registered before the clause scan, or the place otherwise comes from the head's text without changing what REFUSED-SPAN's offsets mean; PIN-REFUSED!'s comment is true; a file-based case in test/load-reject-diag-test.f pins v1. Files: src/core/checker.f, src/compiler/native/definition.f, test/load-reject-diag-test.f. Verify: v1 and v3 on bin/hb; the case fails on the engine before. Depends: habu-a-stored-signature-8de80e37 landed. Ownership: the stored-signature refusal. Claim: unassigned.
