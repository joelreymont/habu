---
title: A refusal in an evaluated string is placed inside a comment
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:24:27.440722+02:00"
---

Problem: 's" : Q ( -- ) ( c ) 1 drop drop ;" evaluate' is refused at 1/17/16-20, inside '( c )', on both engines of the wrong-places lane; the token is in the file's own string. Not reproduced by the lead. Found by the re-review of habu-a-preflight-refusal-87f2d9f2 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rows-rereview.md, Found outside the fold): text with no map takes its definition's origin plus the token's offset in the checked text, which skips comments. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first, on master after that lane lands: what the checked text of an evaluated definition is, why the offset is not the string's, and whether the capture rows can map it. Acceptance: the refusal names the token's own line, column and bytes in the file, or carries no place where none is true. Files: named by the hunt. Verify: the reproducer in test/diag-position-test.f. Depends: habu-a-preflight-refusal-87f2d9f2. Ownership: the place of a refusal in evaluated text. Claim: unassigned.
