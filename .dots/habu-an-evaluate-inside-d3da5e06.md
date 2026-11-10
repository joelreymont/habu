---
title: An evaluate inside a checked file places its refusal at line 1 column 17
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T17:23:37.865464+02:00"
---

Problem: a refusal raised in text run by evaluate inside a checked file is placed at 1/17 (bytes 16-23), the place the old rule gives stdin text, not where the token lies in the file or in the evaluated string. Seen while fixing habu-a-preflight-refusal-87f2d9f2 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rows-done.md, 'Found outside this lane'): that fix leaves stdin and evaluate text unarmed on purpose (ROWS-ARM requires SRCLOC:FILE-BUFFER?), so their packets stay byte-identical to before. Cause not known: hunt first, with a file that evaluates a string holding a refused definition, under tools/check.f --json-errors and --load. Acceptance: the packet's line, column and byte range name the token in the text it lies in, and say which text that is. Files: src/core/checker.f, src/core/check-hook.f, test/diag-position-test.f. Verify: a case in test/diag-position-test.f that fails before. Depends: habu-a-preflight-refusal-87f2d9f2. Ownership: the checker's diagnostics. Claim: unassigned. Source: Joel, 2026-11-10: 'i want all issues identified to be dotted and fixed'.

Acceptance case 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/emptyclause-review.md L3): 's" : MYK ( n -- ) drop does> ( -- ) ;" evaluate' gives ';' at 1/10 bytes 9-10 under the check tool, inside the engine's copied text; the non-empty '... cr ;' puts cr at 1/10 the same way. habu-delete-the-trusted-42b30edd holds the TRUSTED: form of this case; this one stays after TRUSTED: goes.
