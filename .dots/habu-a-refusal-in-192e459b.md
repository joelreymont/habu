---
title: A refusal in a required file is placed in the main file
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:01:19.806930+02:00"
---

Problem: through tools/check.f a refusal inside a file the checked file requires is reported with the main file's name and a place in it. Reproducer (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rows-review.md F5): q1-main.f is '\ main' / 'require sub.f'; sub.f holds a TRUSTED: RAW-IMM immediate, ': A ( -- ) ;' and ': P ( -- ) ( c ) 1 drop \ d' / tab 'RAW-IMM ;'. The packet names q1-main.f at 1/17/16-23 on master (2/3/28-35 with the wrong-places commit xwsykotl); the truth is sub.f 4/3/79-86. The tool marks only the main file, so the origin is not the definition's place. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: where tools/diag-origin-core.f and tools/check-core.f mark and label a run, and what a required file's text carries. Acceptance: the packet names the required file and the token's true place. Files: named by the hunt. Verify: the reproducer in test/diag-position-test.f. Depends: habu-a-preflight-refusal-87f2d9f2 landing. Ownership: the check tool's marking of a run. Claim: unassigned.
