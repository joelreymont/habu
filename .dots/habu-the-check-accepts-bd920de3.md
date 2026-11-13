---
title: The check accepts a definition of an engine-global name the load refuses
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:22:55.115341+02:00"
---

Problem: ': E-CHECKER-RECORD-UNRESOLVED ( -- n ) 1 ;' passes 'bin/hb --load tools/check.f -- --verify-only' with rc 0 while the load refuses it as a duplicate definition, on engines before and after the deferral-cause commit (n4-ec.f); each global name the engine gains widens it: n1-dw.f (a user word named as one of that commit's DW- constants) loads with rc 0 before and is refused, rc 78, after. Found by its review (/home/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/defer-cause-review.md, finding 6d; programs in /home/joel/Work/reviews/habu-10x-2026-11-10/cases8/defer-review/p). May be covered in part by habu-move-engine-internal-fc2ba551 (engine words leave the global dictionary) and habu-report-a-duplicate-5ed354dc: read both first. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the check and the load agree on a definition whose name the engine already defines globally, and a test pins it. Files: src/habu/verify-source.f, src/core/checker.f, test/diag-position-test.f. Verify: n4-ec.f under --verify-only and --load. Depends: none. Ownership: the source pre-pass. Claim: unassigned.

2026-11-10 (bob): this defect is the quick check's alone (tools/check.f --verify-only, src/habu/verify-source.f), the scanner habu-check-src-through-1198f4ed replaces. It is fixed by that dot, not in the scanner, and is worked last in its section.
