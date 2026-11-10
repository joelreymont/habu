---
title: file-objects cases assert only an exit code, and none covers pending-row order
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:00:01.976242+02:00"
---

Problem: test/file-objects.f SEALED-CASE and STORED-CASE assert only that the third build exits 0, where STALE-CASE also asserts what the object names (SPELL-FILES): a check and a link that both wrongly accepted the stale object would pass. No case covers REACHED-CK's ascending-order test over a mix of built and pending rows (reproducer /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/pending-review/fx o-ha, o-hb, o-hc, o-q1, o-q2, o-x2, o-mida, o-midb, root-oa, root-ob: building root-oa, root-ob, root-oa, root-oa fails E-STALE on the lane's parent and passes now). Also: src/habu/linker.f PENDING-HOLDER's -2 - q encoding and HOLDER-ROW only put off adding BUILT-N, which does not change during one FRESH? check; PENDING-HOLDER could answer BUILT-N + q and both go. Found by the review of habu-reached-ck-skips-c609af6b (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/pending-pieces-review.md, Low). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: G3 and T3 assert that sealed-spell.f's and stored-run.f's objects are rewritten; an order case pins the o-* sequence; the holder encoding is simplified if a trace of HOLDER-ROW's callers confirms the review. Files: test/file-objects.f, test/fixtures/file-objects, src/habu/linker.f. Verify: test/file-objects.f; each new assertion fails on an engine without the lane. Depends: none. Ownership: the file linker's pre-link check. Claim: unassigned.
