---
title: GJA-STORAGE-FIELDS repeats GJA-DECL-POSITION inline
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T19:01:50.744474+02:00"
---

Problem: tools/gate-json-assert-core.f GJA-STORAGE-FIELDS (:569-573 at 4e1ac4f5) spells the all-four-or-none place rule itself where the other packet kinds call GJA-DECL-POSITION, so the rule lives in two places in that tool. Found by the re-review of habu-a-preflight-refusal-87f2d9f2 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rows-rereview2.md, O2). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: GJA-STORAGE-FIELDS calls GJA-DECL-POSITION and GJA-DECL-POSITION's comment names storage; a storage packet with a partial place is refused as before. Trace first: that the five lines and the word refuse the same packets (messages included). Files: tools/gate-json-assert-core.f. Verify: the tool's tests that judge a storage packet. Depends: none. Ownership: GJA-STORAGE-FIELDS. Claim: unassigned.
