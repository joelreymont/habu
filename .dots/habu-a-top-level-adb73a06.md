---
title: "A top-level step's refusal record has an empty word and no position"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T13:08:07.052070+02:00"
---

Problem: the DIAG-JSON record for a refused top-level step (variable V / 1 V @ !, E-RAW-CELL-PTR) has "word":"" and no column or byte fields, so diag-contract fails (case top1.f in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases7/effect/cases). Found by the review of habu-a-packet-for-72692145. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the record passes the record check: a shape for a top-level step, or the fields filled. Files: src/core/render.f, tools/gate-json-assert-core.f. Verify: the case through diag-contract. Depends: habu-a-packet-for-72692145 landed. Ownership: the checker's diagnostics. Claim: unassigned.
