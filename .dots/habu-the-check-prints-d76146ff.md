---
title: The check prints no JSON packet for a typed-storage name that is a number or keyword
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:21:44.228322+02:00"
---

Problem: for `TYPED-VARIABLE 12 n` and a keyword as the name (cases e/num-tvar, e/kw-tvar) tools/check.f prints the load's prose with rc 70 and no JSON packet under --json-errors. Outputs in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/sealdef/out/e-*; hunt /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/sealdef-hunt.md (Out of scope). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line why no packet is written; then both cases print a packet with code, token and place that passes the record check, and a test pins one. Files: src/habu/verify-source.f, src/core/layout-buffer.f. Verify: the two cases under --json-errors. Depends: none. Ownership: the source pre-pass. Claim: unassigned.
