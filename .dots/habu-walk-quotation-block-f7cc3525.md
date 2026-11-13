---
title: Walk quotation block parts with one word
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:31:36.488944+02:00"
---

Problem: src/core/checker.f OWN-BLOCKS* (:21912-:21620) repeats the walk of KEEP-QUOTS* (:2953-:2964) over rows, pointers, stale wrappers and family arguments and differs only in what it does at a quotation block part. Found by the fix re-review of the quotation line (finding A2). Acceptance: one walker over block parts that both call with their action; no behaviour change. Trace the callers before briefing: an offered simplification is a change. Files: src/core/checker.f. Verify: test/engine-suite.f and test/quotation-locals.f rc 0 on a rebuilt engine. Depends: none. Ownership: unassigned. Claim: unassigned.
