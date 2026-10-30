---
title: Keep one A-Z fold word in the checker
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T08:17:44.595652+02:00"
---

Problem: src/core/checker.f folds A-Z to a-z in three copies: SYM-FOLD-C (near line 7720, the symbol store's fold through SYM-COPY-FOLD), CHECKER-FOLD-C (near 12052, identical text, 4 callers after habu-refuse-a-top switched the seeded-row comparison to SYM-FOLD-C) and an inline copy in TOKFOLD (near 21921, `dup 64 > over 91 < and IF 32 or THEN`). A seeded-row match depends on the store's fold and the probe's fold agreeing; three copies let one drift. Found by the Opus re-review of habu-refuse-a-top-57851ac2. Acceptance: one fold word (SYM-FOLD-C, defined first) used by every caller, CHECKER-FOLD-C and TOKFOLD's inline copy gone; no behaviour change, so the existing seeded-row and fold rows cover it (aot-payload-graph, outer-interpret). Shared checker code: rebuild to convergence and run the full suite. Files: src/core/checker.f. Verify: bin/hb --load test/run.f after convergence. Depends: habu-refuse-a-top-57851ac2. Ownership: the three sites. Claim: unassigned.
