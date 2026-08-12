---
title: The byte and cost columns cannot re-pin independently
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T09:00:30.000000+02:00"
---

Found by the cmp-imm lane (2026-08-12): --update-chain corpus rewrites EVERY row's cost column from the current run's timings when re-pinning bytes (trial diff: ADD3 1171->1107, CELL-BUMP 1273->1389, FACT 8587->8652 on rows whose bytes never moved), so a byte re-pin publishes a perf verdict the lane may not have measured on a quiet box. Consequence: rows go stale-loose after byte-improving merges (measured on master: WS? 72 vs pinned 88, COUNT-DOWN 32 vs 36, LADDER 144 vs 168 - the improved rows are exactly the under-protected ones). Fix the tool so bytes and costs re-pin independently, then one deliberate re-pin covering the accumulated deltas on a quiet box. NOTE: the judge artifact (judge-baseline.txt) may supersede this per its post-cut deletion plan - check before building. Files: tools/codegen-compare*.f. Depends: none.
