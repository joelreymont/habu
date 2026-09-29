---
title: Scan and interpret in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.646748+03:00"
blocks:
  - habu-find-dictionary-names-e8f56969
  - habu-parse-numbers-in-86b27302
---

Problem: the token scanner and interpret loop are assembly (`LMAIN`, the `habu2.f:4370` top-row hook block).
Acceptance: token scan over `INP`/`INE`, comments, the depth-floor guard (`E-UNDERFLOW`), the min-in guard (`DNAME-MIN-IN`), the `DNAME-WIDE`/`DNAME-INT` fail-closed refusals, the top-row hook events (`habu2.f:4370` block), execute; drives a `--load` of a test file behind a feature cell (not yet the default).
Files: `src/habu/outer.f`, `test/outer-interpret.f`.
Verify: spark `bin/hb --load test/outer-interpret.f`; a whole `--load` of a test file through the Habu loop under the feature cell.
Depends: habu-find-dictionary-names-e8f56969 (I2), habu-parse-numbers-in-86b27302 (I3).
Route: Alder (shared: src/habu/outer.f, test/outer-interpret.f).
Ownership: krait (Intel lane).
Claim: unassigned.
