---
title: Relocate persisted defer cells across snapshots
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-29T21:34:38.660676+02:00\""
---

Full context: lldb evidence in the round-3 review of .jj-ws/habu-relocate-snapshot-region-752042fe shows defer/is cells in the DP heap (including HOOK-CELL) hold execution tokens that are absolute writer-run region addresses; after the region is relocated to text+REGION-OFF these cells still point into the old writer mapping and the restored image crashes on first defer call. This is the hard blocker habu-canonicalise-data-region-72628eaa describes for DATA pointers generally. Required design per review: record a declared relocation kind at the is store site (the store word tags the cell as an xt cell in a relocation table), and rebase tagged cells during EM-SNAPSHOT-REBASE-DATA-XT at write and restore. A value-band scan over the heap guessing which cells look like xts is FORBIDDEN - that is a value heuristic where a structural invariant (declared at store time) is possible. Acceptance: a snapshot written after an is store boots and the deferred word executes correctly at the relocated address; negative regression proving an untagged forged value is not rebased; part of the 200-clean-boot campaign acceptance.

Claim: agent=snapreloc workspace=.jj-ws/habu-relocate-snapshot-region-752042fe
