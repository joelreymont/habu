---
title: Share the GATE=2 engine across aot-wid rows and prune dead AOT rows
status: open
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:21.046714+02:00"
---

Problem: the GATE=2 engine is built identically in boot-open, rebase-sealed and rebase-open; aot-named-cells NO-SUCH-TARGET forge is subsumed by LOCAL; six aot-data-* rows assert nothing on macOS; aot-wid-refuse-bound twins test/seal.f:500-504; aot-wide-format-suite.f:84-85 's" 42" CONTAINS?' cannot fail (the EXT report contains 42). Evidence and design: ~/.cache/tmp/kestrel-gate/test-review/L3-aot-image.md findings 3, 5, 6, 7, 8 and trims. Acceptance: one keyed build serves the wid rows; boot-open folds into rebase-open; batch-observable aot-data claims move into batch probes and the rest are deleted with their duplicates named; the 42 check compares a labelled answer exactly. Files/Ownership: test/aot-wid-*.f, test/aot-data-*.f, test/aot-named-cells-suite.f, test/aot-wide-*.f, test/aot-sig-pool*.f, test/aot-cell-values*.f, registry rows. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: habu Cache-the-native-fixture-writer dot if the wid builds go through the writer. Claim: unassigned.
