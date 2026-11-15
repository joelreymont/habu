---
title: "Refuse an ambiguous parses: spelling in discovery"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T04:14:09.317044+02:00"
---

Problem: when a parsing word's spelling could name more than one row, tools/source-discovery.f reads its operand as ordinary source; that operand can open a comment that hides a load, a missed load (the design's 'an ambiguous spelling is a refusal' holds for family words, not for parses: rows). Fixtures: mirror (its files are lost); mirror-pre (top-level SDT-B has a parsing W with a row; inside SDT-A, 'SDT-B:W \ require dep.f' before part SDT-B and its non-parsing W exist: the engine uses top-level W and loads dep.f; rnqmvxss and the pkg2b folds miss it). Acceptance: an ambiguous parses: spelling refuses (E-DISC-SHADOW or its kin) unless every candidate row reads the same operand; mirror and mirror-pre find the load or are refused; a tools/source-discovery-test.f case (suite row source-discovery) failing before. Files: tools/source-discovery.f, tools/source-discovery-test.f. Verify: tools/source-discovery-test.f; the fixtures. Depends: pkg2b landed (habu-read-a-pkg-272bc1d0). Ownership: tools/source-discovery.f. Claim: unassigned.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
