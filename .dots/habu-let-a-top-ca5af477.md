---
title: Let a top-level stack word move a layout value whole
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T10:22:15.477576+01:00"
---

Problem: at top level an engine stack primitive (dup, swap, over and kin) on a multi-cell value is E-LAYOUT-VALUE (docs/forth-card.md:253), so with the split-str marker gone a top-level `dup` of a str is rejected and test files work around it: private words test/engine-private-test.f TCE-PATH!, test/enum-decl-suite.f EC-PATH! and test/structure-decl-suite.f SC-PATH!, and `str>` splits in test/ndict-binding.f and test/name-length-test.f. The quotation lane lifts the same limit inside a quotation literal. Acceptance: a top-level stack word moves a value whose type the top-level row knows whole, as inside a definition; the workaround words fold back; the card row says so. Depends: none. Claim: unassigned.
