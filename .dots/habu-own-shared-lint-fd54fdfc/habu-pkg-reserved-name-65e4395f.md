---
title: Package reserved-name lint core
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-22T17:33:51.656025+02:00\""
blocks:
  - habu-pkg-reserved-name-c5424f85
  - habu-pkg-remaining-legacy-39ffae5f
  - habu-cut-over-check-ac1b7cdf
---

Files: tools/reserved-name-lint-core.f plus qualified call updates in RESERVED-NAME-LINT-CLI, RESERVED-NAME-LINT-TEST, and package CHECK after those owners land. Put the core in package RESERVED-NAME-LINT, keep all state and helpers private with short tails, and publish exactly RESET ( -- ), FILE-AS ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), JSON! ( bool -- ), OUT-FD! ( fd -- ), and FINISH ( -- ). Do not export counts, buffers, or raw cells. Acceptance: no RNL-* implementation name remains global; all reserved families, case folding, numeric claims, loader and control names, labels, JSON diagnostics, and output routing remain exact; no compatibility alias. Verify: reserved-name-lint-test.f, checker reserved-name cases, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.
