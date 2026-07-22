---
title: Package signature lint core
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-22T17:33:51.445418+02:00\""
blocks:
  - habu-pkg-signature-lint-a20a9041
  - habu-pkg-remaining-legacy-39ffae5f
  - habu-pkg-direct-build-d2e501d3
  - habu-pkg-checker-core-fbd4eb5e
---

Files: tools/signature-lint-core.f plus qualified call updates in SIGNATURE-LINT-CLI, SIGNATURE-LINT-TEST, HB-BUILD-DIRECT-LINTS, and CHECK after those owners land. Put the core in package SIGNATURE-LINT, keep all state and helpers private with short tails, and publish exactly RESET ( -- ), FILE-AS ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), JSON! ( bool -- ), OUT-FD! ( fd -- ), and FINISH ( -- ). Do not export summary, counts, buffers, or raw cells. Acceptance: no SL-* implementation name remains global; text and JSON findings, source labels, missing names, opt-outs, strict signatures, and output routing remain exact; no compatibility alias. Verify: signature-lint-test.f, checker signature cases, hb-build direct lint slice, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.
