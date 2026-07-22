---
title: Package AOT lint core
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:51.543853+02:00"
blocks:
  - habu-pkg-aot-lint-cd6f466a
  - habu-pkg-aot-lint-defb0f2e
  - habu-pkg-direct-build-d2e501d3
---

Files: tools/aot-lint-core.f plus qualified call updates in AOT-LINT-CLI, AOT-LINT-TEST, and HB-BUILD-DIRECT-LINTS after those owners land. Put the core in package AOT-LINT, keep all state and helpers private with short tails, and publish exactly RESET ( -- ), FILE-AS ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), JSON! ( bool -- ), OUT-FD! ( fd -- ), and FINISH ( -- ). Do not export summary, counts, buffers, or raw cells. Acceptance: no AL-* implementation name remains global; clean, rejection, JSON, label, structured process outcomes, and output routing remain exact; no compatibility alias. Verify: aot-lint-test.f, hb-build direct lint slice, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.
