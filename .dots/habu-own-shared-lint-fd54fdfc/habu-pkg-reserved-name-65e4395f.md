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

Need: the reserved-name lint core has no namespace owner, so its state and implementation names leak globally and block package ownership of the shared lexer. Dependencies: the RESERVED-NAME-LINT-CLI and CHECK owners must already exist; do not absorb their migrations. Owned result: in tools/reserved-name-lint-core.f, open package RESERVED-NAME-LINT around the existing core. Keep every existing RNL-* state/helper name private in this leaf. Rename only the six real API definitions at their definition sites and publish exactly RESET ( -- ), FILE-AS ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), JSON! ( bool -- ), OUT-FD! ( fd -- ), and FINISH ( -- ). Update only the exact qualified consumers in tools/reserved-name-lint.f and tools/check-core.f. No forwarding aliases, duplicate API, behavior change, private-tail cleanup, or unrelated lexer work. Acceptance: package RESERVED-NAME-LINT owns the complete core; only the six words are externally callable; all former public global names reject; every reserved family, case folding, numeric claim, loader/control name, label, JSON diagnostic, output route, and CHECK behavior remains exact. Production proof: first show the package gate rejecting one representative global core definition, then pass tools/reserved-name-lint-test.f, checker reserved-name cases through tools/check-test.f, an external old-name rejection, typed-local-diff-lint and package-diff-lint on the exact diff, host-lint, and filemap-lint.
