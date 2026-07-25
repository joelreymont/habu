---
title: Package reserved-name lint core
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-22T17:33:51.656025+02:00\""
closed-at: "2026-07-25T14:39:37.559599+02:00"
close-reason: "Landed in master@origin as commit f1bd0f227e4c 'Package reserved-name lint core', reachable from 79c50e5a9dbf, in the composed CHECK train the landing note required. tools/reserved-name-lint-core.f opens package RESERVED-NAME-LINT with a private body and publishes the six named API words; the CLI, tools/check-core.f, the packaged test library and CHECK's RESERVED-LIST-RUN call them qualified."
blocks:
  - habu-pkg-reserved-name-c5424f85
  - habu-cut-over-check-ac1b7cdf
---

Need: the reserved-name lint core has no namespace owner, so its state and implementation names leak globally and block package ownership of the shared lexer. Dependencies: the RESERVED-NAME-LINT-CLI and CHECK owners must already exist; do not absorb their migrations. Owned result: in tools/reserved-name-lint-core.f, open package RESERVED-NAME-LINT around the existing core. Keep every existing RNL-* state/helper name private in this leaf. Rename only the six real API definitions at their definition sites and publish exactly RESET ( -- ), FILE-AS ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), JSON! ( bool -- ), OUT-FD! ( fd -- ), and FINISH ( -- ). Update only the exact qualified consumers in tools/reserved-name-lint.f, tools/check-core.f, and the packaged test library tools/reserved-name-lint-test-lib.f (its RESERVED-NAME-LINT-TEST package calls the core APIs; the test-library packaging lands before this leaf in the composed train, so these edits compose in-package). A fourth production consumer exists in tools/check-test-lib.f: RESERVED-LIST-RUN drives the core APIs directly from package CHECK. No forwarding aliases, duplicate API, behavior change, private-tail cleanup, or unrelated lexer work. Acceptance: package RESERVED-NAME-LINT owns the complete core; only the six words are externally callable; all former public global names reject; every reserved family, case folding, numeric claim, loader/control name, label, JSON diagnostic, output route, and CHECK behavior remains exact. Production proof: first show the package gate rejecting one representative global core definition, then pass tools/reserved-name-lint-test.f, checker reserved-name cases through tools/check-test.f, an external old-name rejection, typed-local-diff-lint and package-diff-lint on the exact diff, host-lint, and filemap-lint.

Claim: agent=claude workspace=.jj-ws/habu-pkg-reserved-name-65e4395f

Landing note (2026-07-24): dual-accepted candidate 9798c88aa418 lands only in the composed train with the CHECK package cutover per codex ruling A. The CHECK cutover owns the final RESERVED-LIST-RUN composition; no standalone source publication is allowed.
