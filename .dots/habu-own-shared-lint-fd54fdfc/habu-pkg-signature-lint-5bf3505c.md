---
title: Package signature lint core
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-22T17:33:51.445418+02:00\""
closed-at: "2026-07-25T14:39:37.552833+02:00"
close-reason: "Landed in master@origin as commit e3458e479e34 'Package signature lint core', reachable from 79c50e5a9dbf, in the composed CHECK train the landing note required. tools/signature-lint-core.f opens package SIGNATURE-LINT with a private body and publishes the six named API words; tools/signature-lint.f, tools/hb-build-direct-lints.f, tools/check-core.f and tools/signature-lint-test-lib.f call them qualified."
blocks:
  - habu-pkg-signature-lint-a20a9041
  - habu-pkg-direct-build-d2e501d3
  - habu-cut-over-check-ac1b7cdf
---

Need: the signature-lint core has no namespace owner, so its state and implementation names leak globally and block package ownership of the shared lexer. Dependencies: the SIGNATURE-LINT-CLI, HB-BUILD-DIRECT-LINTS, and CHECK owners must already exist; do not absorb their migrations. Owned result: in tools/signature-lint-core.f, open package SIGNATURE-LINT around the existing core. Keep every existing SL-* state/helper name private in this leaf. Rename only the six real API definitions at their definition sites and publish exactly RESET ( -- ), FILE-AS ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), JSON! ( bool -- ), OUT-FD! ( fd -- ), and FINISH ( -- ). Update only the exact qualified consumers in tools/signature-lint.f, tools/hb-build-direct-lints.f, tools/check-core.f, and the packaged test library tools/signature-lint-test-lib.f (its SIGNATURE-LINT-TEST package calls the core APIs; the test-library packaging lands before this leaf in the composed train, so these edits compose in-package). No forwarding aliases, duplicate API, behavior change, private-tail cleanup, or unrelated lexer work. Acceptance: package SIGNATURE-LINT owns the complete core; only the six words are externally callable; all former public global names reject; text and JSON findings, source labels, missing names, opt-outs, strict signatures, output routing, and direct-build/CHECK behavior remain exact. Production proof: first show the package gate rejecting one representative global core definition, then pass tools/signature-lint-test.f, checker signature cases through tools/check-test.f, the real hb-build direct-lint path, an external old-name rejection, typed-local-diff-lint and package-diff-lint on the exact diff, host-lint, and filemap-lint.

Claim: agent=claude workspace=.jj-ws/habu-pkg-signature-lint-5bf3505c

Landing note (2026-07-24): dual-accepted candidate 5720abafe6e9 lands only in the composed train with the CHECK package cutover per codex ruling A. The packaged test-library caller is part of the exact composition; no standalone source publication is allowed.
