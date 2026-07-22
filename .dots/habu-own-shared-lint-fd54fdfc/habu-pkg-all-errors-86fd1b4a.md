---
title: Package all-errors core
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.806999+02:00"
blocks:
  - habu-pkg-all-errors-d32d59f8
  - habu-pkg-repair-hint-1b98c230
  - habu-pkg-repair-packet-405dab84
  - habu-pkg-repair-schema-d20f1641
  - habu-pkg-checker-core-fbd4eb5e
---

Files: tools/check-all-errors-core.f plus qualified call updates in its already-packaged CLI, current test consumers, GATE-DICTIONARY, and package CHECK. Put the core in package CHECK-ALL-ERRORS, keep all state and helpers private with short tails, and publish exactly BUFFERS! ( ptr u8 n ptr u8 n -- ), OUT$ ( -- ptr u8 n ), JSON! ( bool -- ), SUPPORT-RESET ( -- ), SUPPORT+ ( ptr u8 n -- ), FILE ( ptr u8 n ptr u8 n -- ), BUF ( ptr u8 n ptr u8 n -- ), and DUP-RC. Move command-only buffer capacities and writes into CHECK-ALL-ERRORS-CLI; export no raw cells or generic write helper. In CA-CHECK-FULL-SCOPE, after opening the rollback scope, enter neutral top-level checker package state before replay and verification; CHECKER-SCOPE-DONE restores the exact caller package on success and throw. This atomically owns habu-reset-all-errors-bb250f7c because package-diff-lint forbids a standalone change to the legacy global definition. Validate with the saved private-package test candidate, but do not absorb its separate harness rename. Acceptance: no CA-* implementation name remains global; support replay, duplicate classification, output routing, I/O failure propagation, caller-package isolation, and reuse after error remain exact; no compatibility alias. Verify: check-all-errors-test.f plus private-package candidate, checker all-errors slices, gate dictionary, repair tests, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.
