---
title: Package bootstrap mirror lint core
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:51.772904+02:00"
blocks:
  - habu-pkg-bootstrap-mirror-4021501a
---

Files: tools/bootstrap-mirror-lint.f plus qualified call updates in BOOTSTRAP-MIRROR-LINT-TEST after that owner lands. Put the core in package BOOTSTRAP-MIRROR-LINT, keep all state and helpers private with short tails, and publish RESET ( -- ), FILE-AS ( ptr u8 n ptr u8 n -- ), FILE ( ptr u8 n -- ), FINISH ( -- ), and RUN ( -- ). Acceptance: no BML-* implementation name remains global; the source walk, exclusions, definition-name and escaped-token rules, overlay tests, counts, and exit behavior remain exact; no alias. Verify: bootstrap-mirror-lint-test.f, full bootstrap mirror lint, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.
