---
title: Own boundary lint API
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T03:47:22.647698+02:00"
blocks:
  - habu-pkg-boundary-lint-353528aa
  - habu-cut-over-check-ac1b7cdf
---

Why: the checked-boundary provider and its six caller operations are global; package ownership cannot land until every caller has an owner. Exact result: package CHECKED-BOUNDARY-LINT owns tools/checked-boundary-lint-core.f and publishes only RESET ( -- ), JSON! ( bool -- ), STRICT! ( bool -- ), OUT-FD! ( fd -- ), FILE ( ptr u8 n -- ), and FINISH ( -- ). Rename those six operations to their short tails, make every existing buffer, cell, constant, and other word private without otherwise renaming UB-prefixed internals, and update only tools/checked-boundary-lint.f, tools/checked-boundary-lint-test-lib.f, and tools/check-core.f to qualified calls. Preserve borrowed FILE path lifetime, reset state, finding order and count, JSON and prose bytes, strict behavior, output descriptor behavior, and FINISH throw contract exactly; hook authorization remains unchanged for the next leaf. Package CHECK and packages BOUNDARY-LINT-CLI and CBLT are required caller owners. Forbidden: aliases, forwarding globals, extra public words, exported storage, copied policy, registry changes, or behavior changes. Pre-change proof: representative global provider and global CHECK caller edits produce E-PACKAGE-OWNERSHIP. Acceptance: the real CLI, CBLT production suite, and CHECK suite are byte-identical; all six public calls resolve; private and legacy names reject; removing the owner produces the measured package findings; exact typed-local and package diff checks, host lint, and file-map lint pass. Files: tools/checked-boundary-lint-core.f, tools/checked-boundary-lint.f, tools/checked-boundary-lint-test-lib.f, tools/check-core.f. Depends: habu-pkg-boundary-lint-353528aa and habu-cut-over-check-ac1b7cdf. Ownership: package CHECKED-BOUNDARY-LINT public API and the four-file caller cutover. Claim: unassigned.
