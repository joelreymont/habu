---
title: Allowlist type-family.f in package-diff-lint
status: active
priority: 2
issue-type: task
created-at: "2026-07-22T15:30:44.191992+02:00"
---

Why: tools/package-diff-lint-core.f exempts the documented core/prelude language surface (sumtype.f, roles.f, enums.f, structures.f) but not src/core/type-family.f, so every body edit to that core file — by either agent, e.g. the xpad fail-closed fix and the package-family resolver work — reports E-PACKAGE-OWNERSHIP for changed definitions that are legitimately core-global. Owned result: src/core/type-family.f added to the GLOBAL-IMPLEMENTATION? allowlist in tools/package-diff-lint-core.f, with a comment stating it is documented core language surface, plus a negative control proving a NON-allowlisted file still fails on a changed global definition (the exemption must not widen). Acceptance: a fixture diff editing a type-family.f word body passes; the same shape against a non-allowlisted lib file still reports E-PACKAGE-OWNERSHIP; the lint's own suite green. Owning gate: the package-diff-lint test suite via bin/hb. Depends: none. Files: tools/package-diff-lint-core.f and its test.
Claim: agent=allowlist-tf workspace=.jj-ws/habu-allowlist-tf
