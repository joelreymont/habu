---
title: Package all-errors tests
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.358342+02:00"
---

Files: tools/check-all-errors-test.f only. Put every non-fixture region in package CHECK-ALL-ERRORS-TEST, close it before and reopen it after each embedded CAE-PKG-NOMINAL package, make state and helpers private with short tails, and invoke a private RUN inside the final package region. Continue calling the still-global all-errors and checker APIs. Acceptance: no CAE-* implementation name remains global; every support, cascade, duplicate, JSON, nominal, trust, export, and replay case remains active; both embedded fixture packages and all embedded source text are unchanged; no alias. Verify: bin/hb --load tools/check-all-errors-test.f, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=all_errors_test workspace=.jj-ws/habu-pkg-all-errors-042245b9.
