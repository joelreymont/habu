---
title: Package all-errors tests
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.358342+02:00"
---

Files: tools/check-all-errors-test.f only. Put non-fixture code in package CHECK-ALL-ERRORS-TEST, reopen it after the embedded CAE-PKG-NOMINAL fixture, make state and helpers private with short tails, and invoke a private RUN inside the package. Continue calling the still-global all-errors and checker APIs. Acceptance: no CAE-* implementation name remains global; every support, cascade, duplicate, JSON, nominal, trust, export, and replay case remains active; embedded source text is unchanged; no alias. Verify: bin/hb --load tools/check-all-errors-test.f, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.
