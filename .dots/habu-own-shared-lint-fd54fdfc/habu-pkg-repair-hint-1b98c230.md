---
title: Package repair hint tests
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.459595+02:00"
---

Files: tools/check-repair-hints-test.f only. Put the harness in package CHECK-REPAIR-HINTS-TEST, make state and helpers private with short tails, and invoke a private RUN inside the package while still calling global checker and all-errors APIs. Preserve fixture text and exact repair classifications. Acceptance: no CRHT-* definition or storage remains global; all existing cases execute; no alias. Verify: bin/hb --load tools/check-repair-hints-test.f, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=repair_hint_test workspace=.jj-ws/habu-pkg-repair-hint-1b98c230.
