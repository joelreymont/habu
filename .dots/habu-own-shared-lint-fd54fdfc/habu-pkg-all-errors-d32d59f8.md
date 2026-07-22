---
title: Package all-errors command
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.255784+02:00"
---

Files: tools/check-all-errors.f only. Add package CHECK-ALL-ERRORS-CLI, make CLI buffers and helpers private with short tails, retain a private RUN invoked inside the package, and continue calling the still-global all-errors core API. Preserve arguments, output routing, capacities, and exit behavior. Acceptance: no command definition or storage remains global and no alias remains. Verify: bin/hb --load tools/check-all-errors-test.f, direct clean/failing command fixtures, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=all_errors_cli workspace=.jj-ws/habu-pkg-all-errors-d32d59f8.
