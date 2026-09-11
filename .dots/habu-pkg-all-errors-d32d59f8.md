---
title: Package all-errors command
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:12.255784+02:00"
closed-at: "2026-07-25T14:48:55.081079+02:00"
close-reason: "Landed in master@origin as commit 2a260890e62a 'Package all-errors command', reachable from 79c50e5a9dbf, touching tools/check-all-errors.f only as the contract required. That file now opens package CHECK-ALL-ERRORS-CLI at line 8 with a private body and a private RUN invoked inside the package. The leaf landed while the core API was still global; the later core cutover in 79c50e5a9dbf updated these call sites, so the CLI now reaches the core through qualified CHECK-ALL-ERRORS:BUFFERS!, JSON!, FILE and OUT$ calls. No command definition or storage remains global and no alias was added."
---

Files: tools/check-all-errors.f only. Add package CHECK-ALL-ERRORS-CLI, make CLI buffers and helpers private with short tails, retain a private RUN invoked inside the package, and continue calling the still-global all-errors core API. Preserve arguments, output routing, capacities, and exit behavior. Acceptance: no command definition or storage remains global and no alias remains. Verify: bin/hb --load tools/check-all-errors-test.f, direct clean/failing command fixtures, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

Claim: agent=all_errors_cli workspace=.jj-ws/habu-pkg-all-errors-d32d59f8.
