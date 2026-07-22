---
title: Package gate runner
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:11.946042+02:00"
---

Files: test/gate-runner-lib.f and test/gate-runner-entry.f only. Put the runner in package GATE-RUNNER, make all state and helpers private with short tails, publish RUN ( -- ) as the sole entry, and change the entry file to call GATE-RUNNER:RUN. Diagnostic and dictionary calls stay on their current global APIs in this leaf. Acceptance: no GR-* definition or storage remains global; serial and worker dispatch, argument parsing, timing, semantic-tool selection, and reports are byte-for-byte equivalent; no compatibility alias. Verify: gate-runner entry test, one serial and one worker slice, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.
