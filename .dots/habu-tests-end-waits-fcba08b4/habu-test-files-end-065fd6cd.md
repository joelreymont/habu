---
title: test/ files end their waits by a clock, group 2
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:03:23.083842+01:00"
closed-at: "2026-11-13T21:29:13.878393+01:00"
---

Problem: test/deep-cwd-e2e.f, diag-position-test.f, engine-error-package.f, engine-stack-machine.f, exit-hook-test.f, export-package.f, gate-common-lib.f, gate-debug-lib.f, gate-dictionary-lib.f and include-refusal-e2e.f end waits by deadlines. Acceptance: each wait on another party in these files has no deadline or count; a bound its case claims is asserted (test/AGENTS.md); the files' suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wT2.
