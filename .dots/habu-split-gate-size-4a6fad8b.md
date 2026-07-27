---
title: Split gate size test entry
status: active
priority: 2
issue-type: task
created-at: "2026-07-27T10:44:48.847564+02:00"
---

Invariant: reusable SIZE-ATTR manifest definitions load once through a side-effect-free library; executable test entries require that library and run assertions exactly once. Cause: test/gate-runner-support.f requires test/gate-engine-lib.f, which requires executable test/gate-size-attribution-test.f; tail-pure later fork-includes the same file and the inherited dictionary rejects duplicate private CODE-OFF with rc 78. Fix: add test/gate-size-attribution-lib.f for the manifest and public API, make test/gate-size-attribution-test.f a thin require plus SIZE-ATTR:RUN entry, require the library from test/gate-engine-lib.f, retain exactly one production GSI-FORK-INCLUDE row for the entry, add an exact preload-then-entry execution-count and source-shape regression in test/gate-runner-entry-test.f, and update FILEMAP and all canonical path references. Files: test/gate-size-attribution-lib.f, test/gate-size-attribution-test.f, test/gate-engine-lib.f, test/gate-runner-entry-test.f, FILEMAP.md, STATUS.md, docs/size-rca.md, docs/worker-briefing.md, test/gate-build-size.f, test/match-factor-pin.f, LESSONS.md. Forbidden: changing GSI-FORK-INCLUDE to required, removing or skipping the test row, or reordering dependencies to hide the collision. Acceptance: standalone size test, exact preload reproducer proving one SIZE-ATTR:RUN completion, one exact production GSI-FORK-INCLUDE source row, tail-pure, runner/build tests, typed-local and package diff lints, file-map lint, and host lint pass. Base: d380e286cc577463e623f0ac20c9cbbd7e650405. Claim: agent=gate_size workspace=.jj-ws/gate-size-entry
