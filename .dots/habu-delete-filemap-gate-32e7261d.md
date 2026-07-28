---
title: Delete FILEMAP gate
status: active
priority: 2
issue-type: task
created-at: "2026-07-28T10:47:07.971349+02:00"
---

Why: FILEMAP.md is a 2,784-line manual index that duplicates repository search, omits Maki from its source policy, and creates a maintenance gate unrelated to inference correctness. Owned result: delete FILEMAP.md, tools/filemap-lint.f, and tools/filemap-lint-test.f; remove only their gate enrollment, suite classification, checked-boundary registration, host-lint fixture, and FILEMAP-specific comments. Replace tests that merely need an existing root file or invalid non-Forth include with README.md while preserving their exact production behavior. Update the shared interner comment without shrinking capacity. Exact code/test owners: test/gate-stdlib-cases.f, test/gate-stdlib-lint-tools.f, test/gate-stdlib-lib.f, tools/checked-boundary-lint-test-lib.f, tools/host-lint-test.f, tools/lint/intern.f, tools/lint/clobber-wrap-fixture.f, tools/lint/shadow-string-fixture.f, lib/process-cwd-test.f, test/compile-preflight-recovery.f, test/top-row-hook-test.f, and test/gate-size-attribution-test.f. Forbidden: replacement manifest, generated inventory, weakened host/package/trust checks, skipped suites, new host automation, or unrelated cleanup. Checkpoint: current filemap suite is green; deleting only the three owned files makes exact registered loaders fail, proving the real enrollment paths. Acceptance: no executable source or test references either deleted tool or FILEMAP.md; the replacement fixture tests fail before their path edits and pass after; host lint, checked-boundary lint, suite-coverage, lint-tools/status, process cwd, compile preflight, top-row hook, and the standard-library gate remain green. Ownership: executable gate and fixture removal only.

Claim: agent=codex-filemap-gate workspace=.jj-ws/habu-delete-filemap-gate-32e7261d
