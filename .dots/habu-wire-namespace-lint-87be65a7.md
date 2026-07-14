---
title: Wire namespace-lint and error-code-lint into a gate slice
status: open
priority: 3
issue-type: task
created-at: "2026-07-14T18:07:24.198071+02:00"
---

Found by the suite-coverage lane 2026-07-14: tools/namespace-lint.f and tools/error-code-lint.f (+ their -test.f fixtures) have TEST:SUITE entries in test/gate-stdlib-cases.f but their labels appear in NO SUITE-*-LABEL? slice list (test/gate-stdlib-lib.f), so they run in no automatic OR manual slice today - they are documented MANUAL in the suite-coverage table only to make the partition honest. Fix: add their labels to the appropriate slice list (lint-tools most likely) and/or mirror into a scheduled GSI group if cheap enough for the fast tier; then move them out of the MANUAL-GATE table in tools/suite-coverage-lint-core.f (the lint's MANUAL-STALE class will force that cleanup). Verify: suite-coverage-lint 0 findings, the chosen slice green, full test/run.f if a GSI group gains them. Files: test/gate-stdlib-lib.f, possibly test/gate-stdlib-inline-lib.f / gate-stdlib-lint-tools.f, tools/suite-coverage-lint-core.f. Ownership: gate suite membership.
