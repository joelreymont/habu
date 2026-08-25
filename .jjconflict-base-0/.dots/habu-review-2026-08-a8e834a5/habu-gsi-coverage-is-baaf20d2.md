---
title: GSI coverage is granted by text, not by a started phase
status: open
priority: 3
issue-type: task
created-at: "2026-08-23T12:14:26.170656+02:00"
---

Problem: tools/lint/schedule-lint.f SCHED-SET collects every 's" PATH" GSI-*' row under test/ whether or not a started phase ever calls the enclosing word, so a GSI list inside a dead definition still certifies its files - the same hole as the slice-coverage one fixed 2026-08-23, one level down (found by that lane). Acceptance: GSI rows count only when reached from a started phase's inline body (walk from the phase table through the GSI words, structurally), with a fixture: a GSI list in an unreferenced definition must not certify its file. Files: tools/lint/schedule-lint.f and its test. Verify: schedule-lint-test with the fixture. Depends: habu-schedule-lint-counts-9eaac4d2. Ownership: gate runner. Claim: unassigned.
