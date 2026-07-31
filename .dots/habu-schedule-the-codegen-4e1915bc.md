---
title: Schedule the codegen comparison harness in a gate
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T18:58:24.249198+02:00"
---

tools/codegen-compare-test.f is registered in no gate file: it is absent from test/gate-stdlib-cases.f, from every GSI group, and check (e) of tools/suite-coverage-lint-core.f only walks test/, so nothing notices. The harness therefore only ever runs when a person types it, and its green is not evidence about any merge. Wanted: a decision and a registration. It writes temp files and times real code, so the honest options are a SUITE in test/gate-stdlib-cases.f mirrored into a scheduled group, or a SUITE documented manual-gate in tools/suite-coverage-lint-core.f with the reason written down. Note the timing risk before choosing: the measurement pass is about 2 s idle and the note in tools/codegen-compare-core.f records 2.1-4.3 s under twice-oversubscription, with a per-row tolerance of 8x, so a heavily parallel gate could red a row for host load rather than for a compiler change. Found while adding the new chain's column (dot habu-wire-the-new-8428fee8).
