---
title: Lint positive throw codes for collisions
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T12:20:51.613744+02:00"
closed-at: "2026-10-14T19:30:30.919394+03:00"
close-reason: "Implemented in 63f2812c: the error-code lint checks positive throw codes above exit statuses for global collisions and reservations; reported collisions were resolved and lint, focused suites and rebuild convergence passed."
---

Problem (lane 298 declprobe): tools/error-code-lint-core.f:25-26 skips every positive code on the ground that positive E- values are sysexits-style exit codes (64/70/74/76) shared by design. That reason covers 64-78, not the positive throw codes the checker and core mint (7110, 7177, ...). tools/decl-gen-probe.f's E-PROBE-FAMILY reused 7177, which src/core/generated-declaration.f:418 mints as DECL-REPLAY:E-REPLAY-BUSY, and the lint passed (1414 files, 0 findings). A catch on 7177 cannot tell the two apart. Acceptance: the lint holds positive throw codes outside the exit-code range to the same global uniqueness as negative ones (exit codes stay shared); a fixture with two E- names on one such code fails it, seen failing first; every existing collision it then reports is fixed or its allowance stated. Files: tools/error-code-lint-core.f, its test.
