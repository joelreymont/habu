---
title: "Report a top-level throw in check.f's own format"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T21:33:20.739635+02:00"
---

Problem: tools/check.f runs a program's top-level code in its own process, so an engine exit there bypasses check.f's records. /tmp/jerry-sealedrv-tmp/t84.f (`84 throw` at top level) makes check.f exit 84 with no output at all; ow.f (`2 set-current : OWNERFORGE ( -- ) ;`) and a sealed reopen inside `evaluate` print the engine's `E-SEALED-PACKAGE: ...` line instead of a check.f record, in prose and with --json-errors alike (sealed review, lane sealed on 7d3ea46b). Acceptance: every top-level throw or engine exit while check.f checks a subject becomes a check.f record (code, file, line, column, token; JSON with the same keys as other records) and check.f's own status; t84.f, ow.f and the evaluate case are rows in tools/check-verify-test-lib.f or tools/check-all-errors-test.f, failing first on the installed hb. Files: tools/check-core.f (where top-level code runs), src/lib/verify-diagnostics.f (THROW-CODE$ names the code). Verify: the three probes, rows check-verify and check-all-errors-test. Depends: lane sealed landed (habu-answer-tools-check-745b0ccd). Related: habu-check-src-through-1198f4ed, habu-run-check-f-30bef6ec. Claim: unassigned.
