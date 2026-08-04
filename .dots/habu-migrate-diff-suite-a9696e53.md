---
title: Migrate diff-suite build and decode sums
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T14:22:45.941562+02:00"
close-reason: "Landed as cf3586da4976."
---

Wave C8 of the unified-type migration program (.blackboard/migration-plan-20260726.md - recipe, rules R1-R7, STOP conditions; landed precedents A1 c85210fe5817 and C5). maki/db/diff-suite.f:117 build-result 0 (ok suite + 3 payloadless) and :131 decode-result 0 (ok suite + 4 payloadless) become full-mode payload ENUMs, FIELD names justified from source (the C5 diagnostic lane migrated the same-shaped pair with the same tails - copy its cross-package production-collision negative pattern: DIAG and OBLIG declare the same tails). Spellings byte-identical via calibrated verdict tables; consumers untouched, suites run; REFLECT pins per R7; FIELD-removal kills; non-zero discipline; name-cliff precheck; new pin blocks BELOW T-RESET (the D3 trap). Acceptance: diff-suite suite + consumers + maki/test.f; both diff lints; census verify identical.
