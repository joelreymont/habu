---
title: Migrate diagnostic build and decode sums
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T12:42:17.667482+02:00"
close-reason: "Landed as 887763b5d111."
---

Wave C5 of the unified-type migration program. maki/db/diagnostic.f:105 build-result 0 (ok diagnostic + 2) and :111 decode-result 0 (ok diagnostic + 5), consumed across diff-runner/diff-suite/action. Full-mode payload ENUMs, FIELD names justified from source; spellings byte-identical via calibrated verdict tables both trees; consumers untouched, their suites run. A1 pattern + FIELD-removal kills; non-zero payload discipline. STOP conditions per plan; watch the name cliff. Acceptance: diagnostic suite, consumer suites, maki/test.f green; both diff lints; census verify identical (all full-form).
