---
title: Type repair outcome
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:27:26.234155+02:00"
---

maki/eval/repair-mech.f:231-248 returns MECH-GREEN/MECH-UNREPAIRABLE/MECH-CAPPED as raw 0/1/2. The outcome can be interchanged with round counts, grader verdicts, packet classes, and any n; callers are not statically forced to handle all termination cases. Declare a repair-outcome ENUM, return it from MECH-REPAIR, and consume it through exhaustive MATCH in reports/evaluators. Keep MECH-MAX-ROUNDS numeric. Preserve packet-directed edit behavior, round/token tallies, cap ordering, existing unrepairable capability gaps, and rendered results. Add checker negatives for raw n/count/grader/foreign-enum swaps; exhaustive tests cover immediate green, each editable path, too-little packet data, exact cap, and all current repair packet classes. Measure JIT/DATA/CODELEN and repair-loop overhead before/after. Files: maki/eval/repair-mech.f and direct tests/consumers. Verify repair-mech/eval/transcript suites, Maki, typed-local diff, type/package/host/dot lints, and full native gate. Ownership: repair termination domain only; packet capability dots remain separate.
