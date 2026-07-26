---
title: Migrate audit budget capability sums
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T12:42:17.677608+02:00"
---

Wave C3 of the unified-type migration program: three tiny files, one lane. maki/db/audit-log.f:86 verify-result 0 (mixed: broken-chain n, bad-nondeterministic n - two payload arms, FIELD names from source); maki/db/budget-ledger.f:53 budget-result 0 (ok / exhausted BUDGET:dim - the dim FIELD precedent is landed in C4, copy it; consumed by commit-store.f); maki/db/capability.f:59 attenuate-result 1 (ok a / escape-cap / escape-budget BUDGET:dim; consumed by agent-loop.f). Full-mode payload ENUMs; spellings byte-identical via verdict tables; consumers untouched, their suites run. A1 pattern per family + FIELD-removal kills + non-zero discipline (BUDGET:dim rides a non-zero ordinal per the C4 precedent). STOP conditions per plan. Acceptance: three focused suites, commit-store and agent-loop suites, maki/test.f green; both diff lints; census verify identical. Claim: agent=mig-c3 workspace=.jj-ws/habu-mig-c3
