---
title: "seal lib/test/budget.f's remaining globals"
status: open
priority: 3
issue-type: task
created-at: "2026-08-23T12:39:44.950384+02:00"
---

Problem: lib/test/budget.f keeps 13 unpackaged globals (T-BUDGET-* family) beside package TEST-BUDGET; package-diff-lint refuses any edit to them (measured 2026-08-23 by the host-timing lane: three E-PACKAGE-OWNERSHIP findings on a one-constant change), so new budget logic had to be added in-package with the globals left as they were. Sealing them is a 17-consumer, ~72-reference cascade, and the T-BUDGET- spellings cannot simply become TEST-BUDGET: tails under the redundant-prefix rule. Acceptance: the globals move into the package (renamed to drop the repeated prefix where the rule requires), consumers import with using, no forwarding shims, budget-test and every consumer suite green, both diff lints green. Files: lib/test/budget.f, its 17 consumers. Verify: the suites. Depends: habu-runner-budgets-uncalibrated-cb11c328 (landing). Ownership: test harness. Claim: unassigned.
