---
title: Migrate evidence diffsuite numpolicy id-results
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T11:05:08.501026+02:00"
---

Wave B2 of the unified-type migration program (.blackboard/migration-plan-20260726.md). Same contract shape as wave B1 (see habu-migrate-config-producer dot and the landed A1 recipe c85210fe5817), applied to maki/db/evidence.f:56, maki/db/diff-suite-id.f:44, maki/numpolicy.f:148. Constructor spellings byte-identical, consumers untouched, per-file A1 test pattern with public twins, same STOP conditions. Owner: the three files existing packages. Acceptance: paired suites and maki/test.f green; both diff lints; census verify identical. Claim: agent=mig-b2 workspace=.jj-ws/habu-mig-b2
