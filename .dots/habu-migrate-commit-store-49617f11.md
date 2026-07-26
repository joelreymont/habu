---
title: Migrate commit-store result sums
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T11:47:20.728492+02:00"
---

Wave C4 of the unified-type migration program (.blackboard/migration-plan-20260726.md). maki/db/commit-store.f:86 commit-result 1 (4 arms), :100 auth-result 1 (6 arms, consumed cross-package by maki/db/agent-loop.f - consumers untouched, spelling preservation is load-bearing there), :118 commit-discharge-result 1 (8 arms). Full-mode payload ENUMs; every payload cell gains a FIELD named from what commit-store.f stores (justify each from source lines); payloadless arms unchanged. A1 test pattern per family (effect pins, forge negatives, PUBLIC twin, factored round-trips), FIELD-removal mutation kill per family, and for the 8-arm family a wrong-arm MATCH negative (checker exhaustiveness already enforces missing arms - pin one swapped-payload case). STOP conditions per the program plan. Acceptance: commit-store suite, agent-loop suite, maki/test.f green; both diff lints; census verify identical. Claim: agent=mig-c4 workspace=.jj-ws/habu-mig-c4

Closed 2026-07-26: landed as 77c76434f77d. Five payload cells named from source with per-cell production kills; found the 32-byte generated-name cliff (dotted).
