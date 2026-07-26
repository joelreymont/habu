---
title: Migrate diff-runner verdict sums
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T12:16:33.126948+02:00"
---

Wave C7 of the unified-type migration program (.blackboard/migration-plan-20260726.md). maki/db/diff-runner.f four families: :82 run-result 0 (produced n / faulted) and :88 ref-result 0 (value n / skip) become full-mode payload ENUMs with FIELD names justified from source; :94 case-verdict 0 and :102 run-verdict 0 (both fully payloadless, 4 arms each) become COMPACT ENUMs under ruling R1 with live-registry kind pins and conscious baseline re-record (compact sites enter the byte-compared baseline; enumerate the exact row delta). Spellings byte-identical; consumers untouched. A1/A2 patterns per family; FIELD-removal kills for the two payload families; kind-pin + arm-order mutations for the compact pair. STOP conditions per program plan; watch the name cliff. Acceptance: diff-runner suite and dependents through maki/test.f green; both diff lints; census verify with enumerated delta. Claim: agent=mig-c7 workspace=.jj-ws/habu-mig-c7

Closed 2026-07-26: landed as 308598b1d73b. Proved ordinal projections name-keyed and blind to arm reorders; program rules R4-R6 recorded; census delta enumerated and separability proven.
