---
title: Migrate experiment run and metric records
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T11:47:20.714130+02:00"
---

Wave B3 of the unified-type migration program (.blackboard/migration-plan-20260726.md). maki/experiment/run.f:86 id-result 1 (A1 recipe, FIELD id a) and :94 seal-result 1 (VARIANT ok with one payload - name its FIELD from what run.f stores, justify from source lines); maki/experiment/run-metric.f:74 PRODUCT report-metric 0 and :78 PRODUCT objective-metric 0 (slot n each - STRUCTURE per the landed precedent 6ef124d0c64e, FIELD lines byte-identical; their nominal non-unification is the point and gets the twin negative), :85 objective-result 0 (payload variants named from source). Constructor spellings byte-identical (checker verdict tables both trees); consumers untouched; A1 test pattern per family incl. PUBLIC twins; one FIELD-removal mutation kill per sum and one field-order pin per STRUCTURE. STOP conditions per the program plan. Acceptance: paired suites and maki/test.f green; both diff lints; census verify identical. Claim: agent=mig-b3 workspace=.jj-ws/habu-mig-b3
