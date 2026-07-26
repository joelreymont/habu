---
title: Migrate target descriptor and id-result
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T11:05:08.507288+02:00"
---

Wave B4 of the unified-type migration program (.blackboard/migration-plan-20260726.md). maki/target/target.f:35 SUMTYPE descriptor 0 has VARIANT value with SIX positional payload cells - the naming-heavy case: migrate to full-mode payload ENUM with six named FIELDs whose names come from what target.f actually stores (read the accessors and consumers first; short names per docs/forth.md naming; record the name choices and their sources in the report). Also target.f:44 id-result 1 per the A1 recipe (FIELD id a). Constructor spellings byte-identical, consumers untouched, A1 test pattern plus payload-order round-trip pins for all six fields (a swapped-field mutation must red - run it). STOP conditions per the program plan; six same-typed cells is exactly where a silent reorder hides, so the round-trip must pin each field to a distinct value. Owner: package in maki/target/target.f. Acceptance: target suite and maki/test.f green; both diff lints; census verify identical. Claim: agent=mig-b4 workspace=.jj-ws/habu-mig-b4
