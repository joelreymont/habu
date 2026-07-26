---
title: Migrate config producer rev schema id-results
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T11:05:08.489365+02:00"
---

Wave B1 of the unified-type migration program (.blackboard/migration-plan-20260726.md). Migrate the SUMTYPE id-result 1 declarations in maki/config.f:66, maki/producer.f:67, maki/rev.f:61, maki/schema.f:68 to full-mode payload ENUM, copying the landed A1 recipe exactly (master commit c85210fe5817, maki/journal.f): FIELD name id for the ok payload, constructor spellings byte-identical (each package's <PKG>-ID--RESULT:OK family), MATCH sites and consumers untouched. Per file: the A1 test pattern - constructor effect pins via the checker, forge negatives (raw cell, dropped payload, same-width foreign role), a public same-shape twin family with non-unification negatives (twin must be public or the negatives pass vacuously - A1 lane finding), construct-then-MATCH factored one-word-per-variant (the ground-scrutinee checker rule, tracked habu-checker-ground-match-c0cb9d44). STOP conditions per the program plan. Owner: the four files existing packages. Acceptance: the four paired suites and maki/test.f green; typed-local and package diff lints on the diff artifact; enum-census verify baseline identical (full-mode sites are counted, not byte-compared). Claim: agent=mig-b1 workspace=.jj-ws/habu-mig-b1

Closed 2026-07-26: landed as e325ec6865ff on master@origin. Spelling preservation proven by a 28-row calibrated checker verdict table run identically against both trees; per-file FIELD-removal mutation kills; found and fixed the index-zero payload round-trip vacuity in all four suites.
