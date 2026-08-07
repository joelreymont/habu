---
title: Prove a selected slice is actually spawned
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T13:43:34.711755+02:00"
---

tools/lint/schedule-lint.f closes half the scheduling hole: it proves every SUITE registration in test/gate-stdlib-cases.f is reachable by a slice predicate in test/gate-stdlib-lib.f or by a GSI fork list under test/. It does NOT prove the second conjunct - that the slice a predicate belongs to is spawned by a phase of test/run.f. That gap was live: the 'tail' slice existed, SUITE-TAIL? selected five labels, and phase 4 was in none of test/run-lib.f's ordering tables, so those five suites ran nowhere while looking scheduled (found and fixed 2026-08-07 by adding $4 and $28 to TR-EARLY-HOST-ORDER). Nothing stops it recurring: delete a phase id from TR-EARLY-HOST-ORDER and every suite in that slice goes dark with a green gate and a clean schedule-lint. Extend schedule-lint (or add a sibling) to derive, from test/run-lib.f, the map slice-token -> phase id (TR-PHASE-RUNNER-TOKEN / TR-PHASE-ARGS) and the set of scheduled phase ids (TR-EARLY-HOST-ORDER, TR-LATE-ORDER, TR-CANDIDATE-HOST-ORDER plus the direct starts in test/run-resident.f), and RED when a predicate selects labels for a slice whose phase nothing starts. Falsify it by removing one phase id from an order table and checking the named RED.
