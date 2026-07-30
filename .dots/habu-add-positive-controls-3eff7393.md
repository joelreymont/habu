---
title: Add positive controls for substitute nominals
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T22:26:32.204105+02:00"
---

Destruction finding F4 on the M8 dead-nominal commit: four of the five repaired test files (maki/model-ir-test.f, maki/target/target-test.f, maki/fusion-plan-test.f, maki/lower/model-test.f) carry wrong-type rejection negatives whose substitute nominal has no FILE-LOCAL positive control proving it resolves. If the substitute's declaration is ever lost or misspelled, every negative keeps returning 0 and goes green while testing an undefined word instead of the type system. This hole is pre-existing (node-id had it before the M8 repair) and is currently mitigated tree-wide by cad-kinds-test.f's CK-* positives plus verified require paths. Exact behavior: add one -1 positive control per substitute nominal per file that uses it in a negative — an empty-body identity candidate of the shape s" NAME ( CAD-KIND:<sub> -- CAD-KIND:<sub> ) " CHECK-QUIET-CANDIDATE! -1 T= — adjacent to the negative block it protects. Owner: each test file's owning suite. Acceptance: for every CAD-KIND nominal used as a wrong-type operand in a negative anywhere, the same file carries its resolve positive; removing any nominal's declaration flips at least one positive in every file that probes with it. Run in the M17 battery with the rest. Lesson recorded in LESSONS.md: a fixture substitute must be production-load-bearing, and its resolution must be pinned file-locally.
