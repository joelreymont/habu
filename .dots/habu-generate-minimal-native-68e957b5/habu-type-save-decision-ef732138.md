---
title: Type save decision
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:26:51.662325+02:00"
---

maki/saved.f:47-100 models the closed save-versus-recompute decision as raw SV-SAVE/SV-RECOMPUTE integers. SAVED-DECIDE returns n, SV-ROW$ stores it in an n local and branches by equality, so decisions can be confused with costs, op metadata, precision, or other two-valued domains and future variants need untracked comparison edits. Declare a save-decision ENUM with save and recompute variants, return it from SAVED-DECIDE, and render/consume it through exhaustive MATCH. Coordinate habu-structure-op-metadata-7fec08bf so ADJ-SAVE is also typed, and habu-structure-store-query-63edd08e so calibration lookup uses option rather than a trailing boolean. Preserve policy floors, cost formulas, calibration/default behavior, report bytes, graph decisions, and emitted backward code. Add checker negatives for raw n/cost/adjoint-save/foreign-enum swaps; exhaustive tests cover model inputs, floor, lower/equal/higher costs, calibrated/default ratio, and exact report goldens. Measure JIT/DATA/CODELEN and planner throughput before/after. Files: maki/saved.f and focused tests/consumers. Verify saved/autograd/backward/report suites, Maki, typed-local diff, type/package/host/dot lints, and full native gate. Ownership: save decision domain only.
