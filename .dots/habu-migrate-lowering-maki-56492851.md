---
title: Migrate lowering Maki count callers
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T14:14:22.918215+02:00\""
---

Full context: migrate frozen B5 MIR count callers owned by lowering/golden: maki/lower-ew.f, maki/lower-mm.f, maki/lower-move.f, maki/lower-red.f, maki/lower-launch.f, maki/lower-golden.f, maki/lower-model-test.f, maki/golden.f, maki/golden-artifact.f, and maki/golden-artifact-test.f plus focused tests. Replace every old node/slot/operand/materialized count accessor in each owned file with MIR typed counts. Acceptance: lowerer/model/golden tests and PTX bytes exact; no overlap with core/training/sched-key waves; per-file census empty. Depends on MIR count APIs.

Claim: agent=mircount workspace=.jj-ws/fable-mircount (tri-dot lane; per-dot disjoint file ownership per the dots themselves)
