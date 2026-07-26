---
title: Migrate extent tensor-kind to compact ENUM
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T10:31:34.900814+02:00"
---

Pathfinder A2 of the unified-type migration program (.blackboard/migration-plan-20260726.md): pins the payloadless-kind ruling R1. Target: maki/extent-tensor.f line 79, SUMTYPE tensor-kind 0 (variants data and gather, both payloadless) inside package MAKI. Ruling R1 (frozen): payloadless sums migrate to the COMPACT ENUM form; the type-registry kind deliberately changes from TK-SUM to TK-ENUM (width-identical); the lane adds a regression that pins the new kind so an accidental flip back turns the suite red, and consciously re-records any baseline that shifts (enum-census, public signatures), listing every re-recorded artifact in the report. Constructor spellings and MATCH sites must be byte-identical after migration; consumers untouched. Tests: both variants round-trip through MATCH; checker pins of the constructor effects; forge negative (raw cell where tensor-kind is expected rejects); nominal-identity negative against a second payloadless family; the kind-pin regression. STOP conditions as in the program plan: any checker miss reported per the Checker-Miss RCA protocol, constructor spelling drift, reserved-token collision. Owner: package MAKI in maki/extent-tensor.f. Dependencies: none; disjoint from active lanes (the tensor dot 900439a9 owns maki/tensor.f and tensor-value.f, not this file). Acceptance: the extent-tensor focused suite and full maki/test.f green on the exact tree; typed-local-diff-lint and package-diff-lint accept the diff artifact; identical accept/reject behavior plus the pinned kind. Claim: agent=mig-a2 workspace=.jj-ws/habu-mig-extent
