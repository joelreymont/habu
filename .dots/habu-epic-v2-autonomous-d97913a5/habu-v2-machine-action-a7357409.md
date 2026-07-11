---
title: V2 machine action registry
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.021937+02:00"
blocks:
  - habu-v2-canonical-artifact-ee5121b4
---

Implement MODEL-CAD-V2-PLAN.md:1939-1953 action-schema registry. Each action declares checked input/output artifact kinds, preconditions, effects, capabilities, deterministic/cacheable flags, budget dimensions, obligations, verifier, diagnostics, and invalidation. Seed SCHEMA:LIST, ARTIFACT:GET, REVISION:DIFF, TX:BEGIN/APPLY/VALIDATE/COMMIT/ABORT, and PASS:RUN. Acceptance: missing declaration fields reject registration, wrong input kind cannot dispatch, unauthorized effects reject before execution, registry enumeration is canonical and replayable.
