---
title: "V2 R3: type region analysis consumers"
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-12T07:41:37.208194+02:00\\\"\""
closed-at: "2026-07-13T01:18:52.031636+02:00"
close-reason: "Satisfied by 804bb7e9: traffic/sched-key/cad consumers take CAD-KIND:region; schedule.f verified region-free (FAM-SELECT takes classmix bitmask); canonical bytes unchanged (RSIG golden + full key pins byte-identical); REGION_<id> render behind RGN>RAW boundary"
blocks:
  - habu-v2-r3-type-5c26f1cb
---

Problem: traffic, schedule, key, and CAD analysis signatures consume fusion region ids as n after the owner planner. Fix: migrate only semantic region parameters/locals in maki/traffic.f, maki/sched-key.f, maki/schedule.f, and maki/cad.f to CAD-KIND:region; preserve GEMM stages=1/2 and STAGE-AXIS as numeric pipeline-depth counts, not artifact-stage identities; raw projection is permitted only at canonical REGION_<id> rendering with a private audited boundary. Acceptance: region/effect and region/stage swaps reject; canonical schedule/report bytes remain unchanged; all public region handles are nominal. Files: those sources and traffic-test.f, mem-plan-test.f, sched-key-test.f, schedule-test.f, cad-test.f, TRUSTED.md. Verify: focused tests, typed-local diff lint, trust-lint, maki/test.f, host-lint, filemap-lint. Depends: typed fusion region owner.
