---
title: Add PTX target capability records
status: open
priority: 1
issue-type: task
created-at: "2026-07-01T22:29:52.021225+02:00"
---

File: PLAN.md:33; cause: sm_87, CUDA 12.6 paths, ptxas, device caps, and roof constants still leak as hidden constants in generic PTX tooling; fix: add checked target capability records for arch, PTX feature flags, ptxas command, device limits, dtype support, and roof table, then thread them through planning, ptxas, launch, and profile rows; deps: none; verification: planner/profile tests reject missing target and public generic PTX words expose target data rather than Orin constants.
