---
title: Add PTX public-surface leak tests
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:30:21.941105+02:00"
blocks:
  - habu-add-ptx-target-ba119d76
  - habu-wire-ptx-independence-87f54e59
---

File: PLAN.md:259; cause: generic PTX APIs can expose workload names, Orin constants, fixed CUDA paths, or roof literals while still passing functional tests; fix: add token-aware public-surface leak tests that reject SAXPY, TRIAD, Orin, sm_87, fixed CUDA paths, and roof constants in generic PTX public words except whitelisted smoke fixtures; deps: target capability data and PTX independence lint; verification: focused profile/bench/tool lint tests fail on injected leaks and pass with smoke-only adapters.
