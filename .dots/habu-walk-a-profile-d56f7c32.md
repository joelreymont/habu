---
title: "Walk a profile sample's stack to the main stack's top"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T07:24:10.059507+02:00"
---

Problem: C-PROF-WALK (src/habu/prof.f:169) scans at most PROF-WALK-CELLS (64 cells, src/habu/prof-abi.f:82) and never past SP's 4 KiB block, so inclusive counts fall far short: the word a whole program build runs under took 42 inclusive samples of 37,215 (docs/debugging.md Sampling profiler). Acceptance: in a warm engine build under tools/build-profile.f, the word that runs the whole build has inclusive samples within 5% of the Habu samples taken while it ran; profiling overhead stays within the documented tenth. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-prof-b.
