---
title: "V2 R3: type region lowering chain"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T07:41:45.031596+02:00"
blocks:
  - habu-v2-r3-type-69e81081
---

Problem: all lowerers and launch registries accept fusion region ids as raw n, so wrong-role ids can reach emission, cubin lookup, and execution. Fix: migrate lower-ew/red/mm/move/launch analyze, emit, run, membership, MDL-STAGE, MDL-DISPATCH, MDL-CUBIN!, and region predicates to CAD-KIND:region; preserve raw numeric projection only at bounds-checked arrays and REGION_<id> rendering with private audited boundaries. Acceptance: typed node -> FP-RID@ -> typed analyze -> emit -> launch chain passes; effect/stage/node values reject in every public region entry; emitted PTX and device goldens remain byte/element exact. Files: maki/lower-ew.f, lower-red.f, lower-mm.f, lower-move.f, lower-launch.f and focused tests including lower-model/device; TRUSTED.md. Verify: lowerer focused tests, typed-local diff lint, trust-lint, maki/test.f, PTX stdlib/native device slices, host-lint, filemap-lint. Depends: typed fusion region owner and region analysis consumers.
