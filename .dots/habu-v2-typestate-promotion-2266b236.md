---
title: "V2 typestate: promotion transition + store seal"
status: open
priority: 2
issue-type: task
blocks:
  - habu-v2-typestate-report-df8e34fa
  - habu-v2-typestate-store-57afdc0a
created-at: "2026-07-13T16:17:36.146240+02:00"
---

Implement sub-dot 4 of the R7 typestate addendum: MODEL-CAD-V2-PLAN.md:1679-1689 (design at 1280-1643). ART:PROMOTE becomes the ONLY store-row writer; EVID-PUT (maki/store.f:355) and SCHED-PUT (:293) leave the public surface or take typed evidence; closes the planted-evidence replay path (TILE-REPLAY maki/cad.f:943, PROMOTE-EVIDENCE cad.f:1046-1054). Acceptance: incomplete-plan fixture verdict 0 (plan:1597-1607); the forged-promotion probe (three/two GATE! writes satisfying PROMOTE-OK?) becomes unrepresentable or rejects; store bypass regression committed. Verify: typestate-test suite, maki/test.f + store/sched-key/cad focused tests, typed-local-diff-lint. Depends: habu-v2-typestate-promotion-d539e648. Ownership: maki/store.f public surface, maki/cad.f promote path. Claim: unassigned.
