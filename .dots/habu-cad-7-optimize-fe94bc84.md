---
title: "CAD 7: OPTIMIZE loop + EXPLAIN packets"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:50.073238+02:00"
---

docs/model-cad.md Phase 7. OPTIMIZE composes lower->fuse->memory->tile->certify->golden->gradcheck->profile->promote with gates enforced; PROMOTE refuses on any failed required gate; regression detected vs cached baseline. EXPLAIN failure packet: failure class, location, expected/actual contract, suggested repair family, minimal repro (shape of maki/eval-repair.f packets). Profile/roofline rows per kernel (device time, GB/s, GFLOP/s, intensity, class, limiting resource, next-move). Device phases run on Orin. Related: habu-kernel-artifact-export, habu-committed-device-correctness. Depends: cad-0b cad-2 cad-3 cad-4.
