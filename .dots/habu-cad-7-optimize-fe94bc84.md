---
title: "CAD 7: OPTIMIZE loop + EXPLAIN packets"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:50.073238+02:00"
---

docs/model-cad.md Phase 7. OPTIMIZE composes lower->fuse->memory->tile->certify->golden->gradcheck->profile->promote with gates enforced; PROMOTE refuses on any failed required gate; regression detected vs cached baseline. EXPLAIN failure packet: failure class, location, expected/actual contract, suggested repair family, minimal repro (shape of maki/eval-repair.f packets). Profile/roofline rows per kernel (device time, GB/s, GFLOP/s, intensity, class, limiting resource, next-move). Device phases run on Orin. Related: habu-kernel-artifact-export, habu-committed-device-correctness. Depends: cad-0b cad-2 cad-3 cad-4.

UPDATE (plan-review fold, 2026-07-04): also owns (1) the host model-IR reference executor: topo walk of the cad-1 node table calling each op registry scalar reference on host tensors - the GOLDEN composition oracle; (2) the external reference-artifact loader + on-disk format (tensor dump + per-artifact tolerance), LA-workload driven; (3) PROMOTE gate set = CERTIFY + GOLDEN (+ GRADCHECK when backward exists); PROFILE mandatory-to-run, non-blocking (align cad.f PROMOTE-OK? when this lands). OPTIMIZE records the promote decision, never throws; standalone PROMOTE throws. Depends adds: habu-cad-5-artifact-9a3d5a56.

UPDATE (cad-1 merge, 2026-07-05): also owns OPTIMIZE-time shape binding (north-star OPTIMIZE FFN SHAPE batch=.. TARGET ..): bind/override input extents at OPTIMIZE instead of only MODEL: signature literals; unbound extents render ? until bound; re-binding re-plans downstream (CAD-PLAN section 13).
