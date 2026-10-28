---
title: Delete NHOST seeding and the host invalidate cell
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T23:54:01.935726+02:00"
---

L3-C5. Joel 2026-10-28 rulings (C4 review): with NHOST:ID-OF gone nothing reads seeded or published implementation rows. Delete end to end: NHOST SEED-ROW/SEED-SCALAR/SEED-DIVISION, KERNEL-OWNER, INVALIDATE and its hook; engine cells NATIVE-HOST-CELLS:SCALAR-PRIM $2D28 and DIVISION-PRIM $2D40 with their startup stores, label rows and primitive-registry marks (arm64 habu1/habu2, x86-64 boot-x64/kernel-x64); NATIVE-OBS-CELLS:HOST-INVALIDATE $2D10 with its claim, snapshot zero, layout row and kernel calls; IMPL table becomes one-take staging (PUBLISH-TAKEN truncates). Also remove PREPARE's entry argument and its feeders (NHOST:TAKE at, PREPARE-IMPORT base, TAKEN-AT, IMPORT-BASE), changing those public effects. Older hosts' $2D10 xt row dropped in CHECK-HOST-LAYOUT. Work: qyrnnlqs on onrqnwsp in .jj-ws.noindex/jerry-c5 (worker). Verify: arm64 gen-1/gen-2, x86-64 cross-build, full suite.
