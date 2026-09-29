---
title: Run every x86 compare relation natively
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T17:45:15.270462+03:00"
blocks:
  - habu-run-emitted-x86-b704f918
---


Problem: the x86 compare render is proven at run time for one relation only. C8's `CMPSET-IMAGE` and `CMPSETI-IMAGE` (`test/x86-64-peer-routines.f:163-179` on `wptxqqns`) run `<` natively in the register form and the folded-immediate form, each with true and false answers; the other relations the selector produces (`select-x64.f:185-190`: `<=`, `>`, `>=`, `=`, `<>`) are checked only as pinned bytes (`test/compiler/x64-emit.f`). A wrong condition code for any of them (e.g. `setg` for `>=`) would pass. Alder's review of the flag fix (`7be4db87`, all six relations through `PUT-SETCC`) asked for this runtime evidence.
Acceptance: for each of the six relations, a register-form and a folded-immediate-form HIR fixture (the `BUILD-CMPSET`/`BUILD-CMPSETI` shapes, `test/compiler/x64-emit.f:384-400`) runs natively through C8's family with cases on both sides of the boundary and at it (less, equal, greater; `MIN-CELL`/`MAX-CELL` for the signed order), expecting -1 for true and 0 for false, each image with its negative twin in the manifest. Before the change no native case runs `<=`, `>`, `>=`, `=` or `<>`.
Files: `test/compiler/x64-emit.f` (fixtures), `test/x86-64-peer-routines.f`.
Verify: ThinkPad: `bin/hb --load test/x86-64-peer-routines.f`, then every image in `$HB_TMP/x64-routines` natively against the manifest.
Depends: habu-run-emitted-x86-b704f918 (C8). Base: K2+C8, or C8's bookmark.
Route: direct once C8 is on master (x86-only test files).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-run-every-x86-a3663e4d.
