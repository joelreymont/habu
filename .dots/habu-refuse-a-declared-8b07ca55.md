---
title: Refuse a declared cell pointing below the restored span
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T18:24:32.554469+03:00"
---

Problem: src/habu/aot-closure.f XTC-ROW (habu-let-a-stripped-0a064bf5) skips DATA-kind address-cell rows on the ground that DATA is restored MAP_FIXED to the same addresses, but a declared DATA-kind cell inside the capture window whose value points BELOW the window (a preloaded module's data the stripped image never restores) is checked by nothing and dangles at run time; only cells reached through a recorded address chain get REFUSE-UNRESTORED-CELL. Acceptance: a DATA-kind row inside the window whose non-zero value falls outside [BLOB-SRC, BLOB-END) is refused by name with the owning word, DATA offset and value (the unrestored-cell diagnostic), with a rejected fixture that stores a pointer to a preloaded module's variable in a declared cell; a value inside the window keeps passing. Files: src/habu/aot-closure.f, test/compiler/aot-data-cell-refusals.f. Verify: the fixture; test/gate-aot-negative.f; test/run.f. Depends: habu-let-a-stripped-0a064bf5. Ownership: AOT linker. Claim: unassigned.
