---
title: Keep a Wasm module within 100000 data segments
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T19:18:46.595632+02:00"
closed-at: "2026-11-10T20:15:33.073855+02:00"
close-reason: "landed in master e4ec011f; its gate: engines converge, 12 focused tests rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem (feature review B, F5, reproduced): src/arch/wasm/link.f FIND-RUNS writes one data segment per run of non-zero bytes split at GAP-MIN 16 zero bytes, with no cap; `create T : LAY ( -- ) 100001 0 do 1 , 0 , 0 , loop ; LAY : MAIN ( -- ) T @ drop ;` links and validates, and bun refuses it: 'Data section's count is too big 100004 maximum 100000' (the JS API's limit). Acceptance: FIND-RUNS doubles the splitting gap until the runs number at most 100000, so the program above builds and runs to status 0. Files: src/arch/wasm/link.f, test/wasm/hb-build.f. Verify: the case in test/wasm/hb-build.f; engines converge. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-r2.
