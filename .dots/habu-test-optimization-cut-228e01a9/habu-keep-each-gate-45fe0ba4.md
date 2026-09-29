---
title: Keep each gate row under half its deadline in the pool
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T13:58:20.612704+02:00"
---

Problem: the registry rule (test/gate-stdlib-cases.f:1666-1676) keeps a row under half of SUITE-TIMEOUT-MS (360 s, test/gate-stdlib-lib.f:12) measured solo, but rows run 2.1-2.5x slower in the pool: hb-build-fixtures 285 s, hb-build-stripped 265 s and hb-build-stripped-cells 288 s against solo 108-133 s, leaving 20-26% margin. Under external load in native-gate-serial-floor.log (lines 1249-1261, 1339-1341) three such rows were killed at 360 s; the outcome line's code 0 is the pool's placeholder for a timed-out slot (gate-pool.f:206-211), not an exit status. Acceptance: in a gate run after the sibling dots land, every row's pooled time is under 180 s; a row above it is split by moving a fixture group into a new row file, as the registry comment prescribes; the registry comment states the rule in pooled time. The deadline and the slot count stay unchanged. Files: test/gate-stdlib-cases.f and the row files that are split. Verify: PASS durations in that gate log; every split row still runs every fixture it ran before. Depends: the three sibling dots. Ownership: row composition of the rows above 180 s. Claim: agent=kestrel workspace=.jj-ws/row-split (workers: .jj-ws/rs-stripped, rs-hb-build, rs-fixpoint, rs-aot-window).
