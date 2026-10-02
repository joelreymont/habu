---
title: Capture does> in Habu
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.930638+03:00"
closed-at: "2026-10-01T12:56:31.000000+03:00"
close-reason: "done: src/habu/definers.f captures does> as CAPTURE-DOES does (DOESB, the created signature copied to here for the new created-sig! prim, second-does> and bad-signature refusals); bin/hb --load test/outer-interpret.f agrees with the engine on 150 cases (10 new, failing on the base loop) on spark with the rebuilt engine (gen1 == gen2 9204fc56); engine-writers ok; x86-64-kernel suites rc 0, 27 definition images exit as documented."
---

Problem: `does>` capture is the assembly `CAPTURE-DOES` (`habu2.f:7495-7507`).
Acceptance: `CAPTURE-DOES` semantics: `DOESB-CELL`, `C-PARSE-CREATED-SIG`, the second-`does>` refusal.
Files: `src/habu/definers.f`, `src/habu/outer.f` (compile-mode dispatch), cases beside `test/outer-interpret.f`.
Verify: spark: `does>` cases through the Habu loop under the feature cell, the second-`does>` refusal included; gate.
Depends: habu-compile-immediates-from-cc47ecf4 (I5c).
Route: Alder (shared: src/habu/definers.f, src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-09-30, from the I8/I5a design): TCSIG-A/U sit in the friend arena (a store exits 83 after the seal), so I5d adds a TCSIG twin of `trust-sig!` in prims.f with bodies on both targets.
