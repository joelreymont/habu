---
title: Rows that build an engine inside the row die E-T0-SHADOW under the preload
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.356358+02:00"
---

Problem: six suite rows build inside the row and end in uncaught -9605 E-T0-SHADOW at src/habu/t0.f:1828 when the gate preloads test/t0-child.f (suite sort class c, 2026-10-30): the built child boots with the preload's T0 shadowing the engine's. Step 3 (T0 baked into the engine as COMP) removes the preload; these rows are its proof, with the 15 class (a) rows that pass without the preload and fail the same under 1 set-tier (the tests that change when T0 is the engine's default). Row names: .jj-ws.noindex/jerry-s2/.out/suite-t0.md. Acceptance: the six rows green with T0 as the engine's compiler and no preload. Files: test/t0-child.f, src/habu/t0.f, the six rows. Verify: bin/hb --load test/run.f green. Depends: step 3. Ownership: step 3. Claim: unassigned.
