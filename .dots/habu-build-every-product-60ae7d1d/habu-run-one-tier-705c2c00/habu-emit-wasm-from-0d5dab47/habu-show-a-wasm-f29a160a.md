---
title: "Show a Wasm trap's fault and test the stack bounds"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T19:22:00.017927+02:00"
---

Problem (feature review A, F2 and F5): src/habu/t0-wasm.f ROOM and BOUND and src/arch/wasm/link-routines.f STACK-ROOM, and ADDRESS-CHECK, record ctx's fault kind and address, then trap; nothing reads them (test/wasm/run.mjs, harness.f, src/lib/browser/turn.js), so every trap is status 2 and a module built without the checks passes every test. Acceptance: run.mjs prints the fault kind and address after a trap and harness.f answers them; test/wasm/hb-build.f asserts ENGINE-ERROR:STACK-BOUNDS for a recursion that pushes a cell, one after >r and one inside a do, and CRASH-RC with address 8 for low.f. Then, with the fault visible: die's code, a die message past OUT's room, emit on a full OUT, execute of an xt with a nonzero upper half, a store at or past 2^32. Files: test/wasm/run.mjs, test/wasm/harness.f, test/wasm/hb-build.f. Verify: the test ends test: ok; each new case fails with its check removed. Depends: none. Ownership: those files. Claim: unassigned.
