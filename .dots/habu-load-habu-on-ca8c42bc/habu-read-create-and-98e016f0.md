---
title: Read create and variable in the Habu loop
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-10-09T01:01:19.217348+03:00\\\"\""
closed-at: "2026-10-09T16:04:04.480318+03:00"
---

Problem: src/habu/outer.f:218-220: the Habu loop reads neither `create` nor `variable`. Under test/outer-loop-on.f `variable` refuses E-UNDEFINED (70), and `create` falls through to the runtime primitive (src/habu/prims.f, habu1.f:1631), skipping the def hook and the `-- ptr a` trust-raw row the engine's C-CREATE publishes (habu2.f:4795-4807). a02, r02, r03 and r07 of the stage-2 cases use `variable`.
Acceptance: prims.f gains the OUTER-owned writer row `def-create ( -- )`: with a pending DKIND:ADDR definition it rounds DP to a cell under DP-CHECK (76) and finishes the record as the engine's create does (code pushing that address with its address-map site, length, LASTC, native origin, record appended, window closed, flushed); exit 83 for no pending definition or another kind, CP at the ceiling. Its ARM64 body is the tail of EMIT-CREATE (habu2.f:4783-4794) factored into one DEFWRITE emitter that C-CREATE also calls; x86 registers it REFUSE (kernel-x64.f:187-189, no x86 interpreter); a Gforth refusing body if src/host/gforth/prims.fs exists when this lands (dot habu-hand-the-rest-32631946 gives the real one). definers.f reads `create` and `variable` before the word lookup in the engine's order: task guard (79), room, the name (74 naming the keyword), the capture seeded with the name, DEF-QUALIFY and DEF-RECORD with DKIND:ADDR, `def-create`, one cell more for `variable` (no store, habu2.f:4810), the def hook over the capture with its verdict dropped, then `-- ptr a` through trust-raw to the active owner and the target unless the same (habu2.f:3025-3037). E2E: test/outer-interpret.f cases under BOTH: a variable stored and read by a checked word; `create B 16 allot` read by a checked word; a missing name; the hook's event; create and variable in TASK-LIVE-KEYWORDS. test/engine-writers.f: the row's refusals.
Files: src/habu/prims.f, src/habu/habu2.f, src/habu/kernel-x64.f, src/habu/definers.f, src/habu/outer.f, src/host/gforth/prims.fs (only as above), test/outer-interpret.f, test/engine-writers-child.f and test/engine-writers-prepare.f (test/engine-writers.f's cases), docs/x86-64.md (writer-row table :2098-2104), docs/architecture.md (Interpreter Now).
Verify: rebuild bin/hb per docs/gate.md; `bin/hb --load test/outer-interpret.f`; `bin/hb --load test/engine-writers.f`; `bin/hb --load test/run.f`; two-generation build converges.
Depends: none. Ownership: the files above. Worker: worker. Claim: agent=worker (lead carl) workspace=.jj-ws/carl-loopdef.
