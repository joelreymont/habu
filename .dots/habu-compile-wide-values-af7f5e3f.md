---
title: Compile wide values in a native does> split
status: open
priority: 1
issue-type: task
created-at: "2026-10-09T22:14:15.388949+03:00"
---

Problem: native refuses a does> definer whose clause or head holds a value wider than a cell, exit 75 (src/habu/habu2.f:11104-11118 EM-P2-TRIGGER, P2DOESW-MSG$): its pass-2 re-run checks the split body in two phases that index tokens differently, so a width-aware re-run cannot align. This is a limit of the engine, not a language rule. Where pass 2 does not run, native compiles a wide transport in a does> clause as one cell, silently: `: MK ( n -- ) create , does> ( trip -- n trip ) @ swap ; 7 MK X  1 2 3 TRIP:MAKE X TRIP:UNMAKE . . . .` prints `3 7 2 1` on bin/hb, while `: Y ( trip -- n trip ) 7 swap ;` and the Gforth host print `3 2 1 7` (~/.cache/tmp/heron-arm64/evidence/gfcodegen/probes/r3/s-does-tr2.f, s-base.f). The Gforth host checks and compiles the same definer in one pass and runs it: /Users/joel/.cache/tmp/heron-arm64/evidence/gfcodegen/probes/i8/d5.f (wide swap and nip in clause and head) prints 19 20 21.
Acceptance: native compiles and runs a does> definer with wide values in clause and head, d5 prints 19 20 21 on bin/hb, s-does-tr2 prints `3 2 1 7`, and the rc-75 refusal and its message go. d5 joins the native suite and test/gforth/cases/, matching.
Files: src/habu/habu2.f and the pass-2 transaction code it names, the native tests a search finds for EM-P2-TRIGGER, test/gforth/cases/.
Verify: rebuild bin/hb per docs/gate.md; `bin/hb --load test/run.f`; two-generation build converges; `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`.
Depends: none. Ownership: the files above. Worker: worker-max. Claim: unassigned.
