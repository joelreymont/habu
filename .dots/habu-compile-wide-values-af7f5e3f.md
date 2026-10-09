---
title: Compile wide values in a native does> split
status: open
priority: 2
issue-type: task
created-at: "2026-10-09T22:14:15.388949+03:00"
blocks:
  - habu-compile-checked-definitions-6e291539
---

Problem: native refuses a does> definer whose clause or head holds a value wider than a cell, exit 75 (src/habu/habu2.f:11104-11118 EM-P2-TRIGGER, P2DOESW-MSG$): its pass-2 re-run checks the split body in two phases that index tokens differently, so a width-aware re-run cannot align. This is a limit of the engine, not a language rule. The Gforth host checks and compiles the same definer in one pass and runs it: /Users/joel/.cache/tmp/heron-arm64/evidence/gfcodegen/probes/i8/d5.f (wide swap and nip in clause and head) prints 19 20 21.
Acceptance: native compiles and runs a does> definer with wide values in clause and head, d5 prints 19 20 21 on bin/hb, and the rc-75 refusal and its message go. d5 joins the native suite and test/gforth/cases/, matching.
Files: src/habu/habu2.f and the pass-2 transaction code it names, the native tests a search finds for EM-P2-TRIGGER, test/gforth/cases/.
Verify: rebuild bin/hb per docs/gate.md; `bin/hb --load test/run.f`; two-generation build converges; `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`.
Depends: the Gforth codegen dot. Ownership: the files above. Worker: worker-max. Claim: unassigned.
