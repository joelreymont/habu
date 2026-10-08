---
title: Hand the rest of the input to the Habu loop
status: open
priority: 1
issue-type: task
created-at: "2026-10-08T23:11:23.402173+02:00"
blocks:
  - habu-compile-checked-definitions-6e291539
  - habu-compile-definer-bodies-1292d049
  - habu-mark-immediate-and-7ce84436
---

Problem: docs/bootstrap.md stage 2: once the Habu interpreter is loaded it reads the rest. The Gforth boot stream never loads src/habu/interpret.f; src/habu/definers.f:216-220,380-386 compiles through NCOMP-DISPATCH:XT-CELL and fails closed when it is unset.
Acceptance: src/host/gforth/boot.fs requires src/habu/interpret.f after the stream, stores the Gforth codegen's compile entry ( ptr u8 n -- ) in NCOMP-DISPATCH:XT-CELL, binds SOURCE-ROOT:INCLUDE-INTERPRET as test/outer-loop-on.f:28-45 does, and reads every remaining argument file through OUTER:INTERPRET. All of test/gforth/cases/ run that way and match native; cases test/outer-interpret.f refuses on bin/hb join test/gforth/cases/ and are refused the same way. A row the interpreter dots add gets a Gforth body (the kernel dot's row check names a missing one).
Files: src/host/gforth/boot.fs, src/host/gforth/prims.fs, test/gforth/cases/, test/gforth/host-test.f, docs/bootstrap.md (stage 2 names `bin/hb --load test/gforth/host-test.f` and its counts in place of the prototype sentence).
Verify: `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`; `bin/hb --load test/outer-interpret.f` unchanged.
Depends: the Gforth codegen dot; habu-compile-definer-bodies-1292d049 and habu-mark-immediate-and-7ce84436 (the Habu loop refuses variable, constant, create, defer, immediate and cast: today, and r02, r03, r07 use variable). Ownership: the files above. Worker: worker-max. Claim: unassigned.
