---
title: Compile checked definitions on Gforth from events
status: active
priority: 1
issue-type: task
created-at: "\"2026-10-08T23:11:00.276267+02:00\""
---

Problem: docs/bootstrap.md stages 1-2: the Gforth platform's codegen compiles each checked definition from its events at ;, a value of several cells as that many Gforth cells (docs/architecture.md "The codegen is one pass over the checked events"). The prototype's ~/.cache/tmp/stage2-proof/codegen.fs (545 lines) does this but reads checker variables and owner offsets by number.
Acceptance: src/host/gforth/codegen.fs, one pass over the events through a handler per kind, reading only the event dot's events and owner-ABI rows: calls through the binding row's record; a core-op transport of several-cell values as a permutation from its call and glue rows; locals as one (local) per cell; construct as pad zeros then the tag; MATCH as case/of/endcase with the bad-tag death (exit 85); loops with native do/?do/loop/+loop semantics; ['] and is from their recorded record, with a case ticking a word declared but not yet defined (its K-TICK a0 is -1: LIVE-BIND, checker.f:12654-12669, leaves BIND-REC null for a pending declaration), and a checked word that calls and ticks `finally`, the host's record 0. reader.fs's : row captures the body without running a token and arms the tape; a does> definer follows the native order (compiler.f CHECK-DOES-SPLIT, feed.f:312-348). boot.fs then loads the whole boot stream (62 files and the seal texts, habu2.f:1845-1862), exit 0, past lib/memory.f:260. A row the rest of the boot stream reaches gets a Gforth body in prims.fs, `evaluate-closed` among them if the stream reaches it (src/core/include.f:1218 INCLUDE-EVALUATE): its text is engine source, read by the host's reader as LOAD-FILE reads a file, and a failure in it leaves through the host's exit path. test/gforth/cases/ gains the 8 accepted cases a01-a08 and the 12 corpus programs c01-c12 from the prototype; every case there matches native rc, stdout and JSON, a07 included.
Files: src/host/gforth/codegen.fs, src/host/gforth/reader.fs, src/host/gforth/prims.fs, src/host/gforth/boot.fs, test/gforth/cases/, test/gforth/host-test.f.
Verify: `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`; artifact $HB_TMP/gforth-host/.
Ownership: the files above. Worker: worker-max. Claim: agent=worker-max (lead carl) workspace=.jj-ws/carl-gfcodegen.
