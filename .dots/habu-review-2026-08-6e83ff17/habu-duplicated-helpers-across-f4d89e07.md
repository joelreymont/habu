---
title: duplicated helpers across lib
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.957256+02:00"
---

Problem: measured copies: decimal digit printer x13 in lib (fs-mutate.f:207, content-key.f:371,570, object.f:154, render.f:549, codegen.f:593, test/record.f:211, test/spawn-report.f:181, test/runner.f:212, json-write.f:182) +59 files outside while FMT:SB-U exists (fmt.f:39-42); TRUE/FALSE redefined in 18 lib files (+40 outside) while prelude.f:28-29 defines them; STR= clones (test/assert.f:103 T-STR=, argv.f:261 ARGV-BYTES=, tools/bundle-lib-core.f:72, srclist.f:22, seed.f:63, checker.f:3786); HEX? x4, HEX-NIB x2 with magic 87/55; NUL-copy x6 (process.f:199, process-argv.f:52, process-command.f:82, argv.f:298, ffi-abi.f:740, fs.f:170); pollfd packer x3; directory iteration fs.f:464-478 vs fs-mutate.f:104-118; TILE-LOOP/ACC-LOOP identical; tile-v4.f vs tile-v4a.f eight identical bodies; IEEE narrowing x3 (float32.f:231-255, ptx/cg.f:129-151,174-196); EMIT-*-CTX prologue x3 (ptx/cg.f:202,364,392); X-PTR-U8-FIELD triple in 10 files; object-link.f:106-244 38 index accessors; json-read.f PEEK/ADVANCE defined then inlined at 9 sites. Acceptance: one definition each, consumers updated, counts re-measured in the commit. Files: lib/, tools/, src/. Verify: full gate. Depends: none. Ownership: lib. Claim: unassigned.
