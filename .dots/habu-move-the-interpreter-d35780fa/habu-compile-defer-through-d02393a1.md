---
title: Compile defer through NCOMP; interpret-mode is
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T12:34:04.902072+03:00"
blocks:
  - habu-compile-definer-bodies-1292d049
---

Problem: `defer` bodies are assembly (`habu2.f` `C-DEFER-EMIT-CODE`/`C-DEFER-META-WRITE` ~4405-4418) and `defer` is E-UNDEFINED (rc 70) in the Habu loop; interpret-mode `is` and `defer-unset` stay with the assembly interpreter. Split from habu-compile-definer-bodies-1292d049 (I7) by the I7 design (lead correction on I7).
Design (I7 decision 4): keep the `DEFER-MAGIC` + cell trailer at `start+BODY(len)`, so its readers stay unchanged (`habu2.f` C-DEFER-TARGET-META ~4517-4534, `dict.f` SPELL-DEFER-CELL ~474-489, `aot-capture.f` ACAP-DEFER-SITE ~1010, ACAP-GRAPH-RAW-RECORD ~1855). New `NPUB:PUBLISH-PENDING-DEFER ( n -- )` (`publish.f`) publishes the body with exact span `slot - fn` (x86 int3 padding inside the span; trailer at CP), then code-publishes the 16 trailer bytes. Body: `NCOMP:COMPILE-FIXED` (I7b) kind DEFER: the cell's ADDR-DATA literal, a fetch, the indirect call DEFER-SCAN lowers (`elaborate.f` SCAN-FUN), arity from the declared signature. Registration: DEF-TRUST:REGISTER + checker-defer through the owner's `EFFECT-OFF`/`DEFER-OFF` before compiling; `EM-REC-WIDE-PUBLISH` through `REC-WIDE-PUBLISH-OFF`. Shadow: new AOT-SHADOW site kind `DCELL` for the x86 trailer cell; the linker writes raw 8-byte `DATA-VA + DATA-AT + off`.
Measure first: whether a DEFER-SCAN indirect call compiles with no tape (prefer an arity-free tail jump through the cell if HIR has one); whether any native-build prefix reaches the pre-trust case (target owner lacks EFFECT/DEFER, engine C-PD-CAPTURE ~4504-4508); if not, refuse it rc 70 in the Habu loop. Locate interpret-mode `is` (`rg -n LKWIS src/habu/habu2.f`; `J-IS` ~10196 is the compile keyword). `undefine` is already a Habu global (`xref.f:648`): measure whether the Habu loop runs it before writing anything; `defer-unset` is found by name (~4380-4385).
Acceptance: `defer D ( n -- n )`, `' X is D`, `defer-unset` and `undefine` run in the Habu loop with the engine's dictionary, checker and trailer state; x86 links a window holding a defer; refusals: `is` on a non-defer rc `$4C`, `is` on a missing name rc `$46` with the hint, a defer with no name rc `$4A`; snapshot and stripped capture of a defer relocate the cell and name the owner.
Files: `src/compiler/native/{compiler,elaborate,publish}.f`, `src/habu/{definers,aot-decl,aot-shadow,link-x64}.f`, `test/outer-interpret.f`, x86 link test.
Verify: spark rebuild, chain gen2==gen3, stripped family, gate; ThinkPad x86 link image.
Depends: habu-compile-definer-bodies-1292d049 (I7b).
Route: Alder (shared: src/compiler/native/*, src/habu/definers.f, src/habu/aot-decl.f, src/habu/aot-shadow.f).
Ownership: krait (Intel lane).
Claim: unassigned.
