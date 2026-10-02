---
title: Compile defer through NCOMP; interpret-mode is
status: closed
priority: 2
issue-type: task
created-at: "2026-10-02T12:34:04.902072+03:00"
blocks:
  - habu-compile-definer-bodies-1292d049
closed-at: "2026-10-02T18:30:00+03:00"
close-reason: "done: defer, is in bodies and undefine through NCOMP in the Habu loop; chain gen1=gen2=gen3 cbce4d74; outer-interpret 234 agree; stripped family, native-defer-image, x86 link (hb-x64-link-defer 0) and kernel-definition images green; ARM64 image half moved to habu-compile-the-habu-a2760b34"
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

Lead correction (2026-10-02, from the I7c measurements; overrides the lines above where they differ): the engine has no interpret-mode `is` (`LKWIS` is only `J-IS`'s compile-keyword row; `EM-INTERPRET-DEFINE-KEYWORDS` has `defer`, not `is`), so a top-level `' X is D` is `E-UNDEFINED: is` rc 70 in both loops and stays so: the Habu loop keeps parity, and `is` with its `$4C`/`$46` refusals is tested inside `:` bodies. `defer-unset` is not a word to write: it is `DEFER-UNSET` (`src/core/exec-vector.f`), the xt a new defer's cell starts with, found by name. `undefine` already agrees in both loops. No native-build input reaches the pre-trust case through the Habu loop; refuse it rc 70 naming the missing owner operation.

Worker note (2026-10-02, I7c lane): everything above is in and proven except the ARM64 image half of the last acceptance item, which needs a decision. The Habu loop loads only at tier 0: at tier 1 NCOMP refuses 11 of its words, identically on the base (`7554c01d`, engine a95f): `HOOK`, `PUSH-STR`, `PUSH-CSTR`, `PUSH-ESC-STR`, `PUSH-ESC-CSTR`, `PUSH-CHAR`, `PUSH-XT` (`outer.f`), `DEF-PREFLIGHT` and `DEF-COMPILE` ("at execute") and `DEF-TAKE` (`definers.f`), `DISPATCH` (`interpret.f`). An executable build refuses a tier-0 definition (`hb: executable build requires native tier 1`, rc 70) and `APP-IMAGE:SAVE` refuses tier-0 code (`snap: retained code lacks native provenance`, rc 100), so neither a snapshot nor a stripped image can hold a Habu-loop defer until that loop compiles at tier 1. Shown instead: the stripped linker names the defer as its cell's owner (`test/stripped-address-cases.f` SAT-DEFER-OWNER), and the x86 link relocates the cell (`hb-x64-link-defer`, `test/x86-64-link-records.f`).

Lead correction (2026-10-02, at landing): the acceptance item 'snapshot and stripped capture of a defer relocate the cell and name the owner' is met here for the owner (test/stripped-address-cases.f SAT-DEFER-OWNER, through the stripped linker) and for the x86 cell (hb-x64-link-defer); its ARM64 image half moves to habu-compile-the-habu-a2760b34, because only the Habu loop makes an NCOMP defer and that loop loads only at tier 0, which snapshots and executable builds refuse by design.
