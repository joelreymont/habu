---
title: Roll back failed definitions in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.948447+03:00"
blocks:
  - habu-move-evaluate-and-9119f746
  - habu-compile-definer-bodies-1292d049
---

Problem: failed-definition rollback is the assembly `LEVALREC`.
Acceptance: a failed definition rolls back dictionary, DP, code cursor, address rows and package scope (`EM-PKG-RESYNC`) exactly as `LEVALREC` does; the `repl-address-cell-rollback` and `load-reject-diag` suites green through the Habu loop under the feature cell.
Files: `src/habu/outer.f`.
Verify: spark: the `repl-address-cell-rollback` and `load-reject-diag` suites; gate.
Depends: habu-move-evaluate-and-9119f746 (I9a), habu-compile-definer-bodies-1292d049 (I7).
Route: Alder (shared: src/habu/outer.f).
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-10-01, I5e habu-close-definitions-with-8ace78d8 landed): a failed `;` in the Habu loop keeps PEND and the open provenance window, as the engine's `;` does, and the engine's LEVALREC rolls both back today; this leaf's rollback clears them. Visible only through a catch around an included file or an exit hook; untested.

Measured (2026-10-01, CD review, product d6649e79): a caught failed definition leaves the Habu loop compiling, does> refusal or not. A nested file through `TRUSTED: OI-INC ( ptr u8 n -- ) ['] included catch . 2drop OI-ST. ;`, where `OI-ST.` prints PEND<>0, DOESB and BODYLEN, then `1 set-tier : OI-Y ( -- n ) 5 ; OI-Y . 7 .` in the includer. File `1 set-tier : OI-K ( n -- ) create , does> ( -- n ) @` / `DOES> ( -- )` (C-DIE-DOES): the engine prints 70 0 0 0 5 7, rc 0; the Habu loop prints 70 1 29 37, captures the next line into OI-K and ends `ncomp: cannot compile OI-K`, uncaught -8572, rc 67. File `1 set-tier : OI-G ( -- ) OI-NOPE ;` (the compiler refuses at `;`): the engine prints 70 0 0 0 5 7, rc 0; the Habu loop prints 70 1 0 20, captures the next line into OI-G and its second `;` ends rc 70. The engine clears this state in LEVALREC's EM-RESET-COMPILE-STATE; OUTER:INTERPRET's recovery (src/habu/interpret.f) puts back the input, usings and package scope only.

Lead note (2026-10-02, I5f Astra review): acceptance case. The Habu loop pushes no evaluate frame, so a caught throw leaves the pending definition open where the engine's LEVALREC rolls it back; every definer shows it (`:`, `cast:`). Reproduction through `included`, with and without `test/outer-loop-on.f`: catch `cast: CAST-REVIEW-BAD ( n n -- n )` (E-CAST-ARITY 7129), then define and call a word returning 42. Engine: `7129`, pending false, `42`, rc 0. Habu loop today: `7129`, pending true, then rc 70 `undefined word ':'` inside CAST-REVIEW-BAD. This leaf's rollback must make both loops agree, for `cast:` and `:`.

Lead note (2026-10-02, habu-call-the-unit-ec148ccf landed): acceptance add: `test/native-unit-compile-e2e.f` passes with its unit loaded through the Habu loop under the switch. Today it fails assert 26 (a caught event-3 refusal leaves the definition pending) and, past that case, asserts 42, 44 and 48 (`package`'s namespace record survives the refusal); both are this leaf's rollback.

Lead note (2026-10-02, batch 13 sweep): I9a's Habu `evaluate` writes its evaluate frame into a per-call mapping (`EVAL-FRAME-SIZE MEM-ALLOC-PTR`, freed with `munmap`; `src/habu/interpret.f`). The frame has the assembly layout because checked Habu cannot push on the native SP. It was accepted as a bridge while `LEVALREC` stays assembly. When this leaf and habu-recover-throws-across-18bd36f9 move `LEVALREC`'s state into Habu, the frame comes from that state and the per-call mapping goes. `FRAMES-FREED` in `test/outer-interpret.f` guards the leak either way.
