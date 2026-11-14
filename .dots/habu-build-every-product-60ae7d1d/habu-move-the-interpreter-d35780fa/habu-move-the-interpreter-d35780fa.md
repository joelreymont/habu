---
title: Move the interpreter into checked Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.389107+03:00"
blocks:
  - habu-compile-definer-bodies-1292d049
  - habu-move-pkgs-using-22f18b81
  - habu-move-evaluate-and-9119f746
  - habu-recover-throws-across-18bd36f9
  - habu-route-stdin-repl-d7e12895
  - habu-add-the-engine-ebe5d757
  - habu-call-the-unit-ec148ccf
---

Goal: one interpreter, written in checked Habu (src/habu/interpreter.f), on every machine; the machine-code interpreter left in src/habu/habu1.f and habu2.f goes. Ruling (Joel, 2026-10-23): one interpreter; ARM64 keeps tier 0 beside tier 1. Joel 2026-11-04: "we decided to unify the frontend and just have different code-generation backends. why the fuck do arm64 and x86 have different interpreters? why does arm64 not use the typed one if it's better? why are we patching and working around instead of doing what's best for long term?"

Campaign lane only; do not dispatch. It lists its leaves under blocks: so it stays off dot ready until they close. Each case below is a disagreement between two implementations of one thing: it closes when this dot removes the second implementation, never by patching that copy.

Acceptance cases:
- a failed evaluate rolls back fully: a caught throw after `create` (`create LK-Y 0 0 / drop`, in a LINKER:WITH body or outside a build) leaves the ADDRMAP bit of the created body above the code pointer habu2.f LEVALREC restores (EM-EVAL-THROW-RECOVER), so FORGET-SPACE's CODE-RECLAIM:TRUNCATE never reaches it.
- removing a word of a sealed package is refused by the one interpreter.
- habu2.f:1610 encodes bl at run time with its own BL-REACH window and opcode; the checked interpreter calls A64ASM:ENC-BL and keeps no reach check of its own.
- habu2.f EM-PKG-RESYNC (:2004-2008) says a missing resync refuses E-PKG-CONTEXT; once xref.f installs LIVE the checker reads the engine's package cells, so the resync and its mirror are unread and go.
- a failed line's address cells: every failure inside an evaluate frame (habu1.f BTHROW to habu2.f LEVALREC) is rolled back by machine code that never runs interpreter.f FRAME-ROLLBACK-CODE, so ADDRESS-CELLS:KEEP-BELOW never cuts the address-cell list there; hb's text table is cut by checker.f DP-CUT, which that machine code and habu1.f BALLOT reach through CHECKER-OWNER-ABI:GRID-REVOKE-OFF. Accepted when a failed REPL line, a failed `evaluate` and a failed `included` cut both at the restored DP through FRAME-ROLLBACK-CODE, DP-CUT's text cut moves to whatever lowers DP for a negative `allot`, and test/address-cell-rollback.f's failed-load text case keeps passing.
- a layout that moves where DATA starts: the hb building a tree routes a build's xt! and ptr-cell! stores into its header to the image only below its own built-in DATA start (habu2.f ROUTE-HEADER, BXTSTORE, BPTRCELLSTORE), so no build can write an hb whose DATA starts elsewhere; src/arch/arm64/link-kernel.f READ refuses such a kernel object naming both values. Accepted when a tree that moves DATA start builds through the interpreter in checked Habu and the resulting hb rebuilds itself.
- the retained engine never sets DOES-SPLIT, so an open structure in a definer's head is refused at ; there and at does> on the native load. The one interpreter names does>.
- a catch that takes a throw keeps a definition opened inside its extent: src/habu/habu1.f BCATCH-RUN saves the stacks and the genio cells but not the pending definition, so with `: MAKE ( ptr u8 n -- ) EFFECT:NEW EFFECT:N EFFECT:OUT define-word 99 throw ;`, `[: s" DD" MAKE ;] catch` leaves DD pending, the lines after it compile into DD, and the next `:` is `E-UNDEFINED habu: in dd: undefined word ':'`, rc 70. An evaluate frame abandons such a definition (src/habu/interpreter.f FRAME-RECOVER compares the pending record with its entry's). Accepted when the one interpreter's catch abandons a definition opened since its entry and POSTPONE-RUN's own catch around DEF-END goes.
- a `generates: D ( effect )` row's claim is checked at the load's top-level call: the source pre-pass trusts the row and the load does not check it. A D that reads its name on one path only keeps its row (`: MK1 ( n -- ) 0<> if parse-name EFFECT:NEW define-word postpone ; then ;`, `generates: MK1 ( -- )`, `0 MK1 FOO`, `: Y ( -- ) FOO ;`: tools/check.f --verify-only rc 0, load E-UNDEFINED 'FOO' rc 70), and a row with a wrong effect (`: MK ( -- ) parse-name EFFECT:NEW EFFECT:N EFFECT:OUT define-word s" 5" body-append postpone ; ;`, `generates: MK ( -- )`, `MK FOO`, `: Y ( -- ) FOO ;`) gives --verify-only rc 0 where the load refuses `habu: in y: at 'FOO' expected: actual: n`, rc 70. The same on both engines: a word that reads a name and makes nothing (`: SKIP ( -- ) parse-name 2drop ; generates: SKIP ( -- )`, `SKIP FOO`, `: Y ( -- ) FOO ;`: --verify-only rc 0, load E-UNDEFINED rc 70); a word that reads two names (--verify-only rc 0, load rc 78); a row on create (`generates: create ( -- ptr ptr n )`, `: MKV ( -- ) create 0 , ;`, `MKV FOO`, `: Y ( -- n ) FOO @ @ ;`: --verify-only rc 0, load E-RAW-CELL-PTR rc 70). Accepted when each agrees between --verify-only and the load. Joel, 2026-11-10: "i want all issues identified to be dotted and fixed".
- the lookup legs that scan the whole dictionary when the hash table is absent (habu1.f:4583, 4658, 5818; habu2.f:2381, each `HIDXP-CELL ... CBZ`) are deleted: HIDXP-CELL is stored once and LHIDXBUILD runs before every lookup, so production never takes them, and an index defect would silently degrade to a scan of every record instead of failing. A missing table cannot occur by construction or fails loudly where it is detected.
- one name-equal leaf and one probe leaf serve every lookup, the hash computed once per lookup and passed forward: the folded-name compare loop is emitted in many words (habu1.f EMIT-FIND, WLFIND; habu2.f EMIT-FIND-USED and others) and a token is hashed up to three times (LFIND, then LFINDUSED on a miss). Design with the Plan agent before dispatch; after the scan legs go.

Claim: unassigned.

Acceptance case 2026-11-12 (jerry; from habu-print-the-one-ea184772's census, master 045c8ff4): four exits of the arm64 interpreter and definers write nothing or a line off the shape `<class> habu: <text>[ <16 hex>] lr <16 hex>` (src/habu/crash.f LDIE-LINE): src/habu/habu2.f C-EXIT-LABEL :421 (rc 70, 'trust-decl'/'trust-raw'), C-OWNER-FIELD :538 (rc 70, 'rec-wide-publish'/'rec-min-in@', no newline), EMIT-ROLLBACK :1755 (rc 96, evaluate's rollback), EM-COMPILE-DIE LCOMPILEDIE :88 (no lr). Accepted when the typed interpreter's equivalents print the one line.

Acceptance case 2026-11-12 (jerry; from habu-a-build-that-e324b393): a build whose program caught a throw that rolled DP back ends in LINKER:E-FAILED naming the caught code. The typed recovery (src/habu/interpreter.f FRAME-ROLLBACK-CODE) can record it beside DPHW-CELL for src/habu/linker.f END-CK and GOING to print; on arm64 the kernel's LEVALREC (habu2.f EM-EVAL-THROW-RECOVER, its DP-LOWER) recovers instead and records nothing. Accepted when test/file-objects.f CAUGHT-CASE asserts the code test/fixtures/file-objects/rollback.f caught.



Acceptance case 2026-11-13 (carl; from habu-a-failed-repl-5c40e8e8): a failed REPL line, or a failed `evaluate-closed` run from one, with a task live ends the session: arm64's kernel recovery (src/habu/habu2.f EM-EVAL-THROW-RECOVER :2233, EM-DEF-OCC:LRETIRE) retires the line's records and keeps NDICT, then src/habu/interpreter.f REPL-LINE's REPL-RECOVER runs FRAME-ROLLBACK-CODE again, whose `ndict!` (:2739) refuses: `E-TASK-COMPILE habu: ndict! while a task is live`, rc 79. On a PTY `bin/hb`: `require src/lib/task.f`, `TASK:MIN-STACK TASK:TASK SPINNER`, `: SPIN ( -- ) begin TASK:PAUSE false until ;`, `' SPIN SPINNER TASK:ACTIVATE`, `: DOOMED ( -- n ) 1 ; NOSUCH-WORD`; without the ACTIVATE line the session continues. Accepted when one recovery runs per failure and, with a task live, retires instead of lowering, so the session continues, `DOOMED .` is E-UNDEFINED and `1 2 + .` prints 3; a case in test/repl-address-cell-rollback.f holds it.
