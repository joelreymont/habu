---
title: "jit-m fold review: false comments and dead leftovers"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:22:19.344303+02:00"
---

Problem: the re-review of the jit-m fold (change owvxwrvl, "REPL route" removal; workspace .jj-ws.noindex/jit-m, part of habu-delete-the-jit-d9fdaf67) found the code holds but comments are false (card section 9). Apply after jit-rest reports (jit-rest sits on jit-m; rewriting jit-m while it runs stales its workspace); first drop every item whose code jit-rest deleted.

F1, comments the fold made false:
- src/habu/repl.f:197 "(layout.f names it, habu2.f reads it)" -> "(layout.f names it)".
- src/habu/layout.f:711-713 UNCGH-CELL comment becomes:
  \ UNCGH-CELL: runtime address of the uncaught-throw reporter (habu2.f LUNCAUGHT),
  \ written by the link (link-data.f HEADER-CELLS): BTHROW, a leaf, branches through it.
- src/habu/layout.f:1181-1182: delete "The REPL route (REPLH-CELL) is disarmed too, since a tty REPL takes an otherwise uncaught throw back to its prompt."
- src/habu/layout.f:1655 "(recover restores)" -> \ depth saved at REPL line start (habu2.f EM-REPL-READ)
- src/habu/habu2.f:5793-5808 EMIT-EXITHOOK block becomes:
  \ LEXITHOOK: layout.f EXIT-HOOK-CELL's trampoline, BL-called (BDIE, LUNCAUGHT, boot.f
  \ CALL-EXIT,) with the code in x0; it keeps x0 and LR only. Sealed like (PROT-SPAN).
- src/habu/habu2.f:13034 -> \ The checker's record end, end+1 (0 = no owner).

F2, comments the fold rewrote that are still false:
- src/habu/link-data.f:158-161 becomes:
  \ An engine's throw recoveries: its kernel branches through these cells inside an
  \ evaluate and after a data-stack underdepth. A stripped program's cut keeps neither.
- src/habu/habu2.f:12570-12572, first two lines become (keep the rest from "code in [1,255]"):
  \ No handler: the uncaught-throw exit, x9 = x15 = code. LEVLN falls in; BTHROW
  \ reaches LUNCAUGHT through UNCGH-CELL (link-data.f HEADER-CELLS writes it). A
- src/habu/layout.f:1372-1374 becomes:
  \ The readers are the Habu loop's token checks (src/habu/interpreter.f POLICY-CHECK-TOP,
  \ BODY-POLICY, DEF-WALL, PKG-POLICY-GUARD) and habu2.f LKWCMP for a keyword row.
- src/habu/habu2.f:1252-1253 sentence becomes:
  \ A sealed engine helper for the reason (PROT-SPAN) is: prim bodies reach it by
  \ direct BL (src/habu/rt.f G-OUT).
- src/habu/layout.f:505-509 becomes:
  \ RPKG-*: the REPL line's package snapshot (habu2.f EM-REPL-READ). PKGRESYNC-CELL:
  \ LEVALREC arms it, EM-PKG-RESYNC drains it before delivering the throw. $2780..$27C0.

F3: src/habu/jit.f:878, the `public` move is not needed (habu2.f reopens ENGINE-EMIT and calls private EMIT-MEMKW unprefixed): put `public` back above the LVSNAP variables so EMIT-FOPKW stays private.

F4, dead code left: habu2.f:12582 `9 15 0 ADDI` and :12584 `15 9 0 ADDI` (both entries arrive with x9 = x15 = code); dead labels THROW-NOREC (habu1.f:500, :3746, :3747; comments layout.f:302, :712, habu2.f:9134, :12571, test/engine-stack-lifecycle-lib.f:125 then say THROW-NOH), LEVLR (habu2.f:6530, :12457, :12581), LRDIE (habu2.f:2003, :13558, :12646, comment :12409); C-EMIT-TTY-PROBE (habu2.f:1236) has no emitted caller; RSAVCP/RSAVND/RSAVDP/RSAVSP, RSAVROWS, RPKG-* cells, RPKG:ENCLOSING/FLOOR/WIDS, USE-RPKG-SAVE-CELL have no reader (go with EM-REPL-READ); REPLH-CELL's writers (habu2.f:5817, process-fork.f:94, repl.f:202) write a cell nothing reads; tools/lint/clobber-lint-core.f:139 `Lreplroute` entry; LPOLICYTOPREC, LPOLICYREC, LUNDEF emitted but unreachable. LRREC stays until the installed hb carries this tree's link-data.f, then goes with kernel-object.f's RREC and RRECP-CELL rows.

F5: touched comments over two lines: habu2.f LEVLN block, EM-COMPILE-UNDEF header, EMIT-ROW-WALK, GENIO-OUT; layout.f RSAVROWS, DATA-FLOOR, DPHW, policy block; process-fork.f CHILD-MAIN; checker.f two; created-body-abi.f; habu1.f USE-WIDS-SAVE; engine-suite.f header; dictionary-zero.f; C-EMIT-TTY-PROBE.

Outside the fold, false since jit-m itself: habu2.f:8420 and :10960-10964 ((LP2VEXEC) as registered); link-data.f:138-140 (EM-DATA-INIT, EM-STARTUP-RUNTIME-STATE as the boot); layout.f:1172 and rt.f:167 (EM-COMPILE-EXIT/LRBYE as live).

Acceptance: each surviving item applied in the jit-m change (comments by the lead; F3 and F4 code by a worker brief, gate-lane.sh hb1 = hb2). Files: as listed. Verify: `rg -n 'EM-STARTUP-RUNTIME-STATE|REPL route|recover restores' src` shows no false claim. Depends: jit-rest's report. Ownership: jerry. Claim: unassigned.
