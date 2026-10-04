\ interpret.f - the interpret loop written in Habu, OUTER:INTERPRET: it reads a
\ buffer token by token with the readers src/habu/outer.f defines, the
\ package keywords src/habu/packages.f defines and the definition heads and
\ body capture src/habu/definers.f defines. OUTER:EVAL-FRAMED runs the loop
\ under the engine's evaluate frame, and OUTER:INSTALL-EVALUATE makes it a
\ seeded engine's `evaluate` (src/habu/prims.f EPREFIX-PROVIDED!).

require lib/memory.f
require src/habu/layout.f
require src/habu/regalloc-abi.f
require src/habu/address-cells.f
require src/habu/outer.f
require src/habu/packages.f
require src/habu/definers.f
require src/habu/repl.f

package OUTER

private

\ ---- the loop -------------------------------------------------------------------------
\ A number is pushed and a word run: the token's effect on the stack is the
\ program's, which outer.f GIVE-N and RUN-WORD keep out of this row.
: DISPATCH ( -- )
   NUMERAL? if [: VALUE @ ;] GIVE-N TOP-EV-NUM 0 HOOK exit then
   RUN-WORD ;

\ The design seal admits its syntax tokens and numbers. Another top-level
\ token is resolved once here, before a parsing word can consume its operand;
\ STEP then runs that same admitted record. Native compilation checks body
\ tokens, and DEF-IMMEDIATE checks one it executes while capturing a body.
: POLICY-SYNTAX? ( -- bool )
   s" :" TOKEN-IS? if true exit then
   s" if" TOKEN-IS? if true exit then
   s" else" TOKEN-IS? if true exit then
   s" then" TOKEN-IS? if true exit then
   s" {:" TOKEN-IS? if true exit then
   s" :}" TOKEN-IS? if true exit then
   S\" s\"" TOKEN-IS? if true exit then
   s" package" TOKEN-IS? if true exit then
   s" public" TOKEN-IS? if true exit then
   s" private" TOKEN-IS? if true exit then
   s" ;package" TOKEN-IS? if true exit then
   s" using" TOKEN-IS? if true exit then
   s" ;using" TOKEN-IS? ;

: POLICY-PRE ( -- bool )
   POLICY-NDICT-CELL CELL@ 0= if false exit then
   PEND-CELL CELL@ 0<> if false exit then
   POLICY-SYNTAX? if false exit then
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   num if false exit then
   SEARCH
   true ;

\ ---- the unit hook (habu2.f C-UNIT-DISPATCH, C-UNIT-SOURCE-END) -----------------
\ With a unit hook armed, each token past a comment outside a body goes to it,
\ classed as LNUM reads it. In a body none does, and `does>` refuses there:
\ a selected unit may not split a body, at either tier.
: UNIT-CLASS ( -- n )
   TOKEN$ NUMBER {: v:n flt:bool num:bool range:bool :}
   num 0= if 0 exit then
   flt if 2 exit then
   1 ;

: UNIT-TOKEN ( -- )
   UNIT? 0= if exit then
   PEND-CELL CELL@ 0<> if
      s" does>" TOKEN-IS? if UNIT-REFUSE then
      exit
   then
   UNIT-CLASS UNIT-EV-TOKEN UNIT-EVENT drop ;

\ The end of the source, past the refusal of a definition left unfinished. It
\ runs inside the buffer's catch, so a refusal takes the buffer's recovery.
: UNIT-END ( -- )
   UNIT? 0= if exit then
   PEND-CELL CELL@ 0<> if UNIT-REFUSE then
   0 UNIT-EV-END UNIT-EVENT drop ;

: STEP ( -- )
   COMMENT? if exit then
   POLICY-PRE {: found:bool :}
   UNIT-TOKEN
   COMPILING? if exit then
   LITERAL? if exit then
   PACKAGE? if exit then
   DEFINE? if exit then
   found if RUN-FOUND exit then
   DISPATCH ;

\ The kernel's nested-evaluate recovery rolls an outer definition back to the
\ token it was reading. Mark before TOKEN, including the pass that reaches EOF,
\ as EM-COMMENT does for the native loop.
: TOKEN-MARK ( -- )
   BODYLEN-CELL CELL@ TOKBODY-CELL CELL!
   LOCN-CELL CELL@ TOKLOCN-CELL CELL!
   cp@ dbase@ - TOKCP-CELL CELL!
   VSP-CELL CELL@ TOKVSP-CELL CELL!
   REGALLOC-ABI:VRFREE-CELL CELL@ TOKVRF-CELL CELL!
   REGALLOC-ABI:FRFREE-CELL CELL@ TOKFRF-CELL CELL! ;

: RUN-TOKENS ( -- )
   begin TOKEN-MARK TOKEN while STEP repeat ;

: RUN ( -- )
   RUN-TOKENS
   DEF-SOURCE-END UNIT-END ;

\ Run the buffer under catch, refusing at its end a definition it opened, and
\ give its code. A throw also puts back the d used publics below depth d, the
\ includer's: a buffer that closes the includer's package and opens a using
\ writes into one of them (habu1.f B-EVAL keeps the engine's in its frame).
\ Each call keeps one slot.
: RUN-CAUGHT ( n -- n )
   {: d:n :}
   d 0= if [: RUN ;] catch exit then
   d 1- cells USE-WIDS-OFF + CELL@ {: wid:n :}
   d 1- RECURSE {: code:n :}
   code 0<> if wid d 1- cells USE-WIDS-OFF + CELL! then
   code ;

\ The buffer ran out (habu2.f EM-EVAL-CLEAN-EXIT). The evaluate error cell is
\ cleared, as the engine's clean exit clears it: an `evaluate` the buffer caught
\ recorded its code there, and include.f reads a nonzero cell after the buffer
\ as a failed evaluation. The using depth goes back to the buffer's using floor
\ (packages.f USE-FLOOR), which is the entry depth unless a `;package` closed a
\ package opened before the buffer and restored a lower one, and a package the
\ buffer left open keeps none of its usings: its using floor drops to the
\ restored depth.
: CLEAN-END ( -- )
   0 EVALERR-CELL CELL!
   USE-FLOOR @ USE-DEPTH-CELL CELL@ min {: back:n :}
   back USE-DEPTH-CELL CELL!
   USE-PKG-SAVE-CELL CELL@ back > if back USE-PKG-SAVE-CELL CELL! then ;

\ ---- the evaluate frame (habu1.f EVAL-ENTER) ----------------------------------
\ `evaluate` keeps the frame the engine's evaluate pushes, field for field, in a
\ mapping of its own, linked through EVAL-TOP-CELL and counted in EVALD-CELL, so
\ a throw out of its buffer reaches habu2.f LEVALREC, which pops the frame and
\ puts back what it holds: the input, the dictionary top, the stack extent, the
\ package scope and the usings. The fields layout.f does not name sit at the
\ offsets EVAL-ENTER stores them. The clean-exit continuation at $10 stays
\ zero: only the engine's loop reads it (habu2.f EM-EVAL-CLEAN-EXIT), and that
\ loop never reads a buffer this frame bounds.
0 constant FR-INP                  \ the includer's input cursor
8 constant FR-INE                  \ and the end of its input
$18 constant FR-BOUND              \ the evaluate's own catch handler
$20 constant FR-XDS                \ the data-stack cursor the buffer starts on
$28 constant FR-CP                 \ the dictionary top a throw rolls back to
$30 constant FR-NDICT
$38 constant FR-DP

\ The frame on its way into the catch that links it; empty at rest.
TYPED-VARIABLE FRAME-ARG ptr u8

: FR! ( n ptr u8 n -- )
   + CELL-VIEW ! ;

: FR@ ( ptr u8 n -- n )
   + CELL-VIEW @ ;

\ `evaluate` while a task is live ends the process with no output, as the
\ engine's does (habu1.f B-TASK-LIVE-GUARD).
: EVAL-TASK-GUARD ( -- )
   TASKS-LIVE-CELL CELL@ 0= if exit then
   s" " RC-TASK-LIVE FAIL-CLOSED ;

\ The frame of a buffer whose caller leaves n cells on the data stack, all but
\ its link: the includer's input, the dictionary top, the stack extent, the
\ package scope, the using depth, which is also the buffer's using floor, and
\ the includer's used publics.
: FRAME-OPEN ( n -- ptr u8 ) {: cells-in:n :}
   EVAL-FRAME-SIZE MEM-ALLOC-PTR BYTE-VIEW {: f:ptr :}
   INP-CELL CELL@ f FR-INP FR!
   INE-CELL CELL@ f FR-INE FR!
   SRCLOC:INB-CELL CELL@ f EVAL-INB FR!
   STACK-ABI:BASE-CELL CELL@ cells-in cells + f FR-XDS FR!
   cp@ f FR-CP FR!
   ndict@ f FR-NDICT FR!
   DP-CELL CELL@ f FR-DP FR!
   STACK-ABI:BASE-CELL CELL@ f STACK-ABI:EVAL-BASE FR!
   STACK-ABI:CAP-CELL CELL@ f STACK-ABI:EVAL-CAP FR!
   0 f STACK-ABI:EVAL-SEG FR!
   PEND-CELL CELL@ f EVAL-FRAME:PEND FR!
   f EVAL-PKG + {: snap:ptr :}
   CUR-CELL CELL@ snap PKGSNAP-CUR FR!
   PKG-PUB-CELL CELL@ snap PKGSNAP-PUB FR!
   PKG-PRI-CELL CELL@ snap PKGSNAP-PRI FR!
   PKG-PARENT-CELL CELL@ snap PKGSNAP-PARENT FR!
   PKG-REC-CELL CELL@ snap PKGSNAP-REC FR!
   USE-DEPTH-CELL CELL@ snap PKGSNAP-USE FR!
   USE-PKG-SAVE-CELL CELL@ snap PKGSNAP:FLOOR FR!
   USE-DEPTH-CELL CELL@ f EVAL-FRAME:USE-FLOOR FR!
   USE-MAX 0 ?do
      i cells USE-WIDS-OFF + CELL@  f EVAL-FRAME:USE-WIDS i cells + FR!
   loop
   f ;

\ Link the frame as the innermost evaluate's. It runs first in the evaluate's
\ catch, so the nearest handler is that catch's: LEVALREC pops a frame whose
\ boundary lies at or below the nearest handler, and leaves it for a handler
\ the buffer installs, which lies below.
: FRAME-LINK ( ptr u8 -- ) {: f:ptr :}
   NULL-PTR FRAME-ARG !
   HND-CELL CELL@ f FR-BOUND FR!
   EVAL-TOP-CELL CELL@ f EVAL-PREV FR!
   f NULL-PTR - EVAL-TOP-CELL CELL!
   EVALD-CELL CELL@ 1+ EVALD-CELL CELL! ;

\ The buffer ran out: unlink the frame and give the includer its input back.
: FRAME-UNLINK ( ptr u8 -- ) {: f:ptr :}
   f EVAL-PREV FR@ EVAL-TOP-CELL CELL!
   EVALD-CELL CELL@ 1- EVALD-CELL CELL!
   f FR-INP FR@ INP-CELL CELL!
   f FR-INE FR@ INE-CELL CELL!
   f EVAL-INB FR@ SRCLOC:INB-CELL CELL! ;

: FRAME-LEAVE ( ptr u8 -- )
   FRAME-UNLINK
   CLEAN-END ;

\ x86 THROW restores its catch stack but returns with the Habu evaluation
\ frame still linked. ARM LEVALREC has already popped it before catch returns.
\ The top link is the ownership test: exactly one path performs the rollback.
: FRAME-ROLLBACK-CODE ( ptr u8 -- ) {: f:ptr :}
   DEF-ABORT
   \ A buffer may start a task and throw without allocating a definition.
   \ Only changed cursors need their guarded mutation sinks during recovery.
   f FR-NDICT FR@ dup ndict@ <> if ndict! else drop then
   f FR-CP FR@ dup cp@ <> if CODE-RECLAIM:TRUNCATE else drop then
   data-base BYTE-VIEW NULL-PTR BYTE-VIEW - {: base:n :}
   f FR-DP FR@ base DATA-FLOOR-CELL CELL@ + max {: cut:n :}
   cut base - ADDRESS-CELLS:KEEP-BELOW
   cut DP-CELL CELL! ;

: FRAME-ROLLBACK-TOKEN ( -- )
   TOKBODY-CELL CELL@ BODYLEN-CELL CELL!
   TOKLOCN-CELL CELL@ LOCN-CELL CELL!
   dbase@ TOKCP-CELL CELL@ + CODE-RECLAIM:TRUNCATE
   TOKVSP-CELL CELL@ VSP-CELL CELL!
   TOKVRF-CELL CELL@ REGALLOC-ABI:VRFREE-CELL CELL!
   TOKFRF-CELL CELL@ REGALLOC-ABI:FRFREE-CELL CELL! ;

: FRAME-RESTORE-SCOPE ( ptr u8 -- ) {: f:ptr :}
   f EVAL-PKG + {: snap:ptr :}
   snap PKGSNAP-USE FR@ USE-DEPTH-CELL CELL!
   USE-MAX 0 ?do
      f EVAL-FRAME:USE-WIDS i cells + FR@
      i cells USE-WIDS-OFF + CELL!
   loop
   snap PKGSNAP-REC FR@ snap PKGSNAP-PARENT FR@
   snap PKGSNAP-CUR FR@ snap PKGSNAP:FLOOR FR@ PKG-RECOVER ;

: FRAME-RECOVER ( ptr u8 n -- ) {: f:ptr code:n :}
   f EVAL-FRAME:PEND FR@ 0= if f FRAME-ROLLBACK-CODE else FRAME-ROLLBACK-TOKEN then
   f FR-INP FR@ INP-CELL CELL!
   f FR-INE FR@ INE-CELL CELL!
   f EVAL-INB FR@ SRCLOC:INB-CELL CELL!
   f FRAME-RESTORE-SCOPE
   f EVAL-PREV FR@ EVAL-TOP-CELL CELL!
   EVALD-CELL CELL@ 1- EVALD-CELL CELL!
   code EVALERR-CELL CELL! ;

: FRAME-FREE ( ptr u8 -- )
   EVAL-FRAME-SIZE munmap 0<> if E-MEM-UNMAP throw then ;

\ The REPL has one source owner. A line uses a savepoint, but its successful
\ stack, pending definition, package and usings remain for the next line.
create REPL-QNL 63 c, 10 c,
variable REPL-OK

: REPL-READ ( -- ptr u8 n )
   REPLH-PTR @ execute ;

TRUSTED: REPL-STACK-CLEAR ( -- )
   stack-clear ;

: REPL-RECOVER ( ptr u8 n ptr n -- ) {: f:ptr prior:n found:ptr :}
   f FRAME-ROLLBACK-CODE
   f FR-INP FR@ INP-CELL CELL!
   f FR-INE FR@ INE-CELL CELL!
   f EVAL-INB FR@ SRCLOC:INB-CELL CELL!
   f FRAME-RESTORE-SCOPE
   prior USE-FLOOR !
   found REC !
   0 EVALERR-CELL CELL! ;

: REPL-LINE ( ptr u8 n -- bool ) {: a:ptr u:n :}
   depth FRAME-OPEN {: f:ptr :}
   \ The session, not this line, owns a pending definition from an earlier
   \ line. The JIT's source-close check must allow its `;`; a failed line
   \ still abandons that definition through this savepoint's full rollback.
   0 f EVAL-FRAME:PEND FR!
   USE-FLOOR @ {: prior:n :}
   REC @ {: found:ptr :}
   a INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a u + INE-CELL ADDR!
   f FRAME-ARG !
   [: FRAME-ARG @ FRAME-LINK RUN-TOKENS ;] catch {: code:n :}
   code 0= if
      f FRAME-UNLINK
   else
      f NULL-PTR - EVAL-TOP-CELL CELL@ = if f FRAME-UNLINK then
      f prior found REPL-RECOVER
   then
   f FRAME-FREE
   code 0<> if
      REPL-STACK-CLEAR
      2 REPL-QNL 2 write drop
      false exit
   then
   true ;

: REPL-END ( -- )
   [: DEF-SOURCE-END UNIT-END ;] catch {: code:n :}
   code 0<> if NULL$ code die then ;

public

\ Read the hook anew for every line: genio may replace its reader during a
\ session. Only EOF closes a pending definition or selected source unit.
: REPL ( -- )
   0 ENTRY-PEND !
   true REPL-OK !
   begin
      REPL-OK @ if S\"  ok\n" type then
      REPL-READ {: a:ptr u:n :}
      a 0= if REPL-END exit then
      a u REPL-LINE REPL-OK !
   again ;

\ Interpret the buffer as the engine's evaluate does (habu1.f B-EVAL), under the
\ evaluate frame: a throw out of it is rolled back in LEVALREC, which pops the
\ frame before the catch here sees the code. Either way the frame's mapping
\ goes back, and so do the buffer's using floor and the record the step that
\ ran the word looked up (outer.f REC).
: EVAL-FRAMED ( ptr u8 n -- ) {: a:ptr u:n :}
   EVAL-TASK-GUARD
   depth FRAME-OPEN {: f:ptr :}
   USE-FLOOR @ {: outer:n :}
   REC @ {: found:ptr :}
   ENTRY-PEND @ {: prior:n :}
   PEND-CELL CELL@ {: entry:n :}
   entry ENTRY-PEND !
   USE-DEPTH-CELL CELL@ USE-FLOOR !
   a INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a u + INE-CELL ADDR!
   f FRAME-ARG !
   [: FRAME-ARG @ FRAME-LINK RUN ;] catch {: code:n :}
   code 0= if
      f FRAME-LEAVE
   else
      f NULL-PTR - EVAL-TOP-CELL CELL@ = if f code FRAME-RECOVER then
   then
   prior ENTRY-PEND !
   code 0<> if entry PEND-CELL CELL! then
   outer USE-FLOOR !  found REC !
   f FRAME-FREE
   code 0<> if code throw then ;

\ Make EVAL-FRAMED the engine's `evaluate`. A seeded engine's `evaluate` record
\ jumps through PROVIDED-XT:EVALUATE-CELL (habu1.f FPRIM-PROVIDED), so every
\ caller keeps the record it was compiled against, src/core/include.f
\ INCLUDE-EVALUATE's among them. src/habu/native-runtime.f CHECKER-REG:SEAL
\ runs this as the build window's last act. `xt!` makes the cell an address
\ cell, which the capture carries as a fixed row (tools/native-layout.f) and
\ the seed restores on every boot.
: INSTALL-EVALUATE ( -- )
   ['] EVAL-FRAMED data-base PROVIDED-XT:EVALUATE-CELL + xt! ;

\ Interpret the buffer as the engine's evaluate reads it, and put the input
\ cells, the looked-up record (outer.f REC) and the using depth back after,
\ whether the buffer ends or a token throws: usings are file-local (habu1.f
\ B-EVAL, habu2.f EM-EVAL-CLEAN-EXIT), and a buffer a word evaluates leaves
\ the record of the step that ran the word.
\ A throw also puts back the package scope the buffer entered with, its using
\ floor included (habu2.f LEVALREC), so a package a throwing file opened is
\ closed and one it closed is open again, and the used publics RUN-CAUGHT
\ kept, which the checker's resync names. A clean end puts the depth back to
\ the buffer's using floor (packages.f USE-FLOOR), which is the entry depth
\ unless a `;package` closed a package opened before the buffer and restored
\ a lower one (habu2.f EM-EVAL-CLEAN-EXIT). A package the buffer left open
\ keeps none of its usings: the package's using floor drops to the restored
\ depth. A clean end also clears the evaluate error cell, as the engine's does:
\ an `evaluate` the buffer caught recorded its code there, and include.f reads
\ a nonzero cell after the buffer as a failed evaluation. A definition the
\ buffer opened and left open is refused at its end, while the input cells
\ still name the buffer, and a throw gives up a definition the buffer opened:
\ the record pending as it began is pending again (definers.f ENTRY-PEND).
: INTERPRET ( ptr u8 n -- )
   {: a:ptr u:n :}
   INP-CELL CELL@ INE-CELL CELL@ SRCLOC:INB-CELL CELL@ USE-DEPTH-CELL CELL@
   ENTRY-PEND @
   {: p:n e:n b:n d:n o:n :}
   PKG-STATE {: rec:n parent:n cur:n floor:n :}
   USE-FLOOR @ {: outer:n :}
   REC @ {: found:ptr :}
   d USE-FLOOR !
   PEND-CELL CELL@ {: entry:n :}
   entry ENTRY-PEND !
   a INP-CELL ADDR!  a SRCLOC:INB-CELL ADDR!  a u + INE-CELL ADDR!
   d RUN-CAUGHT {: code:n :}
   code 0= if CLEAN-END then
   outer USE-FLOOR !
   p INP-CELL CELL!  e INE-CELL CELL!  b SRCLOC:INB-CELL CELL!  found REC !
   o ENTRY-PEND !
   code 0<> if entry PEND-CELL CELL!  d USE-DEPTH-CELL CELL!  rec parent cur floor PKG-RECOVER  code throw then ;

;package
