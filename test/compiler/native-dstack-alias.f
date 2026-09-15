\ native-dstack-alias.f - production data-stack residency under aliasing.

require lib/prelude.f
require lib/errors.f
require lib/memory.f
require lib/test.f
require src/compiler/native/compiler.f
require tools/codegen-tail-probe.f

\ ---- the program under test --------------------------------------------------
\ A package of its own, and PUBLIC, because the bodies below are compiled from
\ source text at run time and name these words the way any other program would -
\ qualified, through the same dictionary the compiler uses for every callee.
package DKA
public

variable SB                          \ base of the fresh data stack
variable SZ                          \ its size
variable ANS                         \ where a body leaves its answer

: ALLOC ( -- )
   STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED SZ !  SB 0 ptr-field ! ;

: BASE ( -- ptr u8 )   SB 0 ptr-field @ ;
: SIZE ( -- n )        SZ @ ;

: SEVEN ( -- n )       7 ;
: ANS! ( n -- )        ANS ! ;

\ THE ALIASING BODY. `SEVEN` leaves its result in slot zero and the residency map
\ names it there; `BASE` hands back the address of that very slot; the `c!`
\ overwrites its low byte; and `abs` is the unrecorded external call whose
\ save-store the map says is already true. 7 with its low byte replaced by 200
\ is 200.
: POKED ( -- )
   SEVEN 200 BASE c! abs ANS! ;

\ The same body with the store removed, so the case can show the store is what
\ moves the answer rather than the shape of the body.
: CLEAN ( -- )
   SEVEN abs ANS! ;

: SAVE ( -- )
   41 1 + abs ANS! ;

: RESULT ( -- n )
   41 1 + ;

;package

package NDSA-TEST
private

\ `evaluate` enters RESULT by name on a fresh data stack.
TRUSTED: EV-N ( ptr u8 n -- n ) evaluate ;

: ENTRY-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u XREF-FIND dup XREF-FOUND? 0= if
      drop s" native-dstack-alias: record not found" 76 die
   then
   XREF-START ;

\ One direct XT run on a fresh data stack. The buffer is re-taken per run so no
\ answer can survive into the next one.
: RUN-XT ( [ -- ] -- n ) {: xt :}
   DKA:ALLOC
   0 DKA:ANS !
   xt DKA:BASE DKA:SIZE run-in-stack
   DKA:ANS @ ;

: ABS-ENTRY ( -- n )
   s" abs" NDICT:CALL-TARGET ;

\ Other checked helpers remain ordinary calls; this precondition names abs.
: ABS-CALLS ( -- n )
   s" DKA:POKED" ENTRY-OF {: base:n :}
   0
   s" DKA:POKED" NTAILPROBE:INSNS 0 ?do
      s" DKA:POKED" i NTAILPROBE:INSN@ dup NBR:BL? if
         base i NBR:INSN-BYTES * + swap NBR:BL-TARGET
         ABS-ENTRY = if 1+ then
      else drop then
   loop ;


: CALL-PRECONDITION ( -- )
   s" abs remains an external call" T-LABEL
   ABS-ENTRY 0 > TTRUE
   ABS-ENTRY s" DKA:POKED" ENTRY-OF < TTRUE
   ABS-CALLS 1 T= ;

\ ---- 1. the witness ----------------------------------------------------------
: ALIAS-CASE ( -- )
   s" a checked poke reaches the production routine's own stack slot"
   T-LABEL
   ['] DKA:CLEAN RUN-XT 7 T=
   ['] DKA:POKED RUN-XT 200 T= ;

\ ---- 2. a computed value handed to a call ------------------------------------
\ Nothing ever put this value in a cell, so its store is one the pass must emit.
\ A transfer applied twice would have read its own first answer back and called
\ the position resident, and the callee would have taken whatever the cell held.
: SAVE-CASE ( -- )
   s" a value no cell held is stored before the call that takes it" T-LABEL
   ['] DKA:SAVE RUN-XT 42 T= ;

\ ---- 3. a computed value returned --------------------------------------------
\ The same question at the exit run, which is a second transfer and a second
\ chance to apply one twice.
: EXIT-CASE ( -- )
   s" and a computed result is published into the cell the caller reads" T-LABEL
   s" DKA:RESULT" EV-N 42 T= ;

public

: RUN ( -- )
   CALL-PRECONDITION
   ALIAS-CASE
   SAVE-CASE
   EXIT-CASE ;

;package

T-RESET
NDSA-TEST:RUN
T-REPORT
