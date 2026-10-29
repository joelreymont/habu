\ native-dstack-alias.f - production data-stack residency under aliasing.
require src/lib/prelude.f
require src/lib/errors.f
require src/lib/memory.f
require src/lib/test.f
require src/compiler/native/compiler.f

1 set-tier

\ A public package of its own: the bodies below compile from source text at run
\ time and name these words qualified, through the dictionary the compiler uses
\ for every callee.
package DKA
public

PTR-VARIABLE SB                      \ base of the fresh data stack
variable SZ
variable ANS                         \ where a body leaves its answer
variable PRESS-CELL
create PRESS-BYTE 1 allot

: ALLOC ( -- )
   STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED SZ !  SB ! ;

: BASE ( -- ptr u8 )   SB @ ;
: SIZE ( -- n )        SZ @ ;

: SEVEN ( -- n )       7 ;
: ANS! ( n -- )        ANS ! ;

\ `SEVEN` leaves its result in slot zero and the residency map
\ names it there; `BASE` hands back the address of that very slot; the `c!`
\ overwrites its low byte; and `abs` is the unrecorded external call whose
\ save-store the map says is already true. 7 with its low byte replaced by 200
\ is 200.
: POKED ( -- )
   SEVEN 200 BASE c! abs ANS! ;
tier@ constant POKED-TIER

\ The same body with the store removed, so the case can show the store is what
\ moves the answer rather than the shape of the body.
: CLEAN ( -- )
   SEVEN abs ANS! ;

: SAVE ( -- )
   41 1 + abs ANS! ;

: RESULT ( -- n )
   41 1 + ;

\ Integer and real locals remain live through two cell and two byte guards.
: PRESSURE ( n n n n n n r r -- n r )
   {: a:n b:n c:n d:n e:n f:n u:r v:r :}
   a PRESS-CELL !
   $2A PRESS-BYTE c!
   c PRESS-CELL !
   $4D PRESS-BYTE c!
   a b + c + d + e + f + PRESS-CELL @ + PRESS-BYTE c@ +
   u v f+ ;

;package

package NDSA-TEST
private

\ One direct XT run on a fresh data stack. The buffer is re-taken per run so no
\ answer can survive into the next one.
: RUN-XT ( [ -- ] -- n ) {: xt :}
   DKA:ALLOC
   0 DKA:ANS !
   xt DKA:BASE DKA:SIZE run-in-stack
   DKA:ANS @ ;

: ALIAS-CASE ( -- )
   s" a checked poke reaches the production routine's own stack slot"
   T-LABEL
   ['] DKA:CLEAN RUN-XT 7 T=
   ['] DKA:POKED RUN-XT 200 T= ;

\ Nothing ever put this value in a cell, so its store is one the pass must emit.
\ A transfer applied twice would have read its own first answer back and called
\ the position resident, and the callee would have taken whatever the cell held.
: SAVE-CASE ( -- )
   s" a value no cell held is stored before the call that takes it" T-LABEL
   ['] DKA:SAVE RUN-XT 42 T= ;

\ The same question at the exit run, which is a second transfer and a second
\ chance to apply one twice.
: EXIT-CASE ( -- )
   s" and a computed result is published into the cell the caller reads" T-LABEL
   \ TEST-EVAL:N enters RESULT by name above a closed floor.
   s" DKA:RESULT" TEST-EVAL:N 42 T= ;

: PRESSURE-CASE ( -- )
   s" live integer and real locals survive repeated checked stores" T-LABEL
   1 2 3 4 5 6 1.5 2.25 DKA:PRESSURE {: total:n real:r :}
   total 101 T=
   real 3.75 f= TTRUE ;

public

: RUN ( -- )
   ALIAS-CASE
   SAVE-CASE
   EXIT-CASE
   PRESSURE-CASE ;

;package

T-RESET
s" the aliasing subject was compiled at tier 1" T-LABEL
DKA:POKED-TIER 1 T=
NDSA-TEST:RUN
T-REPORT
