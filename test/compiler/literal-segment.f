\ literal-segment.f - open a literal store segment, then ask whether it outlived
\ the rewind of whatever opened it.
\
\ The store opens a segment at `here` when its last one cannot take a body
\ (src/compiler/native/string.f). OPEN interns distinct bodies until one lands at
\ or above the `here` read before its intern: that body is the first of the
\ segment the intern opened. The caller then fails the evaluation, REPL line,
\ definition pass or declaration that OPEN ran inside, and SURVIVED? says whether
\ DATA resumed above the body, and whether the body kept its bytes and its row
\ while new DATA was written after it.

require lib/string.f
require src/compiler/native/string.f

package LITERAL-SEGMENT
private

4096 constant BODY-N
$40000 constant CLOBBER-N            \ DATA written after the rewind
BODY-N BUFFER: BODY
variable K                           \ numbers every body any OPEN interns
variable AT                          \ the body that opened the segment

TRUSTED: PTR>N ( ptr a -- n ) ;
TRUSTED: N>BYTES ( n -- ptr u8 ) ;

: BODY! ( n -- ) {: k:n :}
   BODY-N 0 ?do 76 BODY i + c! loop
   8 0 ?do k i 8 * rshift $FF and  BODY i + c! loop ;

: OPENED? ( -- bool )
   here PTR>N {: before:n :}
   BODY BODY-N NSTR:INTERN {: at:n :}
   at before < if false exit then
   at AT !
   true ;

: ABOVE? ( -- bool )
   here PTR>N  AT @ BODY-N +  >= ;

: CLOBBER ( -- )
   here {: at:ptr :}
   CLOBBER-N allot
   CLOBBER-N 0 ?do 88 at i + c! loop ;

: INTACT? ( -- bool )
   AT @ N>BYTES BODY-N  BODY BODY-N  STR= ;

: SAME? ( -- bool )
   BODY BODY-N NSTR:INTERN  AT @ = ;

public

\ The loop is bounded at twice what one segment holds of these bodies, so a store
\ that never opens one ends it instead of spinning.
: OPEN ( -- )
   0 AT !
   256 0 ?do
      K @ BODY!  1 K +!
      OPENED? if leave then
   loop ;

\ Each question needs the one before it: a body below `here` is not clobbered
\ to prove a point, and a body already clobbered is not looked up.
: SURVIVED? ( -- bool )
   AT @ 0= if false exit then
   ABOVE? 0= if false exit then
   CLOBBER
   INTACT? 0= if false exit then
   SAME? ;

;package
