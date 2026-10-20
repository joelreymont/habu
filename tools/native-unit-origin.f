\ Whether the target compiler defined a pending source-origin provider above
\ the floor its definition cache was seeded with.

require src/lib/errors.f
require src/habu/xref.f

package NUNIT-ORIGIN
private

variable FLOOR

: BAD ( -- ) E-NUNIT-PROFILE throw ;

: TARGET-XT? ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if drop 0 exit then
   dup XREF-RETIRED? if drop 0 exit then
   XREF-START dup cp@ >= if drop 0 then ;

: PENDING-TARGET-XT? ( ptr u8 n -- n )
   TARGET-XT? dup FLOOR @ < if drop 0 then ;

public

: PENDING-AVAILABLE? ( -- bool )
   s" ENGINE-ORIGIN:ACCEPT-PENDING" PENDING-TARGET-XT? 0<>
   s" ENGINE-ORIGIN-EARLY:PREFIX-TAKE" PENDING-TARGET-XT? 0<> or ;

: BIND ( n -- ) {: floor:n :}
   floor 0 < if BAD then
   floor FLOOR ! ;

: RESET ( -- ) 0 FLOOR ! ;

;package
