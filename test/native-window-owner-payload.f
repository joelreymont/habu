\ Run after the real replacement-checker handoff in native-window-owner-child.f.
\ No callback is synthesized: both validation and preparation reach that owner.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/compiler/native/checker-owner.f
require src/habu/aot-arm.f

package OWNER-PAYLOAD-CHECK

: EQ! ( n n -- ) 2dup <> if swap . . cr 79 throw then 2drop ;
: OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;
TRUSTED: AS-PREPARE ( n -- [ -- ] ) ;
: PREPARE ( -- )
   OWNER CHECKER-OWNER-ABI:BYTES CHECKER-OWNER-GUARD:VALIDATE
   CHECKER-OWNER-ABI:CAPTURE-OFF + CELL-VIEW @ AS-PREPARE execute ;

: PAYLOAD-NUMERIC ( n -- n ) 1+ ;

: CHECK-USABLE ( ptr u8 n ptr u8 n -- ) {: good:ptr goodu:n bad:ptr badu:n :}
   good goodu CHECKER-OWNER:CHECK-UNJUDGED -1 EQ!
   bad badu CHECKER-OWNER:CHECK-UNJUDGED 0 EQ! ;

: RUN ( -- )
   OWNER CHECKER-OWNER-ABI:BYTES CHECKER-OWNER-GUARD:VALIDATE OWNER <> if 79 throw then
   AOT-ARM:WINDOW-OPEN-PERSISTENT
   OWNER AOT-ARM:PAYLOAD-PERSISTENT
   AOT-ARM:WINDOW-CLOSE
   AOT-ARM:PAYLOAD-MODE @ 2 EQ!
   AOT-ARM:?FROZEN
   CHECKER-OWNER:CAPTURE-PREPARE
   s" PAYLOAD-VALID1 ( n -- n ) PAYLOAD-NUMERIC"
   s" PAYLOAD-INVALID1 ( ptr u8 -- ptr u8 ) PAYLOAD-NUMERIC" CHECK-USABLE
   PREPARE
   AOT-ARM:?FROZEN
   s" PAYLOAD-VALID2 ( n -- n ) PAYLOAD-NUMERIC"
   s" PAYLOAD-INVALID2 ( ptr u8 -- ptr u8 ) PAYLOAD-NUMERIC" CHECK-USABLE
   \ A second preparation remains callable and the original source owner is live.
   PREPARE
   s" PAYLOAD-VALID3 ( n -- n ) PAYLOAD-NUMERIC"
   s" PAYLOAD-INVALID3 ( ptr u8 -- ptr u8 ) PAYLOAD-NUMERIC" CHECK-USABLE
   AOT-ARM:WINDOW-OPEN-PERSISTENT
   OWNER AOT-ARM:PAYLOAD-PERSISTENT
   AOT-ARM:WINDOW-CLOSE
   AOT-ARM:?FROZEN ;

RUN
;package
