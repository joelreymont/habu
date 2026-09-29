\ Run after the real replacement-checker handoff in native-window-owner-child.f.
\ No callback is synthesized: both validation and preparation reach that owner.
\ test/native-window-capture.f runs RUN before any other checker preparation or
\ boundary mark, and RUN refuses a checker that already shows either.
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

\ The checker as the window loaded it, which RUN's checks before and after its
\ first PREPARE are about. The first capture preparation copies the signature
\ store into the data span and a later one finds it there, which is what makes
\ the first one differ. And no core-prefix boundary is marked, so NORET-COMPACT
\ (src/core/checker.f) compacts without one: a build never takes that branch,
\ because its capture follows src/core/lower-cert-seal.f's mark.
TRUSTED: FRESH? ( -- bool )
   USIGS USIGS-CAP-U @ REG-DATA-SPAN? 0=
   CHECKER-BOUND:CURSORS 0= and ;

: CHECK-USABLE ( ptr u8 n ptr u8 n -- ) {: good:ptr goodu:n bad:ptr badu:n :}
   good goodu CHECKER-OWNER:CHECK-UNJUDGED -1 EQ!
   bad badu CHECKER-OWNER:CHECK-UNJUDGED 0 EQ! ;

: RUN ( -- )
   FRESH? 0= if
      s" payload: signature store already in the data span or boundary marked"
      type cr 79 throw then
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

;package
