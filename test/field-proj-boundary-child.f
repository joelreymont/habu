\ This also executes generated accessor bodies through the trusted arming
\ forwarder, with scalar, offset and generic field projections read by value.
require test/field-proj-lib.f

package FIELD-BOUNDARY-TEST
using FIELD-PROJ-LIB
private

: =ASSERT ( n n -- )
   <> if s" field boundary assertion failed" 76 die then ;

PTR-VARIABLE SOURCE
variable LENGTH

\ Source evaluation is the tested public load boundary; retain no dynamic
\ stack results across catch, so both its success and failure are balanced.
: LOAD-SOURCE ( -- ) SOURCE @ LENGTH @ evaluate-closed ;

\ A closed text leaves nothing, so one that computes a value stores it in
\ RESULT, qualified: the texts run outside this package's private scope.
public
variable RESULT
private

: TRY ( ptr u8 n -- n )
   LENGTH ! SOURCE ! [: LOAD-SOURCE ;] catch ;

: LOCAL-SHADOW ( n -- n )
   {: FIELD-PROJ!:n :} FIELD-PROJ! ;

\ The compiler's trusted crossing can still mutate the protected storage.
TRUSTED: RAW-ROUNDTRIP ( -- n )
   FIELD-PROJ-U @ {: before:n :}
   43 FIELD-PROJ-U ! FIELD-PROJ-U @
   before FIELD-PROJ-U ! ;

: RAW-REFUSALS ( -- )
   s" FIELD-PROJ-A" 0 search-wl 0 =ASSERT
   s" FIELD-PROJ-U" 0 search-wl 0 =ASSERT
   s" FIELD-PROJ-FID" 0 search-wl 0 =ASSERT
   s" FIELD-PROJ-OFF" 0 search-wl 0 =ASSERT
   s" FP-RAW-A ( ptr u8 -- ) FIELD-PROJ-A !" CHECK-CANDIDATE! 0 =ASSERT
   s" FP-RAW-U ( n -- ) FIELD-PROJ-U !" CHECK-CANDIDATE! 0 =ASSERT
   s" FP-RAW-FID ( n -- ) FIELD-PROJ-FID !" CHECK-CANDIDATE! 0 =ASSERT
   s" FP-RAW-OFF ( n -- ) FIELD-PROJ-OFF !" CHECK-CANDIDATE! 0 =ASSERT
   s" : FP-RAW-A ( ptr u8 -- ) FIELD-PROJ-A ! ;" TRY 70 =ASSERT
   s" : FP-RAW-U ( n -- ) FIELD-PROJ-U ! ;" TRY 70 =ASSERT
   s" : FP-RAW-FID ( n -- ) FIELD-PROJ-FID ! ;" TRY 70 =ASSERT
   s" : FP-RAW-OFF ( n -- ) FIELD-PROJ-OFF ! ;" TRY 70 =ASSERT
   s" 0 FIELD-PROJ-A !" TRY 70 =ASSERT
   s" 0 FIELD-PROJ-U !" TRY 70 =ASSERT
   s" 0 FIELD-PROJ-FID !" TRY 70 =ASSERT
   s" 0 FIELD-PROJ-OFF !" TRY 70 =ASSERT
   RAW-ROUNDTRIP 43 =ASSERT ;

public
: RUN ( -- )
   10 20 0 FP-STORE 30 40 1 FP-STORE 42 0 FPG-STORE
   \ A projection rule refuses through the transferred checker: field a's
   \ committed offset is 0, not the CELL the single-shot window is armed with.
   s" FPX-WO" FID-A @ CELL FP-ARM
   s" FPX-WO ( ptr fprec -- ptr n ) 0 field-project" CHECK-QUIET-CANDIDATE! 0 =ASSERT
   s" FIELD-PROJ!" 0 search-wl 0 =ASSERT
   s" FP-GOOD ( -- n ) 42" CHECK-CANDIDATE! -1 =ASSERT
   s" FPX-FORGE ( ptr u8 n n n -- ) FIELD-PROJ!" CHECK-CANDIDATE! 0 =ASSERT
   s" : FPX-FORGE ( ptr u8 n n n -- ) FIELD-PROJ! ;" TRY 70 =ASSERT
   s" FPX-FORGE" 0 search-wl 0 =ASSERT
   s" ' FIELD-PROJ! drop" TRY 70 =ASSERT
   S\" s\" \" 0 0 FIELD-PROJ!" TRY 70 =ASSERT
   RAW-REFUSALS

   \ A different resolved symbol and a local keep their own meanings.
   41 LOCAL-SHADOW 41 =ASSERT
   s" package FIELD-SHADOW : FIELD-PROJ! ( -- n ) 42 ;" TRY 0 =ASSERT
   s" FP-SHADOW ( -- n ) FIELD-PROJ!" CHECK-CANDIDATE! -1 =ASSERT
   s" : CALL ( -- n ) FIELD-PROJ! ; CALL FIELD-BOUNDARY-TEST:RESULT !"
      evaluate-closed
   RESULT @ 42 =ASSERT
   s" ;package" TRY 0 =ASSERT
   s" FPX-FORGE ( ptr u8 n n n -- ) FIELD-PROJ!" CHECK-CANDIDATE! 0 =ASSERT

   \ The trusted forwarder really used this tier, and the earlier projections
   \ remain callable after all refusals.
   s" ' FIELD-PROJ-LIB:FP-ARM dup 4 + code-origin FIELD-BOUNDARY-TEST:RESULT !"
      evaluate-closed
   RESULT @ tier@ =ASSERT
   0 FP-GETA 10 =ASSERT
   1 FP-GETB 40 =ASSERT
   0 FPG-GET 42 =ASSERT
   s" field boundary: ok" type cr ;

;using
;package
FIELD-BOUNDARY-TEST:RUN
