\ Descriptor bounds only: no synthetic record supplies or executes callbacks.
require lib/test.f
require src/habu/aot-arm.f

package OWNER-DESCRIPTOR-TEST

\ Also run on an empty cold engine: these constants precede the checker there,
\ so their public interface must be declared before the guard can compile.
: ABI-READ ( -- n ) CHECKER-OWNER-ABI:HEADER-BYTES ;

create HEADER
   CHECKER-OWNER-ABI:MAGIC , CHECKER-OWNER-ABI:BYTES ,
   CHECKER-OWNER-ABI:BYTES allot
: RECORD ( -- ptr u8 ) HEADER CHECKER-OWNER-ABI:HEADER-BYTES + ;
PTR-VARIABLE ARG
variable NEED

: CHECK ( -- ) ARG @ NEED @ CHECKER-OWNER-GUARD:VALIDATE drop ;
: REJECT ( -- ) ['] CHECK catch -8574 T= ;
: GOOD ( -- ) ['] CHECK catch 0 T= ;
TRUSTED: DATA-START ( -- ptr u8 ) data-base ;

: SOURCE-OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;
: TARGET-OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:TARGET-DECL-CELL + 0 ptr-field @ ;

\ A real cold prefix owns its checker before a partial window can open. The
\ same invariant holds in the retained product after its boot installer runs.
: LIVE-OWNER ( -- )
   SOURCE-OWNER dup TARGET-OWNER = TTRUE
   CHECKER-OWNER-ABI:BYTES CHECKER-OWNER-GUARD:VALIDATE drop
   AOT-ARM:WINDOW-OPEN
   AOT-ARM:PAYLOAD-MODE @ 1 T=
   AOT-ARM:WINDOW-CLOSE
   AOT-ARM:PAYLOAD-FROZEN @ -1 T= ;

TRUSTED: ABI-ONLY ( -- )
   s" OD-ABI-ONLY ( -- n ) 7" CHECK-UNJUDGED! -1 T= ;

: RUN ( -- )
   T-RESET
   ABI-READ 16 T=
   ABI-ONLY
   s" OD-PUBLIC ( -- n ) CHECKER-OWNER-ABI:HEADER-BYTES" CHECK-CANDIDATE! -1 T=
   s" OD-ABI-CALL ( -- n ) OD-ABI-ONLY" CHECK-CANDIDATE! -1 <> TTRUE
   s" OD-PRIVATE ( ptr u8 -- n ) CHECKER-OWNER-GUARD:ADDRESS" CHECK-CANDIDATE! -1 <> TTRUE
   RECORD ARG ! CHECKER-OWNER-ABI:BYTES NEED ! GOOD
   NULL-PTR ARG ! REJECT
   DATA-START ARG ! REJECT
   RECORD ARG !
   0 HEADER ! REJECT
   CHECKER-OWNER-ABI:MAGIC HEADER !
   16 HEADER CELL + ! REJECT
   17 HEADER CELL + ! REJECT
   -1 HEADER CELL + ! REJECT
   $7FFFFFFFFFFFFFFF HEADER CELL + ! REJECT
   CHECKER-OWNER-ABI:BYTES HEADER CELL + !
   -1 NEED ! REJECT
   CHECKER-OWNER-ABI:BYTES 1+ NEED ! REJECT
   CHECKER-OWNER-ABI:BYTES NEED ! GOOD
   LIVE-OWNER
   \ This opening is valid even while the retained host has a legacy record.
   AOT-ARM:WINDOW-OPEN-PERSISTENT
   AOT-ARM:PAYLOAD-MODE @ 0 T=
   AOT-ARM:PAYLOAD-FROZEN @ 0 T=
   T-REPORT ;

RUN
;package
