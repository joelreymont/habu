\ source-unit-run closes one complete callback at the caller's cursor. The
\ callback can use its own intermediate cells, but must return with none.
require lib/errors.f
require lib/test.f
require src/habu/stack-abi.f

package SOURCE-ROOT
private

TRUSTED: CU-RUN ( [ -- ] -- ) source-unit-run ;

: CU-BASE ( -- ptr u8 )
   data-base STACK-ABI:BASE-CELL + 0 ptr-field @ ;

: CU-CAP ( -- n )
   data-base STACK-ABI:CAP-CELL + @ ;

PTR-VARIABLE CU-SAVED-BASE
variable CU-SAVED-CAP

: CU-SAVE ( -- )
   CU-BASE CU-SAVED-BASE !  CU-CAP CU-SAVED-CAP ! ;

: CU-RESTORED ( -- )
   CU-BASE CU-SAVED-BASE @ = TTRUE
   CU-CAP CU-SAVED-CAP @ T= ;

: CU-RAISE ( -- ) 27 throw ;
: CU-RAISE-CLOSED ( -- ) [: CU-RAISE ;] CU-RUN ;

\ Deliberately dishonest trusted callbacks exercise the runtime boundary that
\ the effect checker cannot model: a leftover cell and a cursor below the floor.
TRUSTED: CU-ONE ( -- ) 11 ;
: CU-ONE-CLOSED ( -- ) [: CU-ONE ;] CU-RUN ;
TRUSTED: CU-TAKE ( -- ) drop ;
: CU-TAKE-CLOSED ( -- ) [: CU-TAKE ;] CU-RUN ;

\ The loop consumes its bounds before pushing. At return the stack cursor is
\ exactly the mapped top; the primitive must compare before pushing a code.
TRUSTED: CU-FILL ( -- )
   CU-CAP 8 / 0 ?do 1 loop ;
: CU-FILL-CLOSED ( -- ) [: CU-FILL ;] CU-RUN ;

: CU-CLEAN ( -- )
   CU-SAVE
   7 [: 11 13 + drop ;] CU-RUN 7 T=
   CU-RESTORED ;

: CU-BELOW ( -- )
   CU-SAVE
   7 [: CU-TAKE-CLOSED ;] catch 70 T= 7 T=
   CU-RESTORED
   [: ;] CU-RUN ;

: CU-THROW ( -- )
   CU-SAVE
   7 [: CU-RAISE-CLOSED ;] catch 27 T= 7 T=
   CU-RESTORED
   [: ;] CU-RUN ;

: CU-RESIDUE ( -- )
   CU-SAVE
   7 [: CU-ONE-CLOSED ;] catch E-EVAL-RESIDUE T= 7 T=
   CU-RESTORED
   [: ;] CU-RUN ;

: CU-FULL ( -- )
   CU-SAVE
   7 [: CU-FILL-CLOSED ;] catch E-EVAL-RESIDUE T= 7 T=
   CU-RESTORED
   [: ;] CU-RUN ;

: CU-TEST ( -- )
   T-RESET
   s" a clean complete callback leaves the caller cell" T-LABEL CU-CLEAN
   s" a callback below its floor throws 70" T-LABEL CU-BELOW
   s" a nested throw restores the stack descriptor" T-LABEL CU-THROW
   s" ordinary residue restores the caller and descriptor" T-LABEL CU-RESIDUE
   s" an exactly full physical stack throws named residue" T-LABEL CU-FULL
   T-REPORT ;

' CU-TEST
;package
execute
