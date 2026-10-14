\ An ELAB: name has a separate source form from EPRIM: rows. Its engine
\ identity must still map both ways to the name token.
\ Run: bin/hb --load test/primitive-source.f

require src/lib/test.f
require src/habu/primitive-source.f

package PRIM-SOURCE-TEST

private

DYNAMIC-BUFFER RAW u8

: CASE-ELAB ( -- )
   s" elaborated primitive row" T-LABEL
   s" execute" PRIM-SPEC:FIND {: row:n :}
   row PRIM-SOURCE:TARGET
   {: path:ptr pathu:n start:n end:n found:bool :}
   found TTRUE
   end start - 7 T=
   path pathu FILE-SIZE {: bytes:n :}
   bytes RAW-RESERVE
   path pathu 0 RAW bytes READ-ALL bytes T=
   0 RAW bytes start PRIM-SOURCE:MARKER-IN
   {: marker:n from:n to:n reverse:bool :}
   reverse TTRUE
   marker -2 row - T=
   from start T= to end T= ;

public

: TEST ( -- )
   T-RESET
   CASE-ELAB
   T-REPORT ;

;package

PRIM-SOURCE-TEST:TEST
