\ The primitive source locator uses the raw registry text. An absent file or a
\ table whose names differ cannot lend a source position to a built-in use.
\ Run: bin/hb --load test/primitive-source.f

require src/lib/test.f
require src/habu/primitive-source.f

package PRIM-SOURCE-TEST

private

DYNAMIC-BUFFER RAW u8

: MUL-ROW ( -- n ) s" *" PRIM-SPEC:FIND ;

: LATE-PLUS ( -- n )
   PRIM-SPEC:COUNT s" +" PRIM-SPEC:FIND 1+ ?do
      i PRIM-SPEC:KIND@ PRIM-SPEC:K-PRIM = if
         i PRIM-SPEC:NAME$ s" +" STR= if i unloop exit then
      then
   loop
   -1 ;

: CASE-RAW ( -- )
   MUL-ROW {: row:n :}
   s" raw source row" T-LABEL
   row PRIM-SOURCE:TARGET {: path:ptr pathu:n start:n end:n found:bool :}
   found TTRUE
   pathu 0 > TTRUE
   end start - 1 T=
   path pathu FILE-SIZE {: bytes:n :}
   bytes RAW-RESERVE
   path pathu 0 RAW bytes READ-ALL bytes T=
   s" reverse source row" T-LABEL
   0 RAW bytes start PRIM-SOURCE:MARKER-IN
   {: marker:n from:n to:n reverse:bool :}
   reverse TTRUE
   marker -2 row - T=
   from start T= to end T=
   s" reverse caret at name end" T-LABEL
   0 RAW bytes end PRIM-SOURCE:MARKER-IN
   {: caret:n caret-from:n caret-to:n at-end:bool :}
   at-end TTRUE
   caret marker T=
   caret-from start T= caret-to end T=
   s" reverse overloaded row" T-LABEL
   LATE-PLUS {: late:n :}
   late 0 >= TTRUE
   0 RAW bytes late PRIM-SOURCE:TARGET-IN
   {: late-from:n late-to:n late-found:bool :}
   late-found TTRUE
   0 RAW bytes late-from PRIM-SOURCE:MARKER-IN
   {: plus:n plus-from:n plus-to:n plus-found:bool :}
   plus-found TTRUE
   s" +" PRIM-SPEC:FIND {: first:n :}
   plus -2 first - T=
   0 RAW bytes first PRIM-SOURCE:TARGET-IN
   {: first-from:n first-to:n first-found:bool :}
   first-found TTRUE
   plus-from first-from T= plus-to first-to T=
   late-from first-from <> TTRUE
   s" reverse outside name" T-LABEL
   0 RAW bytes start 1- PRIM-SOURCE:MARKER-IN
   {: other:n outside-from:n outside-to:n outside:bool :}
   outside TFALSE
   s" missing source" T-LABEL
   s" /no/such/habu/prims.f" row PRIM-SOURCE:TARGET-FILE
   {: ms:n me:n missing:bool :}
   missing TFALSE
   ms 0 T= me 0 T=
   s" mismatched source" T-LABEL
   s" EPRIM: + PE-N PE-IN EPRIM;" row PRIM-SOURCE:TARGET-IN
   {: xs:n xe:n mismatched:bool :}
   mismatched TFALSE
   xs 0 T= xe 0 T= ;

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
   CASE-RAW
   CASE-ELAB
   T-REPORT ;

;package

PRIM-SOURCE-TEST:TEST
