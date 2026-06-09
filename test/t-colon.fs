\ t-colon.fs — the `:` override: checked defs compile AND run.

: SQ ( i64 -- i64 ) DUP * ;
T{ 7 SQ -> 49 }T

: HY ( i64 i64 -- i64 ) SQ SWAP SQ + ;
T{ 3 4 HY -> 25 }T

: ABSV ( i64 -- i64 ) DUP 0 < IF NEGATE THEN ;
T{ -5 ABSV -> 5 }T
T{ 5 ABSV -> 5 }T

: CNT ( i64 -- i64 ) BEGIN 1- DUP 0= UNTIL ;
T{ 3 CNT -> 0 }T

\ a quotation executed
: INCX ( i64 -- i64 ) [: 1+ ;] EXECUTE ;
T{ 41 INCX -> 42 }T

\ locals
: SWP ( i64 i64 -- i64 i64 ) {: a b :} b a ;
T{ 1 2 SWP -> 2 1 }T

\ recursion (factorial)
: FAC ( i64 -- i64 ) DUP 0= IF DROP 1 ELSE DUP 1- RECURSE * THEN ;
T{ 5 FAC -> 120 }T

\ case-insensitive: lowercase colon body + mixed type name
: tw ( I64 -- I64 ) 2 * ;
T{ 21 tw -> 42 }T

\ typed locals: checked with the type, compiled as bare names (F3 regression)
: DBL ( i64 -- i64 ) {: a:i64 :} a a + ;
T{ 21 DBL -> 42 }T
