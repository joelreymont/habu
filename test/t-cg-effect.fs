\ t-cg-effect.fs — codegen records carry compact checked-effect metadata.
\ Run via tools/bootstrap-oracle.sh.
require ../bootstrap/src/habu.fs
CHECKING-ON? off
require ../bootstrap/cg/install.fs
require test/tester.fs
CHECKING-ON? on
CODEGEN-ON? on
decimal

: FLAGS ( "name" -- flags )
   parse-name WORD-PFA dup 0= abort" no codegen record" PFA>EFLAGS ;

: BOOLY ( i64 -- bool ) 0= ;
: ARITH ( i64 -- i64 ) 5 + 3 - ;
: POLY  ( a -- a ) dup drop ;

T{ FLAGS BOOLY CGF-IN-INTLIKE and 0<> -> true }T
T{ FLAGS BOOLY CGF-OUT-BOOL and 0<> -> true }T
T{ FLAGS BOOLY CGF-CONCRETE-INTBOOL and 0<> -> true }T
T{ FLAGS ARITH CGF-OUT-INTLIKE and 0<> -> true }T
T{ FLAGS POLY CGF-CONCRETE-INTBOOL and 0= -> true }T
