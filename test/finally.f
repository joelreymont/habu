\ Generic cleanup through the native engine, including both exception paths.
require lib/test.f
require test/checker-assert.f

package FINALLY-TEST

$100000007 constant E-BODY
-$100000009 constant E-CLEANUP
variable CLEANUPS
variable ORDER
variable OPAQUE
TYPED-VARIABLE STORED [ -- ]


: CLEAN ( -- ) 1 CLEANUPS +! ;


: LOG ( n -- ) ORDER @ 10 * + ORDER ! ;


: APPLY ( R [ R -- S ] -- S ) [: CLEAN ;] finally ;


: WRAPPER ( R [ R -- S ] [ -- ] -- S ) finally ;


: CONSUME ( n n -- ) [: 2drop ;] APPLY ;


: PRODUCE ( -- n n n ) [: 17 29 43 ;] APPLY ;


: REPLACE ( n -- n ) [: 7 * 3 + ;] APPLY ;


: PREFIX ( n n -- n n ) [: 7 * 3 + ;] [: CLEAN ;] finally ;


: BODY-THROWS ( -- ) [: E-BODY throw ;] [: CLEAN ;] finally ;


: CLEANUP-THROWS ( -- ) [: 17 29 43 ;] [: E-CLEANUP throw ;] finally ;


: BOTH-THROW ( -- ) [: E-BODY throw ;] [: E-CLEANUP throw ;] finally ;


: BRANCHED-THROW ( -- )
   [: ;] catch 0<> if E-CLEANUP throw then
   E-BODY throw ;


: INNER ( -- ) [: 1 LOG ;] [: 2 LOG ;] finally ;


: NESTED ( -- ) [: INNER 3 LOG ;] [: 4 LOG ;] finally ;


: INNER-THROWS ( -- ) [: 1 LOG E-BODY throw ;] [: 2 LOG ;] finally ;


: NESTED-THROWS ( -- ) [: INNER-THROWS ;] [: 3 LOG ;] finally ;


: REJECT ( ptr u8 n -- )
   2dup T-LABEL CHECK-QUIET-CANDIDATE! 0 T= ;


: RUN ( -- )
   T-RESET
   5 9 CONSUME
   PRODUCE 43 T= 29 T= 17 T=
   5 REPLACE 38 T=
   91 5 PREFIX 38 T= 91 T=
   CLEANUPS @ 4 T=
   [: BODY-THROWS ;] E-BODY TTHROWSQ
   CLEANUPS @ 5 T=
   [: CLEANUP-THROWS ;] E-CLEANUP TTHROWSQ
   [: BOTH-THROW ;] E-CLEANUP TTHROWSQ
   [: BRANCHED-THROW ;] E-BODY TTHROWSQ
   NESTED ORDER @ 1234 T=
   0 ORDER !
   [: NESTED-THROWS ;] E-BODY TTHROWSQ
   ORDER @ 123 T=
   s" FC-GOOD ( n -- n ) [: 1+ ;] [: ;] finally" CHECK-QUIET-CANDIDATE! -1 T=
   s" FC-STORED-BORROW ( -- ) [: 1+ ;] STORED !" REJECT
   s" FC-STORED-GOOD ( -- ) [: ;] STORED ! STORED @ execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" FC-INDIRECT-BORROW ( -- ) [: ;] [: 1+ ;] WRAPPER" REJECT
   s" FC-TICK-BORROW ( -- ) [: ;] [: 1+ ;] ['] finally execute" REJECT
   s" FC-PUSH ( -- ) [: ;] [: 1 ;] finally" REJECT
   s" FC-BORROW ( n -- n ) [: ;] [: 1+ ;] finally" REJECT
   s" FC-DROP ( n -- ) [: ;] [: drop ;] finally" REJECT
   s" FC-RSTACK ( -- ) [: ;] [: 1 >r ;] finally" REJECT
   s" FC-OPAQUE-CLEANUP ( -- ) [: ;] OPAQUE @ finally" REJECT
   s" FC-OPAQUE-BODY ( -- ) OPAQUE @ [: ;] finally" REJECT
   T-REPORT ;

RUN
;package
