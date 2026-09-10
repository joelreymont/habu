require lib/test.f

package STORED-QUOT-TEST
private

TYPED-VARIABLE STEP [ n -- n ]
2 TYPED-BUFFER STEPS [ n -- n ]
TYPED-VARIABLE ACTION [ -- ]
DYNAMIC-BUFFER MAPPED-STEPS [ n -- n ]

: INC ( n -- n ) 1+ ;
: DOUBLE ( n -- n ) 2 * ;
: FAIL ( -- ) 73 throw ;
: CALL ( n -- n ) STEP @ execute ;
: INDEXED ( n n -- n ) STEPS @ execute ;
: CATCH-STORED ( -- n ) ACTION @ catch ;

: PUT ( [ a -- a ] ptr [ a -- a ] -- ) ! ;

: STORE-LOOP ( n -- n ) {: count:n :}
   0 count 0 ?do
      [: INC ;] STEP !
      i CALL +
   loop
   count + ;

: MAPPED ( -- n )
   1 MAPPED-STEPS-RESERVE
   [: INC ;] 0 MAPPED-STEPS PUT
   17 0 MAPPED-STEPS @ execute
   MAPPED-STEPS-RELEASE ;


: INSTALL ( -- )
   [: INC ;] STEP !
   [: INC ;] 0 STEPS !
   [: DOUBLE ;] 1 STEPS !
   [: FAIL ;] ACTION ! ;


: RUN ( -- )
   T-RESET
   INSTALL
   17 CALL 18 T=
   91 17 CALL 18 T= 91 T=
   17 0 INDEXED 18 T=
   17 1 INDEXED 34 T=
   [: DOUBLE ;] STEP PUT
   17 CALL 34 T=
   MAPPED 18 T=
   3 STORE-LOOP 9 T=
   CATCH-STORED 73 T=
   T-REPORT ;

RUN
;package
