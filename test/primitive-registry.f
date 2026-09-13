require lib/test.f
require src/habu/primitive-registry.f

package PRIMITIVE-REGISTRY-TEST
using ENGINE-PRIMS

1025 constant TOTAL
39 constant NAME-SIZE
create NAME-BUF NAME-SIZE allot

: LETTER ( n n -- n ) + 26 mod 65 + ;
: FILL-NAME ( n -- ) {: row:n :}
   NAME-SIZE 0 ?do row i LETTER NAME-BUF i + c! loop ;

: APPEND ( n -- ) {: row:n :}
   row FILL-NAME
   NAME-BUF NAME-SIZE row 1000 + >LABEL row 1002 + >LABEL ADD row T=
   row 32 mod row WID!
   row 10000 + >LABEL row NAME-LABEL! ;

: CHECK-ROW ( n -- ) {: row:n :}
   row FIRST-LABEL LABEL>N row 1000 + T=
   row LAST-LABEL LABEL>N row 1002 + T=
   row NAME-LABEL LABEL>N row 10000 + T=
   row WID row 32 mod T=
   row NAME$ {: name:ptr size:n :}
   size NAME-SIZE T=
   size 0 ?do name i + c@ row i LETTER T= loop ;

: BAD-SIZE ( -- ) NAME-BUF -1 1 >LABEL 2 >LABEL ADD drop ;
: HUGE-SIZE ( -- ) NAME-BUF $7FFFFFFFFFFFFFFF 1 >LABEL 2 >LABEL ADD drop ;
: BAD-ROW ( -- ) -1 FIRST-LABEL drop ;
: PAST-END ( -- ) ENGINE-PRIMS:COUNT FIRST-LABEL drop ;

: RUN ( -- )
   T-RESET RESET
   TOTAL 0 ?do i APPEND loop
   \ All rows must retain their own names after the shared pool grows and the
   \ caller overwrites the original name buffer on every append.
   ENGINE-PRIMS:COUNT TOTAL T=
   TOTAL 0 ?do i CHECK-ROW loop
   ['] BAD-SIZE 7121 TTHROWS
   ['] HUGE-SIZE 7121 TTHROWS
   ['] BAD-ROW 7122 TTHROWS
   ['] PAST-END 7122 TTHROWS
   ENGINE-PRIMS:COUNT TOTAL T= 0 CHECK-ROW TOTAL 1- CHECK-ROW
   RESET ENGINE-PRIMS:COUNT 0 T=
   ['] PAST-END 7122 TTHROWS
   0 APPEND 0 CHECK-ROW ENGINE-PRIMS:COUNT 1 T=
   RELEASE RELEASE ENGINE-PRIMS:COUNT 0 T=
   0 APPEND 0 CHECK-ROW RELEASE
   T-REPORT ;

RUN
;package
