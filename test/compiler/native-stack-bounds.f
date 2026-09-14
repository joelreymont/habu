\ Native calls must validate their complete physical stack window before access.
1 set-tier
require test/engine-stack-lifecycle.f

package STACK-LIFECYCLE-TEST

: NATIVE-OUTPUTS ( -- )
   s" native empty callback accepts zero capacity" T-LABEL
   s" 1 set-tier create BUF 32 allot : EMPTY ( -- ) ; : GO ( -- ) ['] EMPTY BUF 0 run-in-stack ; GO"
   CHILD-RC 0 T=
   s" native two-cell result fits exactly" T-LABEL
   s" 1 set-tier create BUF 32 allot : VALUES ( -- n n ) 11 22 ; : TWO ( -- ) VALUES + 33 <> if 9 throw then ; : GO ( -- ) ['] TWO BUF 16 run-in-stack ; GO"
   CHILD-RC 0 T=
   s" native result refuses one remaining cell" T-LABEL
   s" 1 set-tier create BUF 32 allot : VALUES ( -- n n ) 11 22 ; : TWO ( -- ) VALUES 2drop ; : GO ( -- ) ['] TWO BUF 8 run-in-stack ; GO"
   REFUSED ;

: NATIVE-PATHS ( -- )
   s" untaken larger outputs do not reserve caller capacity" T-LABEL
   s" 1 set-tier create BUF 64 allot variable FLAG : WIDE ( -- n n n n ) 1 2 3 4 ; : SMALL ( -- n ) 7 ; : CHOOSE ( -- n ) FLAG @ if WIDE + + + else SMALL then ; : CHECK-VALUE ( -- ) CHOOSE 7 <> if 9 throw then ; : GO ( -- ) ['] CHECK-VALUE BUF 8 run-in-stack ; GO"
   CHILD-RC 0 T=
   s" repeated native calls restore the same result window" T-LABEL
   s" 1 set-tier create BUF 32 allot : VALUES ( -- n n ) 11 22 ; : TOTAL ( -- n ) VALUES + ; : REPEAT-CALL ( -- ) 12 0 do TOTAL 33 <> if 9 throw then loop ; : GO ( -- ) ['] REPEAT-CALL BUF 16 run-in-stack ; GO"
   CHILD-RC 0 T= ;

public
: RUN-NATIVE-BOUNDS ( -- )
   T-RESET NATIVE-OUTPUTS NATIVE-PATHS T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-NATIVE-BOUNDS
