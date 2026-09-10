\ Inferred PRODUCT locals retain all cells through reads, calls and scopes.
require lib/test.f

package NPL
public

PRODUCT rules 0
   FIELD a n FIELD b n FIELD c n FIELD d n FIELD e n FIELD f n
;PRODUCT

PRODUCT wide 0
   FIELD f01 n FIELD f02 n FIELD f03 n FIELD f04 n FIELD f05 n FIELD f06 n
   FIELD f07 n FIELD f08 n FIELD f09 n FIELD f10 n FIELD f11 n FIELD f12 n
   FIELD f13 n FIELD f14 n FIELD f15 n FIELD f16 n FIELD f17 n FIELD f18 n
   FIELD f19 n FIELD f20 n FIELD f21 n FIELD f22 n FIELD f23 n FIELD f24 n
;PRODUCT

NEWTYPE item-id 0
NEWTYPE length 0
NEWTYPE angle 0
PRODUCT point 0 FIELD x length FIELD y length ;PRODUCT
SUMTYPE shape 0
   VARIANT core n n n n n n ;VARIANT
   VARIANT polygon item-id point angle ;VARIANT
;SUMTYPE

CAST: ITEM>N ( item-id -- n )
CAST: LENGTH>N ( length -- n )
CAST: ANGLE>N ( angle -- n )
CAST: >ITEM ( n -- item-id )
CAST: >LENGTH ( n -- length )
CAST: >ANGLE ( n -- angle )

-29103 constant E-SHAPE

: CORE-DATA ( shape -- n n n n n n )
   MATCH shape
      core OF ENDOF
      polygon OF {: item:item-id origin turn:angle :} E-SHAPE throw ENDOF
   ;MATCH ;

: CORE-PREFIX ( rules shape -- rules n n n n n n )
   MATCH shape
      core OF ENDOF
      polygon OF {: item:item-id origin turn:angle :} E-SHAPE throw ENDOF
   ;MATCH ;

: POLYGON-DATA ( shape -- n n n n )
   MATCH shape
      core OF drop drop drop drop drop drop E-SHAPE throw ENDOF
      polygon OF {: item:item-id origin turn:angle :}
         item ITEM>N origin NPL-POINT:UNMAKE
         {: x:length y:length :} x LENGTH>N y LENGTH>N turn ANGLE>N
      ENDOF
   ;MATCH ;

: POLYGON ( -- shape )
   7 >ITEM 8 >LENGTH 9 >LENGTH
   NPL-POINT:MAKE 10 >ANGLE NPL-SHAPE:POLYGON ;

: CHECK-NESTED-PAYLOAD ( -- )
   123 1 2 3 4 5 6 NPL-SHAPE:CORE CORE-DATA
   6 T= 5 T= 4 T= 3 T= 2 T= 1 T= 123 T=
   123 POLYGON POLYGON-DATA 10 T= 9 T= 8 T= 7 T= 123 T=
   [: POLYGON CORE-DATA drop drop drop drop drop drop ;] E-SHAPE TTHROWSQ
   [: 1 2 3 4 5 6 NPL-SHAPE:CORE POLYGON-DATA 2drop 2drop ;] E-SHAPE TTHROWSQ ;

2 TYPED-BUFFER RULES-AT rules


: VALIDATE ( rules -- )
   NPL-RULES:UNMAKE drop drop drop drop drop drop ;


: STORE ( n rules -- ) {: slot:n value :}
   slot {: index:n :}
   value index RULES-AT ! ;


: CHECK-STORE ( n rules -- ) {: slot:n value :}
   slot {: index:n :}
   value VALIDATE
   value index RULES-AT ! ;


: SELECT ( bool rules -- rules ) {: flag:bool value :}
   flag if
      value {: copy :} copy VALIDATE copy
   else
      7 {: padding:n :} padding drop value
   then ;


: KEEP-THROUGH-LOOP ( n rules -- rules ) {: count:n value :}
   count 0 ?do
      value {: copy :} copy VALIDATE
   loop
   value ;


: SIX ( -- rules ) 1 2 3 4 5 6 NPL-RULES:MAKE ;


: PROVIDE ( n -- rules ) drop SIX ;


: APPLY ( [ n -- rules ] n -- ) {: provider count:n :}
   count 0 ?do
      i provider execute {: value :}
      value VALIDATE value i RULES-AT !
   loop ;


: EXEC-DIRECT ( [ -- rules ] -- rules ) execute ;


: EXEC-MIXED ( [ -- n rules n ] -- n rules n )
   execute {: first:n value last:n :} first value last ;


: CHECK-SIX ( rules -- )
   NPL-RULES:UNMAKE 6 T= 5 T= 4 T= 3 T= 2 T= 1 T= ;

: CHECK-PAYLOAD-PREFIX ( -- )
   SIX 1 2 3 4 5 6 NPL-SHAPE:CORE CORE-PREFIX
   6 T= 5 T= 4 T= 3 T= 2 T= 1 T= CHECK-SIX ;


\ The bound value has more cells than this definition has source tokens.
: KEEP-WIDE ( wide -- wide ) {: value :} value ;


: CHECK-WIDE ( -- )
   1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
   NPL-WIDE:MAKE KEEP-WIDE NPL-WIDE:UNMAKE
   24 T= 23 T= 22 T= 21 T= 20 T= 19 T= 18 T= 17 T= 16 T= 15 T= 14 T= 13 T=
   12 T= 11 T= 10 T= 9 T= 8 T= 7 T= 6 T= 5 T= 4 T= 3 T= 2 T= 1 T= ;


: RUN ( -- )
   T-RESET
   0 SIX STORE
   1 11 12 13 14 15 16 NPL-RULES:MAKE CHECK-STORE
   0 RULES-AT @ CHECK-SIX
   1 RULES-AT @ NPL-RULES:UNMAKE
   16 T= 15 T= 14 T= 13 T= 12 T= 11 T=
   true SIX SELECT CHECK-SIX
   false SIX SELECT CHECK-SIX
   0 SIX KEEP-THROUGH-LOOP CHECK-SIX
   3 SIX KEEP-THROUGH-LOOP CHECK-SIX
   [: PROVIDE ;] 2 APPLY
   0 RULES-AT @ CHECK-SIX
   1 RULES-AT @ CHECK-SIX
   [: SIX ;] EXEC-DIRECT CHECK-SIX
   [: 17 SIX 29 ;] EXEC-MIXED 29 T= CHECK-SIX 17 T=
   CHECK-WIDE
   CHECK-NESTED-PAYLOAD
   CHECK-PAYLOAD-PREFIX
   T-REPORT ;

;package

NPL:RUN
