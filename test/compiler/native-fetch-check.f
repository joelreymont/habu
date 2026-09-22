\ Frozen descriptor checks retain the JIT's active-variant and unsigned rules.
\ Tier-neutral by design: the descriptor check is called directly on hand-built
\ descriptors, and the cases that need a compiler run in a child of their own.
require src/compiler/native/fetch-check.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package NATIVE-FETCH-CHECK-TEST
private

create VALUE 3 cells allot
create SCALAR 1 , 0 , 2 , 0 ,
create NESTED
   3 ,
   2 , 2 , 0 ,
   1 , 2 , 1 , 2 , 0 , 2 ,
   0 , 2 , 2 , 2 , 0 , 2 , 1 , 0 , 2 ,
create UNALIGNED 19 cells 1+ allot
create OUT-OF-RANGE 1 , 1 , 2 , 0 ,
create OUT 256 allot
create ERR 256 allot

: VALUE! ( n n n -- ) {: inner:n middle:n outer:n :}
   inner VALUE CELL-VIEW !
   middle VALUE CELL-VIEW cell+ !
   outer VALUE CELL-VIEW 2 cells + ! ;

: SCALAR-CHECK ( n -- )
   0 0 VALUE! VALUE SCALAR 4 cells 1 NFETCH-CHECK:CHECK ;

: NESTED-CHECK ( n n n -- )
   VALUE! VALUE NESTED 19 cells 3 NFETCH-CHECK:CHECK ;

: UNALIGNED-CHECK ( -- )
   NESTED UNALIGNED 1+ 19 cells BYTE-COPY
   1 0 0 VALUE! VALUE UNALIGNED 1+ 19 cells 3 NFETCH-CHECK:CHECK ;

: REFUSES ( ptr u8 n -- )
   OUT 256 >LEN ERR 256 >LEN 1000 >MS SUBJECT:RUN
   85 T-OUTCOME-EXITED= {: outu:len erru:len :}
   outu LEN>N 0 T=
   ERR erru LEN>N S\" hb: bad layout tag\n" T$= ;

: REFUSES-DESCRIPTOR ( ptr u8 n -- )
   OUT 256 >LEN ERR 256 >LEN 1000 >MS SUBJECT:RUN
   76 T-OUTCOME-EXITED= {: outu:len erru:len :}
   outu LEN>N 0 T=
   ERR erru LEN>N S\" hb: bad layout descriptor\n" T$= ;

public

: TEST ( -- )
   T-RESET
   s" valid scalar and active nested tags" T-LABEL
   0 SCALAR-CHECK 1 SCALAR-CHECK
   1 0 0 NESTED-CHECK
   UNALIGNED-CHECK
   s" inactive variants do not inspect their payload tags" T-LABEL
   99 99 1 NESTED-CHECK
   99 1 0 NESTED-CHECK
   s" scalar domain rejects its exclusive limit and unsigned high values" T-LABEL
   s" package NATIVE-FETCH-CHECK-TEST 2 SCALAR-CHECK ;package" REFUSES
   s" package NATIVE-FETCH-CHECK-TEST -1 SCALAR-CHECK ;package" REFUSES
   s" active root and nested domains are checked before reading values" T-LABEL
   s" package NATIVE-FETCH-CHECK-TEST 0 0 2 NESTED-CHECK ;package" REFUSES
   s" package NATIVE-FETCH-CHECK-TEST 0 2 0 NESTED-CHECK ;package" REFUSES
   s" package NATIVE-FETCH-CHECK-TEST 2 0 0 NESTED-CHECK ;package" REFUSES
   s" malformed descriptor bounds fail before reading memory" T-LABEL
   s" package NATIVE-FETCH-CHECK-TEST VALUE NESTED 18 cells 3 NFETCH-CHECK:CHECK ;package" REFUSES-DESCRIPTOR
   s" package NATIVE-FETCH-CHECK-TEST VALUE OUT-OF-RANGE 4 cells 1 NFETCH-CHECK:CHECK ;package" REFUSES-DESCRIPTOR
   T-REPORT ;

;package

NATIVE-FETCH-CHECK-TEST:TEST
