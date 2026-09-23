\ Frozen descriptor checks retain the JIT's active-variant and unsigned rules.
\ FREEZE validates a descriptor's structure and TAGS walks the tag rows of one
\ already frozen; CHECK is still the pair, so a hand-built descriptor is refused
\ before any memory is read.
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

\ The split pair the compiled fetch uses: FREEZE validates the structure once,
\ TAGS walks the tag rows of an already frozen descriptor.
: SCALAR-TAGS ( n -- )
   0 0 VALUE! VALUE SCALAR 4 cells 1 NFETCH-CHECK:FREEZE NFETCH-CHECK:TAGS ;

: NESTED-TAGS ( n n n -- )
   VALUE! VALUE NESTED 19 cells 3 NFETCH-CHECK:FREEZE NFETCH-CHECK:TAGS ;

: UNALIGNED-TAGS ( -- )
   NESTED UNALIGNED 1+ 19 cells BYTE-COPY
   1 0 0 VALUE! VALUE UNALIGNED 1+ 19 cells 3 NFETCH-CHECK:FREEZE NFETCH-CHECK:TAGS ;

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

\ The checker refuses the forgery instead of the runtime: only NFETCH-CHECK's
\ own private casts mint a frozen value, so no checked caller reaches TAGS with
\ a raw descriptor address.
: REFUSES-FORGERY ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:ptr wantu:n :}
   src srcu OUT 256 >LEN ERR 256 >LEN 1000 >MS SUBJECT:RUN
   70 T-OUTCOME-EXITED= {: outu:len erru:len :}
   outu LEN>N 0 T=
   ERR erru LEN>N want wantu T$= ;

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
   s" a frozen descriptor still walks its tags, and only its tags" T-LABEL
   0 SCALAR-TAGS 1 SCALAR-TAGS
   1 0 0 NESTED-TAGS
   UNALIGNED-TAGS
   99 99 1 NESTED-TAGS
   s" package NATIVE-FETCH-CHECK-TEST 0 0 2 NESTED-TAGS ;package" REFUSES
   s" only NFETCH-CHECK mints a frozen descriptor" T-LABEL
   s" package NATIVE-FETCH-CHECK-TEST : FORGED ( -- ) VALUE NESTED NFETCH-CHECK:TAGS ; ;package"
   S\" habu: in forged: at 'NFETCH-CHECK:TAGS' expected: ptr u8 nfetch-check:frozen<> actual: ptr a ptr b \nhook: non-certified definition: forged at 'NFETCH-CHECK:TAGS'\n"
   REFUSES-FORGERY
   T-REPORT ;

;package

NATIVE-FETCH-CHECK-TEST:TEST
