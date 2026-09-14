\ Complete primitive and width-aware JIT transfers use the active allocation.
require test/engine-stack-lifecycle.f
require lib/string.f

package STACK-LIFECYCLE-TEST

: PAIR-SOURCE ( -- )
   SB-RESET
   s" 0 set-tier package SW public PRODUCT pair 0 FIELD a n FIELD b n ;PRODUCT create BUF 16384 allot " SB-APPEND ;

: PAIR-RC ( ptr u8 n -- n )
   PAIR-SOURCE SB-APPEND s"  ;package" SB-APPEND SB$ CHILD-RC ;

: PAIR-REFUSED ( ptr u8 n -- )
   PAIR-SOURCE SB-APPEND s"  ;package" SB-APPEND SB$ REFUSED ;

: PRIMITIVE-GROUPS ( -- )
   s" a primitive result pair fits exactly" T-LABEL
   S\" create BUF 32 allot : COUNTED ( -- ) c\" x\" count 2drop ; : GO ( -- ) ['] COUNTED BUF 16 run-in-stack ; GO"
   CHILD-RC 0 T=
   s" count reserves both results before consuming its input" T-LABEL
   S\" create BUF 32 allot : COUNTED ( -- ) c\" x\" count 2drop ; : GO ( -- ) ['] COUNTED BUF 8 run-in-stack ; GO" REFUSED
   s" a compiled string needs its complete result pair" T-LABEL
   S\" create BUF 32 allot : TEXT ( -- ) s\" x\" 2drop ; : GO ( -- ) ['] TEXT BUF 8 run-in-stack ; GO" REFUSED
   s" a primitive multi-result push reserves all three cells" T-LABEL
   s" create BUF 32 allot : PIPES ( -- ) pipe drop close close ; : GO ( -- ) ['] PIPES BUF 16 run-in-stack ; GO" REFUSED ;

: PAIR-TRANSFERS ( -- )
   s" a two-cell copy fits at the exact limit" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE dup drop drop ; : GO ( -- ) ['] COPY BUF 32 run-in-stack ; GO"
   PAIR-RC 0 T=
   s" a two-cell copy refuses one free cell" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE dup drop drop ; : GO ( -- ) ['] COPY BUF 24 run-in-stack ; GO" PAIR-REFUSED
   s" tuck reserves its copy before permuting either group" T-LABEL
   s" : COPY ( -- ) 1 11 22 SW-PAIR:MAKE tuck drop drop drop ; : GO ( -- ) ['] COPY BUF 32 run-in-stack ; GO" PAIR-REFUSED
   s" wide local reload copies its whole value" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE {: value :} value value 2drop ; : GO ( -- ) ['] COPY BUF 24 run-in-stack ; GO" PAIR-REFUSED
   s" wide locals retain order through their frame" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE {: value :} value value 2drop ; : GO ( -- ) ['] COPY BUF 32 run-in-stack ; GO"
   PAIR-RC 0 T= ;

: WIDE-RETURN-GROUPS ( -- )
   s" wide return move uses exactly the last two slots" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r r@ drop r> drop ; STACK-ABI:RETURN-CELLS 2 - data-base RSP-CELL + ! MOVE data-base RSP-CELL + @ STACK-ABI:RETURN-CELLS 2 - = ."
   PAIR-RC 0 T= OUT OUTLEN @ S\" -1\n" T$=
   s" wide return move needs two free slots" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r r> drop ; STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! MOVE" PAIR-REFUSED
   s" wide return pop proves its whole source before decrement" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r 1 data-base RSP-CELL + ! r> drop ; MOVE" PAIR-REFUSED
   s" wide return peek proves its whole source" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r 1 data-base RSP-CELL + ! r@ drop r> drop ; MOVE" PAIR-REFUSED ;

\ 513 cells crosses ADD/SUB imm12's byte range while staying inside the real
\ boot allocation. Nested products keep this a small checked source fixture.
: LARGE-SOURCE ( -- )
   SB-RESET
   s" 0 set-tier PRODUCT sw-double 1 FIELD a a FIELD b a ;PRODUCT PRODUCT sw-plus 1 FIELD a a FIELD b n ;PRODUCT " SB-APPEND
   s" 1 TYPED-BUFFER ITEM sw-plus<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>>>>>> create BUF 16384 allot " SB-APPEND ;

: LARGE-RC ( ptr u8 n -- n )
   LARGE-SOURCE SB-APPEND SB$ CHILD-RC ;

: LARGE-REFUSED ( ptr u8 n -- )
   LARGE-SOURCE SB-APPEND SB$ REFUSED ;

: LARGE-TRANSFERS ( -- )
   s" a 513-cell typed memory round trip uses full-width offsets" T-LABEL
   s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; : GO ( -- ) ['] MOVE BUF 4112 run-in-stack ; GO"
   LARGE-RC 0 T=
   s" a 513-cell typed fetch refuses one missing cell" T-LABEL
   s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; : GO ( -- ) ['] MOVE BUF 4096 run-in-stack ; GO" LARGE-REFUSED ;

: MEDIUM-SOURCE ( -- )
   LARGE-SOURCE
   s" 1 TYPED-BUFFER SMALL sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>>> " SB-APPEND ;

: MEDIUM-RC ( ptr u8 n -- n )
   MEDIUM-SOURCE SB-APPEND SB$ CHILD-RC ;

: MEDIUM-TRANSFERS ( -- )
   s" cumulative local frame release exceeds imm12 without truncating" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ {: a :} 0 SMALL @ {: b :} 0 SMALL @ {: c :} 0 SMALL @ {: d :} 1 {: z:n :} a drop b drop c drop d drop z drop ; : GO ( -- ) ['] MOVE BUF 2048 run-in-stack ; GO"
   MEDIUM-RC 0 T=
   s" a 128-cell copy fits its exact complete window" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ dup drop drop ; : GO ( -- ) ['] MOVE BUF 2048 run-in-stack ; GO"
   MEDIUM-RC 0 T=
   s" a 128-cell copy refuses one missing cell" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ dup drop drop ; : GO ( -- ) ['] MOVE BUF 2040 run-in-stack ; GO"
   MEDIUM-SOURCE SB-APPEND SB$ REFUSED ;

public
: RUN-WIDE ( -- )
   T-RESET PRIMITIVE-GROUPS PAIR-TRANSFERS WIDE-RETURN-GROUPS LARGE-TRANSFERS MEDIUM-TRANSFERS T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-WIDE
