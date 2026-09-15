\ Complete primitive and width-aware JIT transfers use the active allocation.
require test/engine-stack-lifecycle.f
require lib/string.f

package STACK-LIFECYCLE-TEST

: PAIR-TYPE-SOURCE ( -- )
   SB-RESET
   s" 0 set-tier package SW public PRODUCT pair 0 FIELD a n FIELD b n ;PRODUCT " SB-APPEND ;

\ BUF is a real guarded mapping (whole STACK-ABI:PAGE-BYTES multiples only),
\ used by the success cases below: run-in-stack now refuses anything else, so
\ every "fits" case runs on the full 64 KB stack rather than a size tuned to
\ the transfer's exact width -- there is no way to request a smaller guarded
\ stack any more.
: PAIR-SOURCE ( -- )
   PAIR-TYPE-SOURCE
   s" require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop constant BUF " SB-APPEND ;

\ BUF is a plain create/allot buffer here: GUARDED-EXTENT? refuses it outright
\ (it names an address inside the DATA region) before the JIT body it would
\ have run ever executes. Used for the data-stack-capacity "refuses" cases,
\ which used to depend on a per-transfer capacity check that no longer exists.
: PAIR-SOURCE-PLAIN ( -- )
   PAIR-TYPE-SOURCE
   s" create BUF 16384 allot " SB-APPEND ;

: PAIR-RC ( ptr u8 n -- n )
   PAIR-SOURCE SB-APPEND s"  ;package" SB-APPEND SB$ CHILD-RC ;

: PAIR-REFUSED ( ptr u8 n -- )
   PAIR-SOURCE-PLAIN SB-APPEND s"  ;package" SB-APPEND SB$ UNGUARDED-REFUSED ;

\ WIDE-RETURN-GROUPS never touches BUF (its bodies only manipulate the return
\ stack), so it only needs PAIR-SOURCE for the SW package/PRODUCT declaration;
\ its refusals are real return-stack guard-page faults.
: PAIR-REFUSED-RETURN ( ptr u8 n -- )
   PAIR-SOURCE SB-APPEND s"  ;package" SB-APPEND SB$ REFUSED-RETURN ;

: PRIMITIVE-GROUPS ( -- )
   s" a primitive result pair fits on a guarded stack" T-LABEL
   S\" require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop constant BUF : COUNTED ( -- ) c\" x\" count 2drop ; : GO ( -- ) ['] COUNTED BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   CHILD-RC 0 T=
   s" count on an unguarded buffer is refused" T-LABEL
   S\" create BUF 32 allot : COUNTED ( -- ) c\" x\" count 2drop ; : GO ( -- ) ['] COUNTED BUF 8 run-in-stack ; GO" UNGUARDED-REFUSED
   s" a compiled string on an unguarded buffer is refused" T-LABEL
   S\" create BUF 32 allot : TEXT ( -- ) s\" x\" 2drop ; : GO ( -- ) ['] TEXT BUF 8 run-in-stack ; GO" UNGUARDED-REFUSED
   s" a primitive multi-result push on an unguarded buffer is refused" T-LABEL
   s" create BUF 32 allot : PIPES ( -- ) pipe drop close close ; : GO ( -- ) ['] PIPES BUF 16 run-in-stack ; GO" UNGUARDED-REFUSED ;

: PAIR-TRANSFERS ( -- )
   s" a two-cell copy runs on a guarded stack" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE dup drop drop ; : GO ( -- ) ['] COPY BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   PAIR-RC 0 T=
   s" a two-cell copy on an unguarded buffer is refused" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE dup drop drop ; : GO ( -- ) ['] COPY BUF 24 run-in-stack ; GO" PAIR-REFUSED
   s" tuck on an unguarded buffer is refused" T-LABEL
   s" : COPY ( -- ) 1 11 22 SW-PAIR:MAKE tuck drop drop drop ; : GO ( -- ) ['] COPY BUF 32 run-in-stack ; GO" PAIR-REFUSED
   s" wide local reload on an unguarded buffer is refused" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE {: value :} value value 2drop ; : GO ( -- ) ['] COPY BUF 24 run-in-stack ; GO" PAIR-REFUSED
   s" wide locals retain order through their frame" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE {: value :} value value 2drop ; : GO ( -- ) ['] COPY BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   PAIR-RC 0 T= ;

: WIDE-RETURN-GROUPS ( -- )
   s" wide return move uses exactly the last two slots" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r r@ drop r> drop ; STACK-ABI:RETURN-CELLS 2 - data-base RSP-CELL + ! MOVE data-base RSP-CELL + @ STACK-ABI:RETURN-CELLS 2 - = ."
   PAIR-RC 0 T= OUT OUTLEN @ S\" -1\n" T$=
   s" wide return move needs two free slots" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r r> drop ; STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! MOVE" PAIR-REFUSED-RETURN
   s" wide return pop proves its whole source before decrement" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r 1 data-base RSP-CELL + ! r> drop ; MOVE" PAIR-REFUSED-RETURN
   s" wide return peek proves its whole source" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r 1 data-base RSP-CELL + ! r@ drop r> drop ; MOVE" PAIR-REFUSED-RETURN ;

\ 513 cells crosses ADD/SUB imm12's byte range while staying inside a single
\ guarded stack's 64 KB capacity. Nested products keep this a small checked
\ source fixture.
: LARGE-TYPE-SOURCE ( -- )
   SB-RESET
   s" 0 set-tier PRODUCT sw-double 1 FIELD a a FIELD b a ;PRODUCT PRODUCT sw-plus 1 FIELD a a FIELD b n ;PRODUCT " SB-APPEND
   s" 1 TYPED-BUFFER ITEM sw-plus<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>>>>>> " SB-APPEND ;

: LARGE-SOURCE ( -- )
   LARGE-TYPE-SOURCE
   s" require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop constant BUF " SB-APPEND ;

: LARGE-SOURCE-PLAIN ( -- )
   LARGE-TYPE-SOURCE
   s" create BUF 16384 allot " SB-APPEND ;

: LARGE-RC ( ptr u8 n -- n )
   LARGE-SOURCE SB-APPEND SB$ CHILD-RC ;

: LARGE-REFUSED ( ptr u8 n -- )
   LARGE-SOURCE-PLAIN SB-APPEND SB$ UNGUARDED-REFUSED ;

: LARGE-TRANSFERS ( -- )
   s" a 513-cell typed memory round trip uses full-width offsets" T-LABEL
   s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   LARGE-RC 0 T=
   s" a 513-cell typed fetch on an unguarded buffer is refused" T-LABEL
   s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; : GO ( -- ) ['] MOVE BUF 4096 run-in-stack ; GO" LARGE-REFUSED ;

: MEDIUM-TYPE-SOURCE ( -- )
   LARGE-TYPE-SOURCE
   s" 1 TYPED-BUFFER SMALL sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>>> " SB-APPEND ;

: MEDIUM-SOURCE ( -- )
   MEDIUM-TYPE-SOURCE
   s" require lib/memory.f STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop constant BUF " SB-APPEND ;

: MEDIUM-SOURCE-PLAIN ( -- )
   MEDIUM-TYPE-SOURCE
   s" create BUF 16384 allot " SB-APPEND ;

: MEDIUM-RC ( ptr u8 n -- n )
   MEDIUM-SOURCE SB-APPEND SB$ CHILD-RC ;

: MEDIUM-TRANSFERS ( -- )
   s" cumulative local frame release exceeds imm12 without truncating" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ {: a :} 0 SMALL @ {: b :} 0 SMALL @ {: c :} 0 SMALL @ {: d :} 1 {: z:n :} a drop b drop c drop d drop z drop ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   MEDIUM-RC 0 T=
   s" a 128-cell copy runs on a guarded stack" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ dup drop drop ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   MEDIUM-RC 0 T=
   s" a 128-cell copy on an unguarded buffer is refused" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ dup drop drop ; : GO ( -- ) ['] MOVE BUF 2040 run-in-stack ; GO"
   MEDIUM-SOURCE-PLAIN SB-APPEND SB$ UNGUARDED-REFUSED ;

public
: RUN-WIDE ( -- )
   T-RESET PRIMITIVE-GROUPS PAIR-TRANSFERS WIDE-RETURN-GROUPS LARGE-TRANSFERS MEDIUM-TRANSFERS T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-WIDE
