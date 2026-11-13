\ engine-stack-wide.f - primitive and width-aware native transfers use the
\ active stack allocation.
require test/engine-stack-lifecycle-lib.f
require src/lib/string.f

package STACK-LIFECYCLE-TEST

: PAIR-TYPE-SOURCE ( -- )
   SB-RESET
   s" package SW public PRODUCT pair FIELD a n FIELD b n ;PRODUCT " SB-APPEND ;

: PAIR-RC ( ptr u8 n -- n )
   PAIR-TYPE-SOURCE SB-APPEND s"  ;package" SB-APPEND SB$ CHILD-RC ;

\ RATCHET-TAIL wraps the named transfer in `begin dup <transfer> recurse again`.
\ The declared `( n -- )` input makes it grow: the checker models `recurse` as
\ consuming the cell `dup` produced and nothing pops it at runtime, so each
\ level leaves the stack one cell deeper while the transfer runs whole and
\ unwinds. The body is live, and every transfer below is wider than one cell, so
\ the first write past the capacity is the transfer's own. The child dies on the
\ data guard page: ENGINE-ERROR:STACK-BOUNDS (102), "E-STACK-BOUNDS habu: stack
\ bounds exceeded (data)".
: RATCHET-TAIL ( ptr u8 n -- ) {: name:ptr nameu:n :}
   s"  : RATCHET ( n -- ) begin dup " SB-APPEND  name nameu SB-APPEND
   s"  recurse again ; : RATCHET0 ( -- ) 1 RATCHET ; RATCHET0" SB-APPEND ;

: PLAIN-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   SB-RESET defs defsu SB-APPEND name nameu RATCHET-TAIL
   SB$ REFUSED-DATA ;

: PAIR-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   PAIR-TYPE-SOURCE defs defsu SB-APPEND name nameu RATCHET-TAIL
   s"  ;package" SB-APPEND SB$ REFUSED-DATA ;

: PAIR-REFUSED-RETURN ( ptr u8 n -- )
   PAIR-TYPE-SOURCE SB-APPEND s"  ;package" SB-APPEND SB$ REFUSED-RETURN ;

: PRIMITIVE-GROUPS ( -- )
   s" count past the capacity faults the data guard page" T-LABEL
   S\" : COUNTED ( -- ) c\" x\" count 2drop ; " s" COUNTED" PLAIN-OVERFLOW
   s" a compiled string past the capacity faults the data guard page" T-LABEL
   S\" : TEXT ( -- ) s\" x\" 2drop ; " s" TEXT" PLAIN-OVERFLOW ;

: PAIR-TRANSFERS ( -- )
   s" a two-cell copy past the capacity faults the data guard page" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE dup drop drop ; " s" COPY" PAIR-OVERFLOW
   s" tuck past the capacity faults the data guard page" T-LABEL
   s" : TUCKED ( -- ) 1 11 22 SW-PAIR:MAKE tuck drop drop drop ; " s" TUCKED" PAIR-OVERFLOW
   s" a wide local reload past the capacity faults the data guard page" T-LABEL
   s" : RELOAD ( -- ) 11 22 SW-PAIR:MAKE {: value :} value value 2drop ; " s" RELOAD" PAIR-OVERFLOW ;

\ Set RSP inside the compiled word: the interpreter parks its own tokens on
\ the return stack between top-level words. An observable wide peek/pop
\ reaches the guarded VM mapping, so these measure its two-cell boundary.
: WIDE-RETURN-GROUPS ( -- )
   s" native wide return peek and pop preserve both cells" T-LABEL
   s" variable OLD-RSP TRUSTED: SET-EDGE ( -- ) data-base RSP-CELL + @ OLD-RSP ! STACK-ABI:RETURN-CELLS 2 - data-base RSP-CELL + ! ; TRUSTED: RESTORE-EDGE ( -- ) data-base RSP-CELL + @ STACK-ABI:RETURN-CELLS 2 - = . OLD-RSP @ data-base RSP-CELL + ! ; : MOVE ( -- ) SET-EDGE 11 22 SW-PAIR:MAKE >r r@ SW-PAIR:UNMAKE . . r> SW-PAIR:UNMAKE . . RESTORE-EDGE ; MOVE"
   PAIR-RC 0 T=
   OUT OUTLEN @ S\" 22\n11\n22\n11\n-1\n" T$=
   s" observable native wide return transfer needs two free slots" T-LABEL
   s" TRUSTED: SET-EDGE ( n -- ) data-base RSP-CELL + ! ; : MOVE ( -- ) STACK-ABI:RETURN-CELLS 1 - SET-EDGE 11 22 SW-PAIR:MAKE >r r@ SW-PAIR:UNMAKE . . r> SW-PAIR:UNMAKE . . ; MOVE"
   PAIR-REFUSED-RETURN
   s" native wide return pop needs two live slots" T-LABEL
   s" TRUSTED: SET-EDGE ( n -- ) data-base RSP-CELL + ! ; : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r 1 SET-EDGE r> SW-PAIR:UNMAKE . . ; MOVE"
   PAIR-REFUSED-RETURN
   s" native wide return peek needs two live slots" T-LABEL
   s" TRUSTED: SET-EDGE ( n -- ) data-base RSP-CELL + ! ; : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r 1 SET-EDGE r@ SW-PAIR:UNMAKE . . r> SW-PAIR:UNMAKE . . ; MOVE"
   PAIR-REFUSED-RETURN ;


\ The 513-cell value's byte offsets pass arm64 ADD/SUB's 12-bit immediate.
: LARGE-TYPE-SOURCE ( -- )
   SB-RESET
   s" PRODUCT sw-double<a> FIELD a a FIELD b a ;PRODUCT PRODUCT sw-plus<a> FIELD a a FIELD b n ;PRODUCT " SB-APPEND
   s" 1 TYPED-BUFFER ITEM sw-plus<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>>>>>> " SB-APPEND ;

: LARGE-RC ( ptr u8 n -- n )
   LARGE-TYPE-SOURCE SB-APPEND SB$ CHILD-RC ;

: LARGE-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   LARGE-TYPE-SOURCE defs defsu SB-APPEND name nameu RATCHET-TAIL
   SB$ REFUSED-DATA ;

: LARGE-TRANSFERS ( -- )
   s" a 513-cell typed memory round trip uses full-width offsets" T-LABEL
   s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; MOVE"
   LARGE-RC 0 T=
   s" a 513-cell typed fetch past the capacity faults the data guard page" T-LABEL
   s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; "
   s" MOVE" LARGE-OVERFLOW ;

: MEDIUM-TYPE-SOURCE ( -- )
   LARGE-TYPE-SOURCE
   s" 1 TYPED-BUFFER SMALL sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>>> " SB-APPEND ;

: MEDIUM-RC ( ptr u8 n -- n )
   MEDIUM-TYPE-SOURCE SB-APPEND SB$ CHILD-RC ;

: MEDIUM-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   MEDIUM-TYPE-SOURCE defs defsu SB-APPEND name nameu RATCHET-TAIL
   SB$ REFUSED-DATA ;

: MEDIUM-TRANSFERS ( -- )
   s" cumulative local frame release exceeds imm12 without truncating" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ {: a :} 0 SMALL @ {: b :} 0 SMALL @ {: c :} 0 SMALL @ {: d :} 1 {: z:n :} a drop b drop c drop d drop z drop ; MOVE"
   MEDIUM-RC 0 T=
   s" a 128-cell copy past the capacity faults the data guard page" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ dup drop drop ; " s" MOVE" MEDIUM-OVERFLOW ;

public
: RUN-WIDE ( -- )
   T-RESET PRIMITIVE-GROUPS PAIR-TRANSFERS WIDE-RETURN-GROUPS LARGE-TRANSFERS MEDIUM-TRANSFERS T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-WIDE
