\ engine-stack-wide.f - primitive and width-aware native transfers use the
\ active stack allocation.
require test/engine-stack-lifecycle-lib.f
require src/lib/string.f

package STACK-LIFECYCLE-TEST

: PAIR-TYPE-SOURCE ( -- )
   SB-RESET
   s" package SW public PRODUCT pair FIELD a n FIELD b n ;PRODUCT " SB-APPEND ;

\ BUF is a real guarded mapping in whole STACK-ABI:PAGE-BYTES multiples, the
\ only extent run-in-stack accepts, so every case runs on the full 64 KB stack.
\ test/engine-stack-lifecycle-lib.f UNGUARDED-UNCAUGHT and test/stack-guard.f
\ RUN-IN-STACK-REFUSALS own the refusal. The address lives in a declared pointer
\ cell read through an accessor named BUF: a raw cell such as a `constant` never
\ holds an address (E-RAW-CELL-PTR).
: GUARDED-BUF ( -- )
   s" require src/lib/memory.f PTR-VARIABLE BUF-A STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop BUF-A ! : BUF ( -- ptr u8 ) BUF-A @ ; " SB-APPEND ;

: PAIR-SOURCE ( -- )
   PAIR-TYPE-SOURCE GUARDED-BUF ;

: PAIR-RC ( ptr u8 n -- n )
   PAIR-SOURCE SB-APPEND s"  ;package" SB-APPEND SB$ CHILD-RC ;

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
   s"  recurse again ; : RATCHET0 ( -- ) 1 RATCHET ; " SB-APPEND
   s" : GO ( -- ) ['] RATCHET0 BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO" SB-APPEND ;

: PLAIN-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   SB-RESET GUARDED-BUF defs defsu SB-APPEND name nameu RATCHET-TAIL
   SB$ REFUSED-DATA ;

: PAIR-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   PAIR-SOURCE defs defsu SB-APPEND name nameu RATCHET-TAIL
   s"  ;package" SB-APPEND SB$ REFUSED-DATA ;

\ WIDE-RETURN-GROUPS never touches BUF (its bodies only manipulate the return
\ stack), so it only needs PAIR-SOURCE for the SW package/PRODUCT declaration;
\ its refusals are real return-stack guard-page faults.
: PAIR-REFUSED-RETURN ( ptr u8 n -- )
   PAIR-SOURCE SB-APPEND s"  ;package" SB-APPEND SB$ REFUSED-RETURN ;

: PRIMITIVE-GROUPS ( -- )
   s" a primitive result pair fits on a guarded stack" T-LABEL
   S\" require src/lib/memory.f PTR-VARIABLE BUF-A STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop BUF-A ! : BUF ( -- ptr u8 ) BUF-A @ ; : COUNTED ( -- ) c\" x\" count 2drop ; : GO ( -- ) ['] COUNTED BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   CHILD-RC 0 T=
   s" count past the capacity faults the data guard page" T-LABEL
   S\" : COUNTED ( -- ) c\" x\" count 2drop ; " s" COUNTED" PLAIN-OVERFLOW
   s" a compiled string past the capacity faults the data guard page" T-LABEL
   S\" : TEXT ( -- ) s\" x\" 2drop ; " s" TEXT" PLAIN-OVERFLOW
   \ pipe is the one multi-result push that cannot be ratcheted: each level
   \ would hold two more open descriptors and the process would run out of
   \ them long before 64 KB of stack, so this transfer is executed rather than
   \ overflowed.
   s" a primitive multi-result push runs on a guarded stack" T-LABEL
   s" require src/lib/memory.f PTR-VARIABLE BUF-A STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop BUF-A ! : BUF ( -- ptr u8 ) BUF-A @ ; : PIPES ( -- ) pipe drop close close ; : GO ( -- ) ['] PIPES BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   CHILD-RC 0 T= ;

: PAIR-TRANSFERS ( -- )
   s" a two-cell copy runs on a guarded stack" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE dup drop drop ; : GO ( -- ) ['] COPY BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   PAIR-RC 0 T=
   s" a two-cell copy past the capacity faults the data guard page" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE dup drop drop ; " s" COPY" PAIR-OVERFLOW
   s" tuck past the capacity faults the data guard page" T-LABEL
   s" : TUCKED ( -- ) 1 11 22 SW-PAIR:MAKE tuck drop drop drop ; " s" TUCKED" PAIR-OVERFLOW
   s" a wide local reload past the capacity faults the data guard page" T-LABEL
   s" : RELOAD ( -- ) 11 22 SW-PAIR:MAKE {: value :} value value 2drop ; " s" RELOAD" PAIR-OVERFLOW
   s" wide locals retain order through their frame" T-LABEL
   s" : COPY ( -- ) 11 22 SW-PAIR:MAKE {: value :} value value 2drop ; : GO ( -- ) ['] COPY BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   PAIR-RC 0 T=
   \ A wide transfer executed while the switched allocation is active, proved
   \ by its result rather than by an exit code: the store runs on BUF's stack
   \ and the value is read back after the switch has been unwound.
   s" a wide store on the switched stack reaches the caller's buffer" T-LABEL
   s" 1 TYPED-BUFFER SLOT pair : STORE ( -- ) 11 22 SW-PAIR:MAKE 0 SLOT ! ; : GO ( -- ) ['] STORE BUF STACK-ABI:PAGE-BYTES run-in-stack 0 SLOT @ SW-PAIR:UNMAKE . . ; GO"
   PAIR-RC 0 T= OUT OUTLEN @ S\" 22\n11\n" T$= ;

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

: LARGE-SOURCE ( -- )
   LARGE-TYPE-SOURCE GUARDED-BUF ;

: LARGE-RC ( ptr u8 n -- n )
   LARGE-SOURCE SB-APPEND SB$ CHILD-RC ;

: LARGE-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   LARGE-SOURCE defs defsu SB-APPEND name nameu RATCHET-TAIL
   SB$ REFUSED-DATA ;

: LARGE-TRANSFERS ( -- )
   s" a 513-cell typed memory round trip uses full-width offsets" T-LABEL
   s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   LARGE-RC 0 T=
   s" a 513-cell typed fetch past the capacity faults the data guard page" T-LABEL
   s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; "
   s" MOVE" LARGE-OVERFLOW ;

: MEDIUM-TYPE-SOURCE ( -- )
   LARGE-TYPE-SOURCE
   s" 1 TYPED-BUFFER SMALL sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>>> " SB-APPEND ;

: MEDIUM-SOURCE ( -- )
   MEDIUM-TYPE-SOURCE GUARDED-BUF ;

: MEDIUM-RC ( ptr u8 n -- n )
   MEDIUM-SOURCE SB-APPEND SB$ CHILD-RC ;

: MEDIUM-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   MEDIUM-SOURCE defs defsu SB-APPEND name nameu RATCHET-TAIL
   SB$ REFUSED-DATA ;

: MEDIUM-TRANSFERS ( -- )
   s" cumulative local frame release exceeds imm12 without truncating" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ {: a :} 0 SMALL @ {: b :} 0 SMALL @ {: c :} 0 SMALL @ {: d :} 1 {: z:n :} a drop b drop c drop d drop z drop ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   MEDIUM-RC 0 T=
   s" a 128-cell copy runs on a guarded stack" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ dup drop drop ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   MEDIUM-RC 0 T=
   s" a 128-cell copy past the capacity faults the data guard page" T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ dup drop drop ; " s" MOVE" MEDIUM-OVERFLOW ;

public
: RUN-WIDE ( -- )
   T-RESET PRIMITIVE-GROUPS PAIR-TRANSFERS WIDE-RETURN-GROUPS LARGE-TRANSFERS MEDIUM-TRANSFERS T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-WIDE
