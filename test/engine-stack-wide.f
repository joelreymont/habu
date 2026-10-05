\ Complete primitive and width-aware JIT transfers use the active allocation.
require test/engine-stack-lifecycle-lib.f
require lib/string.f

package STACK-LIFECYCLE-TEST

\ Intel has no tier 0; these checked transfer bodies run through native HIR.
: SOURCE-TIER ( -- )
   HB-TARGET-LINUX-X86-64? if s" 1 set-tier " else s" 0 set-tier " then SB-APPEND ;

: PAIR-TYPE-SOURCE ( -- )
   SB-RESET
   SOURCE-TIER
   s" package SW public PRODUCT pair 0 FIELD a n FIELD b n ;PRODUCT " SB-APPEND ;

\ BUF is a real guarded mapping (whole STACK-ABI:PAGE-BYTES multiples only):
\ run-in-stack refuses anything else, so every case below runs on the full
\ 64 KB stack rather than on an extent tuned to the transfer's own width --
\ there is no way to request a smaller guarded stack any more. The refusal
\ itself is not retested here: test/engine-stack-lifecycle-lib.f UNGUARDED-UNCAUGHT
\ owns the uncaught child and test/stack-guard.f RUN-IN-STACK-REFUSALS owns one
\ case per GUARDED-EXTENT? clause.
\ The mapping's address is held in a DECLARED pointer cell and read back through
\ an accessor of the same name, so every case below still spells `BUF`. A
\ `constant` is raw storage and a raw cell never holds an address, so the
\ `constant BUF` this used to be stopped certifying with the child's `BUF` as a
\ `ptr u8` (E-RAW-CELL-PTR).
: GUARDED-BUF ( -- )
   s" require lib/memory.f PTR-VARIABLE BUF-A STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop BUF-A ! : BUF ( -- ptr u8 ) BUF-A @ ; " SB-APPEND ;

: PAIR-SOURCE ( -- )
   PAIR-TYPE-SOURCE GUARDED-BUF ;

: PAIR-RC ( ptr u8 n -- n )
   PAIR-SOURCE SB-APPEND s"  ;package" SB-APPEND SB$ CHILD-RC ;

\ The capacity boundary these transfers used to be checked against, restored
\ on the only stack that still has one: the guard page above a real mapping.
\ RATCHET-TAIL wraps the named transfer in `begin dup <transfer> recurse
\ again`. The declared `( n -- )` input is what makes that grow -- the checker
\ models `recurse` as consuming the one cell `dup` just produced, and nothing
\ pops it at runtime -- so each level leaves the stack exactly ONE cell deeper
\ while the transfer itself runs whole and unwinds. Two things follow. The
\ body is live: it is what fills the stack, thousands of times over, and a
\ body that never ran could not. And because the ratchet advances one cell at
\ a time while every transfer named below is wider than one cell, the first
\ write past the capacity is always the transfer's own -- it arrives at a
\ depth where the ratchet still fits and its own width no longer does. The
\ child dies on the data guard page: ENGINE-ERROR:STACK-BOUNDS (102),
\ "hb: stack bounds exceeded (data)".
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
   S\" require lib/memory.f PTR-VARIABLE BUF-A STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop BUF-A ! : BUF ( -- ptr u8 ) BUF-A @ ; : COUNTED ( -- ) c\" x\" count 2drop ; : GO ( -- ) ['] COUNTED BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
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
   s" require lib/memory.f PTR-VARIABLE BUF-A STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop BUF-A ! : BUF ( -- ptr u8 ) BUF-A @ ; : PIPES ( -- ) pipe drop close close ; : GO ( -- ) ['] PIPES BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
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

\ These subjects select tier 1 before defining their bodies. VALUES publishes
\ two distinct results at one native return; TAKE consumes them at one native
\ call. ARM also checks the selected post-index pair instruction. The ratchet
\ advances by one cell, so VALUES crosses the data guard while returning.
: NATIVE-PAIR-SOURCE ( -- )
   SB-RESET s" 1 set-tier require src/habu/xref.f " SB-APPEND GUARDED-BUF
   s" : VALUES ( -- n n ) 11 22 ; " SB-APPEND
   HB-TARGET-LINUX-X86-64? if exit then
   s" : CODE@ ( n -- n ) {: at:n :} at XREF-N>U8 c@ at 1+ XREF-N>U8 c@ 8 lshift or " SB-APPEND
   s" at 2 + XREF-N>U8 c@ 16 lshift or at 3 + XREF-N>U8 c@ 24 lshift or ; " SB-APPEND
   S\" : SELECTED ( -- ) s\" VALUES\" XREF-FIND {: rec:ptr :} rec XREF-FOUND? 0= if s\" missing VALUES\" 1 die then " SB-APPEND
   s" 0 rec XREF-CODE-BYTES 4 / 0 ?do rec XREF-START i 4 * + CODE@ {: w:n :} " SB-APPEND
   s" w 4290772992 and 2826960896 = w 5 rshift 31 and 19 = and if 1+ then loop " SB-APPEND
   S\" 1 <> if s\" VALUES has no post-index pair\" 1 die then ; SELECTED " SB-APPEND ;

: NATIVE-TRANSFERS ( -- )
   s" native calls and returns preserve two ordered results" T-LABEL
   NATIVE-PAIR-SOURCE
   s" variable ANSWER : TAKE ( n n -- ) 3 * swap 5 * + ANSWER ! ; " SB-APPEND
   S\" : MOVE ( -- ) 79 VALUES TAKE 79 <> if s\" displaced sentinel\" 1 die then ; " SB-APPEND
   s" : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ANSWER @ . ; GO" SB-APPEND
   SB$ CHILD-RC 0 T= OUT OUTLEN @ S\" 121\n" T$=
   s" native two-result return faults on its second guarded cell" T-LABEL
   NATIVE-PAIR-SOURCE
   s" : MOVE ( -- ) VALUES 2drop ; " SB-APPEND
   s" MOVE" RATCHET-TAIL SB$ REFUSED-DATA ;

\ Set RSP inside the compiled word: the interpreter parks its own tokens on
\ the return stack between top-level words. An observable wide peek/pop
\ reaches the guarded VM mapping, so these measure its two-cell boundary.
: WIDE-RETURN-NATIVE ( -- )
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

: WIDE-RETURN-ARM ( -- )
   s" wide return move uses exactly the last two slots" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r r@ drop r> drop ; STACK-ABI:RETURN-CELLS 2 - data-base RSP-CELL + ! MOVE data-base RSP-CELL + @ STACK-ABI:RETURN-CELLS 2 - = ."
   PAIR-RC 0 T= OUT OUTLEN @ S\" -1\n" T$=
   s" wide return move needs two free slots" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r r> drop ; STACK-ABI:RETURN-CELLS 1 - data-base RSP-CELL + ! MOVE" PAIR-REFUSED-RETURN
   s" wide return pop proves its whole source before decrement" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r 1 data-base RSP-CELL + ! r> drop ; MOVE" PAIR-REFUSED-RETURN
   s" wide return peek proves its whole source" T-LABEL
   s" : MOVE ( -- ) 11 22 SW-PAIR:MAKE >r 1 data-base RSP-CELL + ! r@ drop r> drop ; MOVE" PAIR-REFUSED-RETURN ;

: WIDE-RETURN-GROUPS ( -- )
   HB-TARGET-LINUX-X86-64? if WIDE-RETURN-NATIVE else WIDE-RETURN-ARM then ;

\ ARM's 513-cell value crosses ADD/SUB imm12's byte range. Intel's native
\ elaborator holds at most 64 cells in its value vector, so a 33-cell value
\ at buffer index 16 exercises a 4224-byte displacement inside that limit.
: LARGE-TYPE-SOURCE ( -- )
   SB-RESET
   SOURCE-TIER
   s" PRODUCT sw-double 1 FIELD a a FIELD b a ;PRODUCT PRODUCT sw-plus 1 FIELD a a FIELD b n ;PRODUCT " SB-APPEND
   HB-TARGET-LINUX-X86-64? if
      s" 17 TYPED-BUFFER ITEM sw-plus<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>> " SB-APPEND
   else
      s" 1 TYPED-BUFFER ITEM sw-plus<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>>>>>> " SB-APPEND
   then ;

: LARGE-SOURCE ( -- )
   LARGE-TYPE-SOURCE GUARDED-BUF ;

: LARGE-RC ( ptr u8 n -- n )
   LARGE-SOURCE SB-APPEND SB$ CHILD-RC ;

: LARGE-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   LARGE-SOURCE defs defsu SB-APPEND name nameu RATCHET-TAIL
   SB$ REFUSED-DATA ;

: LARGE-TRANSFERS ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" a 33-cell typed memory round trip uses a 4224-byte offset"
   else
      s" a 513-cell typed memory round trip uses full-width offsets"
   then T-LABEL
   HB-TARGET-LINUX-X86-64? if
      s" : MOVE ( -- ) 16 ITEM {: dest :} dest @ dest ! ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   else
      s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   then
   LARGE-RC 0 T=
   HB-TARGET-LINUX-X86-64? if
      s" a 33-cell typed fetch past capacity faults the data guard page"
   else
      s" a 513-cell typed fetch past the capacity faults the data guard page"
   then T-LABEL
   HB-TARGET-LINUX-X86-64? if
      s" : MOVE ( -- ) 16 ITEM {: dest :} dest @ dest ! ; "
   else
      s" : MOVE ( -- ) 0 ITEM {: dest :} dest @ dest ! ; "
   then
   s" MOVE" LARGE-OVERFLOW ;

: MEDIUM-TYPE-SOURCE ( -- )
   LARGE-TYPE-SOURCE
   HB-TARGET-LINUX-X86-64? if
      s" 1 TYPED-BUFFER SMALL sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>> " SB-APPEND
   else
      s" 1 TYPED-BUFFER SMALL sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<sw-double<n>>>>>>> " SB-APPEND
   then ;

: MEDIUM-SOURCE ( -- )
   MEDIUM-TYPE-SOURCE GUARDED-BUF ;

: MEDIUM-RC ( ptr u8 n -- n )
   MEDIUM-SOURCE SB-APPEND SB$ CHILD-RC ;

: MEDIUM-OVERFLOW ( ptr u8 n ptr u8 n -- ) {: defs:ptr defsu:n name:ptr nameu:n :}
   MEDIUM-SOURCE defs defsu SB-APPEND name nameu RATCHET-TAIL
   SB$ REFUSED-DATA ;

\ Seventeen 32-cell locals exceed the 4 KiB immediate offset while each
\ individual native value stays inside the elaborator's 64-cell vector.
\ Storing each local back makes every frame slot live until its use.
: X64-LOCAL-RELEASE-RC ( -- n )
   MEDIUM-SOURCE
   s" : MOVE ( -- ) " SB-APPEND
   17 0 do
      s" 0 SMALL @ {: " SB-APPEND
      s" abcdefghijklmnopq" drop i + 1 SB-APPEND
      s"  :} " SB-APPEND
   loop
   17 0 do
      s" abcdefghijklmnopq" drop i + 1 SB-APPEND
      s"  0 SMALL ! " SB-APPEND
   loop
   s" ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO" SB-APPEND
   SB$ CHILD-RC ;

: MEDIUM-TRANSFERS ( -- )
   s" cumulative local frame release exceeds imm12 without truncating" T-LABEL
   HB-TARGET-LINUX-X86-64? if
      X64-LOCAL-RELEASE-RC 0 T=
   else
      s" : MOVE ( -- ) 0 SMALL @ {: a :} 0 SMALL @ {: b :} 0 SMALL @ {: c :} 0 SMALL @ {: d :} 1 {: z:n :} a drop b drop c drop d drop z drop ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
      MEDIUM-RC 0 T=
   then
   HB-TARGET-LINUX-X86-64? if
      s" a 32-cell copy runs on a guarded stack"
   else
      s" a 128-cell copy runs on a guarded stack"
   then T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ dup drop drop ; : GO ( -- ) ['] MOVE BUF STACK-ABI:PAGE-BYTES run-in-stack ; GO"
   MEDIUM-RC 0 T=
   HB-TARGET-LINUX-X86-64? if
      s" a 32-cell copy past capacity faults the data guard page"
   else
      s" a 128-cell copy past the capacity faults the data guard page"
   then T-LABEL
   s" : MOVE ( -- ) 0 SMALL @ dup drop drop ; " s" MOVE" MEDIUM-OVERFLOW ;

public
: RUN-WIDE ( -- )
   T-RESET PRIMITIVE-GROUPS PAIR-TRANSFERS NATIVE-TRANSFERS WIDE-RETURN-GROUPS LARGE-TRANSFERS MEDIUM-TRANSFERS T-REPORT ;

;package

STACK-LIFECYCLE-TEST:RUN-WIDE
