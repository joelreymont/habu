\ Recorded layout facts: the real opcode enum and the shared scratch lifetime.

require test/compiler/native-eval-fixture.f
require lib/test/src-shape.f
require lib/test/mapped.f
require src/compiler/native/checker-owner.f

package NMX-LAYOUT
private

: OFFSET ( ptr u8 n ptr u8 n -- n )
   FIND-SUB MATCH option
      none OF s" native-match: opcode declaration missing" 76 die ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ Compile the actual compiler declaration in a fresh package. Requiring a64ir.f
\ would reuse the engine's already compiled copy and never exercise recording.
: LOAD-OPCODE ( -- )
   s" src/compiler/native/a64ir.f" SHAPE:LOAD
   SHAPE:TEXT {: a:ptr u:n :}
   a u s" ENUM opcode DERIVE eq" OFFSET {: start:n :}
   a start + u start - s" ;ENUM" OFFSET 5 + {: count:n :}
   SB-RESET
   s\" package NMXC public\n" SB-APPEND
   a start + count SB-APPEND
   s\" \n;package\n" SB-APPEND
   SB$ NATIVE-EVAL:DEFINE-RC dup 0 <> if throw then drop ;

' LOAD-OPCODE
;package
execute

package NMX-LAYOUT
private

\ These pre-hook checker words have no native call model. Bind their actual
\ effects only inside this test, through private typed slots. Read their live
\ records directly; ordinary tick still refuses internal execution tokens.
\ The recorded numbers remain visible through the real owner readers.
defer START-RECORD ( -- )
defer STOP-RECORD ( -- )
defer RELEASE-RECORD ( -- )
defer LAYOUT! ( n -- )
defer COMMIT-ROW ( -- )
defer NEXT-ROW ( -- )
defer FINALIZE ( -- )
defer ROWS-PTR ( -- ptr ptr n )
defer ROWS-CAP ( -- ptr n )

: INTERNAL-XT ( ptr u8 n -- n )
   XREF-FIND
   dup XREF-FOUND? 0= if drop s" native-match: checker word missing" 76 die then
   dup XREF-RETIRED? if drop s" native-match: checker word retired" 76 die then
   XREF-START dup 0= if drop s" native-match: checker word has no code" 76 die then ;

TRUSTED: BIND ( -- )
   s" REC-RESET" INTERNAL-XT is START-RECORD
   s" REC-OFF" INTERNAL-XT is STOP-RECORD
   s" REC-RELEASE" INTERNAL-XT is RELEASE-RECORD
   s" MWIN-CELLS!" INTERNAL-XT is LAYOUT!
   s" REC-COMMIT" INTERNAL-XT is COMMIT-ROW
   s" REC-STEP" INTERNAL-XT is NEXT-ROW
   s" CALL-FINALIZE" INTERNAL-XT is FINALIZE
   s" CWIN-P" INTERNAL-XT is ROWS-PTR
   s" CWIN-CAP" INTERNAL-XT is ROWS-CAP ;
BIND

: ROW! ( n -- )
   LAYOUT! COMMIT-ROW NEXT-ROW ;

: ABSENT2 ( n n -- )
   -1 T= -1 T= ;

: OPCODE-CASE ( -- )
   s" the compiler's 76-arm opcode declaration compiles and executes" T-LABEL
   NMXC-OPCODE:MOVZ NMXC-OPCODE:TAG 0 T=
   NMXC-OPCODE:STORE NMXC-OPCODE:TAG 13 T=
   NMXC-OPCODE:FCMPBR NMXC-OPCODE:TAG 56 T=
   NMXC-OPCODE:CODEADDR NMXC-OPCODE:TAG 75 T=
   NMXC-OPCODE:CODEADDR NMXC-OPCODE:CODEADDR NMXC-OPCODE:EQ TTRUE
   NMXC-OPCODE:MOVZ NMXC-OPCODE:CODEADDR NMXC-OPCODE:EQ TFALSE ;

: GROWTH-CASE ( -- )
   s" growing recorded facts retains rows and releases the previous mapping" T-LABEL
   RELEASE-RECORD START-RECORD
   100 ROW!
   ROWS-CAP @ {: cap:n :}
   cap 1 ?do i 100 + ROW! loop
   ROWS-PTR @ BYTE-VIEW {: prior:ptr :}
   prior MAPPED:LIVE? TTRUE
   cap 100 + ROW!
   prior MAPPED:LIVE? TFALSE
   ROWS-CAP @ cap > TTRUE
   FINALIZE
   cap 1 + 0 ?do i CHECKER-OWNER:MATCH-CELLS i 100 + T= loop
   cap 1 + CHECKER-OWNER:MATCH-CELLS -1 T= ;

: KIND-CASE ( -- )
   s" layout facts cannot answer call, glue, payload or quotation queries" T-LABEL
   0 CHECKER-OWNER:CALL-CELLS ABSENT2
   0 CHECKER-OWNER:CALL-GLUE ABSENT2
   0 CHECKER-OWNER:MATCH-PAYLOAD ABSENT2
   0 0 CHECKER-OWNER:CALL-QUOT-IN ABSENT2
   0 0 CHECKER-OWNER:CALL-QUOT-OUT ABSENT2
   0 CHECKER-OWNER:CATCH-CELLS ABSENT2
   0 CHECKER-OWNER:EXEC-CELLS ABSENT2
   0 CHECKER-OWNER:FINALLY-CELLS -1 T= ABSENT2 ;

: REUSE-CASE ( -- )
   s" a new definition reuses capacity and clears rows and pending facts" T-LABEL
   ROWS-PTR @ {: prior:ptr :}
   ROWS-CAP @ {: cap:n :}
   777 LAYOUT!
   START-RECORD
   ROWS-PTR @ prior = TTRUE
   ROWS-CAP @ cap T=
   COMMIT-ROW
   0 CHECKER-OWNER:MATCH-CELLS -1 T=
   3 ROW!
   STOP-RECORD
   999 LAYOUT! COMMIT-ROW
   0 CHECKER-OWNER:MATCH-CELLS 3 T=
   1 CHECKER-OWNER:MATCH-CELLS -1 T= ;

: RELEASE-CASE ( -- )
   s" capture cleanup releases the retained mapping and recording can resume" T-LABEL
   ROWS-PTR @ BYTE-VIEW {: prior:ptr :}
   prior MAPPED:LIVE? TTRUE
   RELEASE-RECORD
   prior MAPPED:LIVE? TFALSE
   ROWS-PTR @ 0= TTRUE
   ROWS-CAP @ 0 T=
   0 CHECKER-OWNER:MATCH-CELLS -1 T=
   RELEASE-RECORD
   START-RECORD 7 ROW! STOP-RECORD
   0 CHECKER-OWNER:MATCH-CELLS 7 T=
   RELEASE-RECORD ;

public

: TEST ( -- )
   OPCODE-CASE
   GROWTH-CASE
   KIND-CASE
   REUSE-CASE
   RELEASE-CASE ;

;package
