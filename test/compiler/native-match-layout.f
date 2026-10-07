\ Recorded layout facts and their shared scratch lifetime.

require lib/test.f
require lib/test/mapped.f
require src/compiler/native/checker-owner.f

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

\ A live record's code address takes its slot's quotation type.
CAST: >ACTION ( n -- [ -- ] )
CAST: >STORE ( n -- [ n -- ] )
CAST: >ROWS ( n -- [ -- ptr ptr n ] )
CAST: >CAP ( n -- [ -- ptr n ] )

: BIND ( -- )
   s" REC-RESET" INTERNAL-XT >ACTION is START-RECORD
   s" REC-OFF" INTERNAL-XT >ACTION is STOP-RECORD
   s" REC-RELEASE" INTERNAL-XT >ACTION is RELEASE-RECORD
   s" MWIN-CELLS!" INTERNAL-XT >STORE is LAYOUT!
   s" REC-COMMIT" INTERNAL-XT >ACTION is COMMIT-ROW
   s" REC-STEP" INTERNAL-XT >ACTION is NEXT-ROW
   s" CALL-FINALIZE" INTERNAL-XT >ACTION is FINALIZE
   s" CWIN-P" INTERNAL-XT >ROWS is ROWS-PTR
   s" CWIN-CAP" INTERNAL-XT >CAP is ROWS-CAP ;
BIND

: ROW! ( n -- )
   LAYOUT! COMMIT-ROW NEXT-ROW ;

: ABSENT2 ( n n -- )
   -1 T= -1 T= ;

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
   GROWTH-CASE
   KIND-CASE
   REUSE-CASE
   RELEASE-CASE ;

;package
