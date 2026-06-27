\ c-call-emitter-test.f - source-shape regression for native C-CALL emitter.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/c-call-emitter-test.f

$20000 constant CCET-CAP

create CCET-BUF CCET-CAP allot
variable CCET-LEN

: CCET-SOURCE ( -- ptr u8 n )
   CCET-BUF CCET-LEN @ ;

: CCET-LOAD ( -- )
   s" src/habu/habu2.f" CCET-BUF CCET-CAP READ-ALL CCET-LEN ! ;

: CCET-HAS? ( ptr u8 n -- bool )
   CCET-SOURCE 2swap CONTAINS? ;

: CCET-MUST-HAVE ( ptr u8 n -- )
   CCET-HAS? TTRUE ;

: CCET-MUST-LACK ( ptr u8 n -- )
   CCET-HAS? 0= TTRUE ;

: CCET-COUNT ( ptr u8 n -- n ) {: needle:ptr needleu :}
   needleu 0= if 0 exit then
   CCET-LEN @ needleu < if 0 exit then
   0 0 begin dup CCET-LEN @ needleu - <= while
      CCET-BUF over + needleu needle needleu STR= if swap 1+ swap then
      1+
   repeat drop ;

: CCET-COUNT= ( ptr u8 n n -- ) {: needle:ptr needleu want :}
   needle needleu CCET-COUNT want T= ;

: CCET-TEST-HELPERS ( -- )
   s" : C-CALL-BRANCH-NO-PROLOGUE ( label -- )" CCET-MUST-HAVE
   s" : C-CALL-PROLOGUE-SPAN ( label -- )" CCET-MUST-HAVE
   s" : C-CALL-REQUIRE-RET-SLOT ( label -- )" CCET-MUST-HAVE
   s" : C-CALL-PLAIN-SPAN ( label -- )" CCET-MUST-HAVE
   s" : C-CALL-REJECT-MASKED ( n n label -- )" CCET-MUST-HAVE
   s" : C-CALL-REJECT-EXACT ( n label -- )" CCET-MUST-HAVE
   s" : C-CALL-REJECT-UNSAFE ( label -- )" CCET-MUST-HAVE
   s" : C-CALL-SCAN-SAFE ( label label label -- )" CCET-MUST-HAVE
   s" : C-CALL-COPY-INLINE ( label label -- )" CCET-MUST-HAVE
   s" : C-CALL-EMIT-ABSOLUTE ( -- )" CCET-MUST-HAVE ;

: CCET-TEST-HELPER-USES ( -- )
   s" C-CALL-BRANCH-NO-PROLOGUE" 2 CCET-COUNT=
   s" C-CALL-PROLOGUE-SPAN" 2 CCET-COUNT=
   s" C-CALL-PLAIN-SPAN" 2 CCET-COUNT=
   s" C-CALL-REJECT-UNSAFE" 2 CCET-COUNT=
   s" C-CALL-SCAN-SAFE" 2 CCET-COUNT=
   s" C-CALL-COPY-INLINE" 2 CCET-COUNT=
   s" C-CALL-EMIT-ABSOLUTE" 2 CCET-COUNT= ;

: CCET-TEST-CALL-BODY ( -- )
   s" lnopro C-CALL-BRANCH-NO-PROLOGUE" CCET-MUST-HAVE
   s" lcall C-CALL-PROLOGUE-SPAN" CCET-MUST-HAVE
   s" lcall C-CALL-PLAIN-SPAN" CCET-MUST-HAVE
   s" lcopy lcall lsbody C-CALL-SCAN-SAFE" CCET-MUST-HAVE
   s" linl ldone C-CALL-COPY-INLINE" CCET-MUST-HAVE
   s" C-CALL-EMIT-ABSOLUTE" 2 CCET-COUNT= ;

: CCET-TEST-REMOVED-DUPLICATION ( -- )
   s" 8 $FC000000 LIT64,  10 9 8 AND,  8 $94000000 LIT64" CCET-MUST-LACK
   s" 8 $D65F03C0 LIT64,  9 8 CMP,  C-NE lcall BCOND" CCET-MUST-LACK
   s" 7 11 16 LSRI,  7 7 5 AND,   7 7 5 LSLI,  8 $F2A00010 LIT64" CCET-MUST-LACK ;

: CCET-MAIN ( -- )
   T-RESET
   CCET-LOAD
   CCET-TEST-HELPERS
   CCET-TEST-HELPER-USES
   CCET-TEST-CALL-BODY
   CCET-TEST-REMOVED-DUPLICATION
   T-REPORT
   s" c-call-emitter-test: ok" type cr ;

CCET-MAIN
