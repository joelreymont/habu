\ tier0-string-literals.f - the JIT interns a string literal's body through NSTR
\ (src/habu/habu2.f C-SDQ, C-CQ, C-ESDQ, C-ECQ): once
\ src/compiler/native/string.f installs NSTR:INTERN in NSTR-ABI:INTERN-CELL, the
\ body lives in a DATA pool row NSTR:OWNER-ROW reports, equal bodies share one
\ address, and a failed definition's row stays. Spans come from the dictionary
\ record (src/habu/xref.f), as in test/compiler/tier0-join.f. ARM's tier 0 is
\ T0, so the file selects the JIT by zeroing NCOMP-DISPATCH:COMP-CELL.

require src/lib/test.f
require src/lib/string.f
require src/habu/xref.f
require src/habu/code-bytes.f
require src/habu/address-carrier.f
require src/compiler/native/string.f

0 data-base NCOMP-DISPATCH:COMP-CELL + !

package TIER0-STRLIT
private

\ Byte idx of the span is want.
: BYTE= ( ptr u8 n n -- bool ) {: a:ptr k:n want:n :}
   a k + c@ want = ;

public

\ The address is a row of an NSTR pool. Public for TEST-FAILED-LINE's text.
: OWNED? ( ptr u8 -- bool )
   NULL-PTR - NSTR:OWNER-ROW >r 2drop r> ;

: S-EMPTY ( -- ptr u8 n ) s" " ;
: C-EMPTY ( -- ptr u8 ) c" " ;
: S-ESC ( -- ptr u8 n ) s\" a\tb\x41\qc" ;
: C-ESC ( -- ptr u8 ) c\" a\nb" ;
: S-TWICE ( -- ptr u8 n ptr u8 n ) s" twin" s" twin" ;
: S-AGAIN ( -- ptr u8 n ) s" twin" ;
: C-TWICE ( -- ptr u8 ptr u8 ) c" pair" c" pair" ;

\ Interning lays nothing at `here`, so a `create` body around a definition
\ keeps its layout: the cell after SL-T's first is the 2.
create SL-T 1 ,
: SL-F ( -- ptr u8 n ) s" xyz" ;
2 ,

\ A wide fetch and a MATCH over the same family.
SUMTYPE sl-pair
  VARIANT sl-two n n ;VARIANT
  VARIANT sl-none ;VARIANT
;SUMTYPE
2 TYPED-BUFFER SL-PAIRS sl-pair
: SL-PUT ( -- ) 3 4 construct sl-pair sl-two 0 SL-PAIRS ! ;
: SL-GET ( -- sl-pair ) 0 SL-PAIRS @ ;
: SL-SUM ( sl-pair -- n ) MATCH sl-pair sl-two OF + ENDOF sl-none OF 0 ENDOF ;MATCH ;

private

: TEST-BYTES ( -- )
   s" every form pushes its body's bytes" T-LABEL
   S-EMPTY nip 0 T=
   C-EMPTY c@ 0 T=
   S-ESC 6 T=  {: e:ptr :}
   e 0 [char] a BYTE= TTRUE
   e 1 9 BYTE= TTRUE
   e 2 [char] b BYTE= TTRUE
   e 3 [char] A BYTE= TTRUE
   e 4 34 BYTE= TTRUE
   e 5 [char] c BYTE= TTRUE
   C-ESC {: c:ptr :}
   c c@ 3 T=
   c 1 [char] a BYTE= TTRUE
   c 2 10 BYTE= TTRUE
   c 3 [char] b BYTE= TTRUE
   S-AGAIN s" twin" STR= TTRUE
   C-TWICE drop count s" pair" STR= TTRUE
   SL-F s" xyz" STR= TTRUE ;

: TEST-OWNED ( -- )
   s" every body is an NSTR row" T-LABEL
   S-EMPTY drop OWNED? TTRUE
   C-EMPTY OWNED? TTRUE
   S-ESC drop OWNED? TTRUE
   C-ESC OWNED? TTRUE
   S-AGAIN drop OWNED? TTRUE
   C-TWICE drop OWNED? TTRUE ;

: TEST-SHARED ( -- )
   s" equal bodies push one address" T-LABEL
   S-TWICE drop {: b:ptr :} drop {: a:ptr :}
   a NULL-PTR - b NULL-PTR - T=
   S-AGAIN drop NULL-PTR - a NULL-PTR - T=
   C-TWICE NULL-PTR - swap NULL-PTR - T= ;

: TEST-LAYOUT ( -- )
   s" a definition with a literal lays nothing in a create body" T-LABEL
   SL-T cell+ @ 2 T= ;

\ The first definition interns the body, then fails at an undefined word; its
\ row stays, and the second definition finds it.
: TEST-FAILED-LINE ( -- )
   s" a failed definition's literal outlives the line" T-LABEL
   S\" : SL-BAD ( -- ptr u8 n ) s\" keep-me\" SL-NO-SUCH-WORD ;" TEST-EVAL:RC 0 T<>
   S\" : SL-KEEP ( -- ptr u8 n ) s\" keep-me\" ;" TEST-EVAL:RC 0 T=
   S\" SL-KEEP s\" keep-me\" STR=" TEST-EVAL:FLAG TTRUE
   s" SL-KEEP drop TIER0-STRLIT:OWNED?" TEST-EVAL:FLAG TTRUE ;

\ Interners compiled into the code region, as tier 0 compiles every word here;
\ the baked interner lies in the engine's text.
: SL-WRAP ( ptr u8 n -- n ) NSTR:INTERN ;
: SL-THROW ( ptr u8 n -- n ) 99 throw ;

: SL-INTERNER! ( [ ptr u8 n -- n ] -- )
   data-base NSTR-ABI:INTERN-CELL + xt! ;

\ A region-resident interner interns; one that throws fails its definition
\ with its code and the next definition compiles.
: TEST-REGION-INTERNER ( -- )
   s" a region-resident interner interns, and its throw fails the definition" T-LABEL
   ['] SL-WRAP SL-INTERNER!
   S\" : SL-VIA ( -- ptr u8 n ) s\" via-wrap\" ;" TEST-EVAL:RC 0 T=
   S\" SL-VIA s\" via-wrap\" STR=" TEST-EVAL:FLAG TTRUE
   s" SL-VIA drop TIER0-STRLIT:OWNED?" TEST-EVAL:FLAG TTRUE
   ['] SL-THROW SL-INTERNER!
   S\" : SL-THROWN ( -- ptr u8 n ) s\" thrown\" ;" TEST-EVAL:RC 99 T=
   NSTR:INSTALL
   S\" : SL-AFTER ( -- ptr u8 n ) s\" after\" ;" TEST-EVAL:RC 0 T=
   S\" SL-AFTER s\" after\" STR=" TEST-EVAL:FLAG TTRUE
   s" SL-AFTER drop TIER0-STRLIT:OWNED?" TEST-EVAL:FLAG TTRUE ;

\ Tier 0 builds two bodies rather than parsing them, a wide fetch's tag
\ descriptor (LP2VEMIT) and a MATCH's bad-tag message (C-DIE-BAD-TAG); their
\ addresses reach the code as a DATA address chain (src/habu/address-carrier.f).
\ The NSTR row a chain at p names, when p starts one and the row starts at the
\ chain's address.
: CHAIN-ROW ( ptr u8 ptr u8 -- ptr u8 n bool ) {: p:ptr end:ptr :}
   p end ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
   size ADDRESS-CARRIER:DATA-CHAIN-BYTES <> if NULL-PTR 0 false exit then
   p size ADDRESS-CARRIER:CHAIN-VALUE {: at:n :}
   at NSTR:OWNER-ROW 0= if 2drop NULL-PTR 0 false exit then
   {: body:ptr u:n :}
   body NULL-PTR - at <> if NULL-PTR 0 false exit then
   body u true ;

\ The NSTR row the first such chain in the named word's code names.
: CODE-ROW ( ptr u8 n -- ptr u8 n bool ) {: na:ptr nu:n :}
   na nu XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if NULL-PTR 0 false exit then
   rec XREF-START rec XREF-CODE-BYTES CODE-BYTES:AT {: base:ptr bytes:n :}
   base bytes + {: end:ptr :}
   bytes 0 ?do
      base i + end CHAIN-ROW if true unloop exit then 2drop
   4 +loop
   NULL-PTR 0 false ;

: TEST-DESCRIPTOR ( -- )
   s" a wide fetch's descriptor is an NSTR row outside the word's code" T-LABEL
   SL-PUT SL-GET SL-SUM 7 T=
   s" TIER0-STRLIT:SL-GET" CODE-ROW {: d:ptr du:n found:bool :}
   found TTRUE
   found if
      d 0 1 BYTE= TTRUE                         \ one CHECK row
   then ;

: TEST-BAD-TAG ( -- )
   s" a MATCH's bad-tag message is an NSTR row" T-LABEL
   s" TIER0-STRLIT:SL-SUM" CODE-ROW {: m:ptr mu:n found:bool :}
   found TTRUE
   found if
      m mu S\" hb: bad sl-pair tag\n" STR= TTRUE
   then ;

public

: RUN ( -- )
   T-RESET
   TEST-BYTES
   TEST-OWNED
   TEST-SHARED
   TEST-LAYOUT
   TEST-FAILED-LINE
   TEST-DESCRIPTOR
   TEST-BAD-TAG
   TEST-REGION-INTERNER
   T-REPORT ;

;package

TIER0-STRLIT:RUN
