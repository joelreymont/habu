\ Reader-to-tape unit tests. CHECK! drives the real checker reader over explicit
\ reconstructed definition text. Normal compilation owns its own feed unit;
\ the ordinary compiled fixtures below also exercise source reconstruction.
\ Tier-neutral by design: the reader is driven directly over explicit text, so
\ the rows asserted are the same at either tier.
require lib/test.f
require src/compiler/native/feed.f

package NFEED-FIXTURE
public
defer HOOK ( n -- n )
: IMPL ( n -- n ) 1 + ;
: TEXT ( -- ptr u8 n ) s" a  b ; : ( x" ;
: ESCAPED ( -- ptr u8 n ) S\" a\tb\x41\qc" ;
: COMMENTS ( n -- n ) ( zdup 77 )
   dup  \ ignored by source reconstruction
   * 3 + ;
;package

package NFEED-TEST
private
CAST: REAL-CELL ( r -- n )

2 constant UNITS
UNITS TYPED-BUFFER T-KEY IR-ID:ir-module-key
UNITS TYPED-BUFFER T-MOD IR-BUILD:module
UNITS TYPED-BUFFER T-TAPE IR-ARENA:view
create T-VERDICT UNITS cells allot
256 constant TEXT-CAP
create UTXT UNITS TEXT-CAP * allot
: TEXT-BUF ( n -- ptr u8 ) TEXT-CAP * UTXT + ;

: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: KEY@ ( n -- IR-ID:ir-module-key )  T-KEY @ ;
: MOD@ ( n -- IR-BUILD:module )      T-MOD @ ;
: TAPE@ ( n -- IR-ARENA:view )       T-TAPE @ ;
: VERDICT@ ( n -- n )                cells T-VERDICT + @ ;
: SRC@ ( n -- IR-ARENA:view )        MOD@ IR-BUILD:FSOURCES ;

: SRC-ID ( n -- IR-ID:ir-source-id )
   {: slot:n :}
   slot TAPE@ slot KEY@ 0 NTAPE:SPAN@ IR-SOURCE:SPAN-SRC ;

: NEW-BLD ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c s" habu" 1 0 IR-BUILD:NEW-BUILDER ;

: TOKENS ( n -- n )
   TAPE@ NTAPE:TOKENS ;

: SPELL ( n n -- IR-ID:ir-symbol-id )
   {: slot:n i:n :}
   slot TAPE@ slot KEY@ i NTAPE:SPELL@ ;

: SPELL-IS? ( n n ptr u8 n -- bool )
   {: slot:n i:n a:ptr u:n :}
   slot MOD@ IR-BUILD:FSYM-POOL  slot MOD@ IR-BUILD:FSYM-ROWS
   slot i SPELL  a u IR-SYM:FEQ? ;

: ANY-SPELL? ( n ptr u8 n -- bool )
   {: slot:n a:ptr u:n :}
   false
   slot TOKENS 0 ?do
      slot i a u SPELL-IS? or
   loop ;

: KIND-IS? ( n n NTAPE:kind -- bool )
   {: slot:n i:n k:NTAPE:kind :}
   slot TAPE@ i NTAPE:KIND@ k NTAPE-KIND:EQ ;

: MODE-IS? ( n n NTAPE:mode -- bool )
   {: slot:n i:n m:NTAPE:mode :}
   slot TAPE@ i NTAPE:MODE@ m NTAPE-MODE:EQ ;

: SPAN-START ( n n -- n )
   {: slot:n i:n :}
   slot TAPE@ slot KEY@ i NTAPE:SPAN@ IR-SOURCE:SPAN-START ;

: SPAN-LEN ( n n -- n )
   {: slot:n i:n :}
   slot TAPE@ slot KEY@ i NTAPE:SPAN@ IR-SOURCE:SPAN-LEN ;

: LIT ( n n -- n )
   {: slot:n i:n :}
   slot TAPE@ i NTAPE:LIT@ ;

: LOCAL-SPELL ( n n -- n )
   SPELL IR-ID:SYMBOL-LOCAL ;

: SRC-LEN ( n -- n )
   {: slot:n :}
   slot SRC@ slot SRC-ID IR-SOURCE:FLEN@ ;

: SRC-DIGEST ( n -- CDIGEST:digest )
   {: slot:n :}
   slot SRC@ slot SRC-ID IR-SOURCE:FDIGEST@ ;

: SAME-TAPE? ( -- bool )
   0 TAPE@ NTAPE:DIGEST  1 TAPE@ NTAPE:DIGEST  CDIGEST-DIGEST:EQ ;

: SAME-TEXT? ( -- bool )
   0 SRC-DIGEST  1 SRC-DIGEST  CDIGEST-DIGEST:EQ ;

: REC ( IR-CTX:ctx ptr u8 n n -- )
   {: c:IR-CTX:ctx a:ptr u:n slot:n :}
   c NEW-BLD {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY slot T-KEY !
   c b IR-BUILD:MODULE-KEY 32 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c b tp slot TEXT-BUF TEXT-CAP NFEED:BEGIN-UNIT
   a u CHECK! {: verdict:n :}
   NFEED:END-UNIT {: view:IR-ARENA:view recorded:n :}
   recorded verdict T=
   recorded slot cells T-VERDICT + ! view slot T-TAPE !
   c b IR-BUILD:FREEZE slot T-MOD ! ;

: GRID$ ( -- ptr u8 n ) s" NF-GRID ( n -- n ) dup * 3 +" ;
: GRID ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c GRID$ 0 REC
   0 VERDICT@ -1 T= 0 TOKENS 5 T=
   0 0 s" NF-GRID" SPELL-IS? TTRUE
   0 1 s" dup" SPELL-IS? TTRUE 0 2 s" *" SPELL-IS? TTRUE
   0 3 s" 3" SPELL-IS? TTRUE 0 4 s" +" SPELL-IS? TTRUE
   0 0 NTAPE-KIND:NAME KIND-IS? TTRUE
   0 3 NTAPE-KIND:INT-LITERAL KIND-IS? TTRUE
   0 0 NTAPE-MODE:INTERPRETING MODE-IS? TTRUE
   0 3 NTAPE-MODE:COMPILING MODE-IS? TTRUE
   0 0 SPAN-START 0 T= 0 0 SPAN-LEN 7 T=
   GRID$ drop 0 3 SPAN-START + 0 3 SPAN-LEN s" 3" T$=
   0 3 LIT 3 T= ;

: HEX ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-HEX ( -- n ) -$FF" 0 REC 0 1 LIT -255 T=
   c s" NF-HEX-BITS ( -- n ) $FFFFFFFFFFFFFFFF" 0 REC 0 1 LIT -1 T= ;

: REALS ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-REAL ( -- r ) 1.9482199351819093" 0 REC
   0 1 NTAPE-KIND:REAL-LITERAL KIND-IS? TTRUE
   0 1 LIT 1.9482199351819093 REAL-CELL T= ;

: ZEROS ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-NZERO ( -- r ) -0.0" 0 REC 0 1 LIT -0.0 REAL-CELL T=
   c s" NF-PZERO ( -- r ) 0.0" 0 REC 0 1 LIT 0 T= ;

: CHARACTER ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-DOT ( -- r ) .5" 0 REC 0 1 LIT .5 REAL-CELL T=
   c s" NF-CHAR ( -- n ) [char] Z" 0 REC
   0 TOKENS 2 T= 0 1 NTAPE-KIND:CHAR-LITERAL KIND-IS? TTRUE
   0 1 LIT 90 T= ;

: STRINGS ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c S\" NF-HIDES ( -- n ) s\" zdup z77\" 2drop 5" 0 REC
   0 TOKENS 4 T=
   0 s" zdup" ANY-SPELL? TFALSE 0 s" z77" ANY-SPELL? TFALSE
   0 1 NTAPE-KIND:STRING-LITERAL KIND-IS? TTRUE
   0 1 s" zdup z77" SPELL-IS? TTRUE
   c S\" NF-VERB ( -- n ) s\" a  b ; : ( x\" 2drop 5" 0 REC
   0 TOKENS 4 T= 0 1 s" a  b ; : ( x" SPELL-IS? TTRUE ;

: ESCAPES ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c S\" NF-ESC ( -- n ) s\\\" a\\tb\\x41\\qc\" 2drop 5" 0 REC
   0 TOKENS 4 T= 0 1 SPAN-LEN 11 T=
   0 1 S\" a\tbA\qc" SPELL-IS? TTRUE
   c S\" NF-EMPTY ( -- n ) s\" \" 2drop 5" 0 REC
   0 TOKENS 4 T= 0 1 s" " SPELL-IS? TTRUE
   0 1 NTAPE-KIND:STRING-LITERAL KIND-IS? TTRUE ;

using NTAPE-KIND

: COUNTED-FORMS ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c S\" NF-COUNTED ( -- ptr u8 ) C\q body\q" 0 REC
   0 VERDICT@ -1 T= 0 TOKENS 2 T=
   0 1 COUNTED-STRING-LITERAL KIND-IS? TTRUE
   0 1 s" body" SPELL-IS? TTRUE ;


: PRINTED-FORMS ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c S\" NF-PRINTED ( -- ) .\q body\q" 0 REC
   0 VERDICT@ -1 T= 0 TOKENS 2 T=
   0 1 PRINTED-STRING-LITERAL KIND-IS? TTRUE
   0 1 s" body" SPELL-IS? TTRUE ;


: ESCAPED-FORMS ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c S\" NF-COUNTED-ESC ( -- ptr u8 ) c\\\q a\\tb\q" 0 REC
   0 1 COUNTED-STRING-LITERAL KIND-IS? TTRUE
   0 1 S\" a\tb" SPELL-IS? TTRUE
   c S\" NF-PRINTED-ESC ( -- ) .\\\q a\\tb\q" 0 REC
   0 1 PRINTED-STRING-LITERAL KIND-IS? TTRUE
   0 1 S\" a\tb" SPELL-IS? TTRUE ;

;using

: REPEATED ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-TWICE ( n -- n ) dup + dup +" 0 REC
   0 TOKENS 5 T=
   0 1 LOCAL-SPELL 0 3 LOCAL-SPELL T=
   0 1 SPAN-START 0 3 SPAN-START T<>
   c S\" NF-SAME ( -- n ) s\" ab\" 2drop s\" ab\" 2drop 5" 0 REC
   0 TOKENS 6 T=
   0 1 LOCAL-SPELL 0 3 LOCAL-SPELL T=
   0 1 SPAN-START 0 3 SPAN-START T<> ;

: NAME-DIGEST ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-NMA ( -- n ) 1" 0 REC c s" NF-NMB ( -- n ) 1" 1 REC
   SAME-TAPE? TTRUE SAME-TEXT? TFALSE ;

: COMMENT-DIGEST ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-CMA ( -- n ) ( aa ) 1" 0 REC
   c s" NF-CMB ( -- n ) ( ab ) 1" 1 REC
   0 TOKENS 2 T= 1 TOKENS 2 T=
   0 s" aa" ANY-SPELL? TFALSE 1 s" ab" ANY-SPELL? TFALSE
   SAME-TAPE? TTRUE SAME-TEXT? TFALSE ;

: LITERAL-DIGEST ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-LIA ( -- n ) 1" 0 REC c s" NF-LIB ( -- n ) 2" 1 REC
   SAME-TAPE? TFALSE ;

: SHAPE-DIGEST ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-SHA ( -- n ) 1" 0 REC c s" NF-SHB ( -- n ) 1 1 +" 1 REC
   0 TOKENS 2 T= 1 TOKENS 4 T= SAME-TAPE? TFALSE ;

: KEPT$ ( -- ptr u8 n ) s" NF-KEPT ( n -- n ) dup * 3 +" ;
create INPUT TEXT-CAP allot
: KEPT ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   KEPT$ INPUT swap BYTE-COPY
   c INPUT KEPT$ nip 0 REC
   TEXT-CAP 0 ?do 0 INPUT i + c! loop
   c s" NF-AFTER ( -- n ) 9" 1 REC
   0 TEXT-BUF 0 SRC-LEN KEPT$ T$=
   0 TEXT-BUF 0 SRC-LEN CDIGEST:COMPUTE 0 SRC-DIGEST CDIGEST-DIGEST:EQ TTRUE ;

\ The reader also reports names consumed by the `is` judgment itself.
: CONSUMED-NAMES ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c s" NF-SWALLOW ( -- ) [: NFEED-FIXTURE:IMPL ;] is NFEED-FIXTURE:HOOK" 0 REC
   0 TOKENS 6 T= 0 4 s" is" SPELL-IS? TTRUE
   0 5 s" NFEED-FIXTURE:HOOK" SPELL-IS? TTRUE
   0 5 NTAPE-KIND:NAME KIND-IS? TTRUE
   0 5 NTAPE-MODE:COMPILING MODE-IS? TTRUE
   c S\" NF-SWHID ( -- ) ( is NFEED-FIXTURE:HOOK ) s\" is\" 2drop [: NFEED-FIXTURE:IMPL ;] is NFEED-FIXTURE:HOOK" 0 REC
   0 TOKENS 8 T= 0 1 NTAPE-KIND:STRING-LITERAL KIND-IS? TTRUE
   0 1 s" is" SPELL-IS? TTRUE 0 2 s" 2drop" SPELL-IS? TTRUE
   0 6 s" is" SPELL-IS? TTRUE 0 7 s" NFEED-FIXTURE:HOOK" SPELL-IS? TTRUE ;

1 TYPED-BUFFER U-CTX IR-CTX:ctx
1 TYPED-BUFFER U-BLD IR-BUILD:builder
1 TYPED-BUFFER U-TP IR-ARENA:arena
: OPEN-SCRATCH ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c NEW-BLD {: b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY 32 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c 0 U-CTX ! b 0 U-BLD ! tp 0 U-TP !
   c b tp UTXT TEXT-CAP NFEED:BEGIN-UNIT ;
: REOPEN-SCRATCH ( -- )
   0 U-CTX @ 0 U-BLD @ 0 U-TP @ UTXT TEXT-CAP NFEED:BEGIN-UNIT ;
: END-QUIET ( -- ) NFEED:END-UNIT 2drop ;
: CHECK-ONE ( -- ) s" NF-ONE ( -- n ) 1" CHECK! drop ;
: STATES ( IR-CTX:ctx -- )
   OPEN-SCRATCH
   [: END-QUIET ;] E-NFEED-STATE TTHROWSQ
   [: REOPEN-SCRATCH ;] E-NFEED-STATE TTHROWSQ
   CHECK-ONE [: CHECK-ONE ;] E-NFEED-SCAN TTHROWSQ
   NFEED:ABANDON-UNIT ;
: OWNER ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c NEW-BLD {: b:IR-BUILD:builder :}
   c NEW-BLD {: other:IR-BUILD:builder :}
   c other IR-BUILD:MODULE-KEY 32 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c b tp UTXT TEXT-CAP NFEED:BEGIN-UNIT
   [: CHECK-ONE ;] E-NTAPE-OWNER TTHROWSQ
   NFEED:ABANDON-UNIT ;
: CAPACITY ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c NEW-BLD {: b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY 32 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c b tp UTXT 8 NFEED:BEGIN-UNIT
   [: CHECK-ONE ;] E-NFEED-TEXT TTHROWSQ
   NFEED:ABANDON-UNIT
   c s" NF-RECOVER ( -- n ) 6" 0 REC
   0 TOKENS 2 T= 0 VERDICT@ -1 T= 0 1 LIT 6 T= ;

: PARSED? ( ptr u8 n -- bool )
   num-parse {: v:n flt:bool ok:bool :} ok ;

: PARSED-FLOAT? ( ptr u8 n -- bool )
   num-parse {: v:n flt:bool ok:bool :} flt ;

: PARSED-VALUE ( ptr u8 n -- n )
   num-parse {: v:n flt:bool ok:bool :} v ;

: DECLINE-CASE ( -- )
   s" the engine's reader declines what the engine declines" T-LABEL
   s" 5." PARSED? TFALSE
   s" 1.2.3" PARSED? TFALSE
   s" 1.5e3" PARSED? TFALSE
   s" 12a" PARSED? TFALSE
   s" $" PARSED? TFALSE
   s" -" PARSED? TFALSE
   s" " PARSED? TFALSE
   s" 0.0000000000000000000" PARSED? TFALSE
   s" -0.0085031157383406233" PARSED? TFALSE
   s" 9223372036854775808.0" PARSED? TFALSE
   s" -9223372036854775808.0" PARSED? TFALSE
   s" 18446744073709551617" PARSED? TFALSE
   s" a declined spelling answers no value and no float flag" T-LABEL
   s" 12a" PARSED-VALUE 0 T=
   s" 12a" PARSED-FLOAT? TFALSE
   s" 5." PARSED-VALUE 0 T=
   s" 5." PARSED-FLOAT? TFALSE
   s" 0.0000000000000000000" PARSED-VALUE 0 T=
   s" 0.0000000000000000000" PARSED-FLOAT? TFALSE ;

public
: RUN ( -- )
   T-RESET
   NFEED-FIXTURE:TEXT s" a  b ; : ( x" T$=
   NFEED-FIXTURE:ESCAPED S\" a\tbA\qc" T$=
   4 NFEED-FIXTURE:COMMENTS 19 T=
   DECLINE-CASE
   BND [: GRID ;] IR-CTX:WITH-CONTEXT
   BND [: HEX ;] IR-CTX:WITH-CONTEXT
   BND [: REALS ;] IR-CTX:WITH-CONTEXT
   BND [: ZEROS ;] IR-CTX:WITH-CONTEXT
   BND [: CHARACTER ;] IR-CTX:WITH-CONTEXT
   BND [: STRINGS ;] IR-CTX:WITH-CONTEXT
   BND [: COUNTED-FORMS ;] IR-CTX:WITH-CONTEXT
   BND [: PRINTED-FORMS ;] IR-CTX:WITH-CONTEXT
   BND [: ESCAPED-FORMS ;] IR-CTX:WITH-CONTEXT
   BND [: ESCAPES ;] IR-CTX:WITH-CONTEXT
   BND [: REPEATED ;] IR-CTX:WITH-CONTEXT
   BND [: NAME-DIGEST ;] IR-CTX:WITH-CONTEXT
   BND [: COMMENT-DIGEST ;] IR-CTX:WITH-CONTEXT
   BND [: LITERAL-DIGEST ;] IR-CTX:WITH-CONTEXT
   BND [: SHAPE-DIGEST ;] IR-CTX:WITH-CONTEXT
   BND [: KEPT ;] IR-CTX:WITH-CONTEXT
   BND [: CONSUMED-NAMES ;] IR-CTX:WITH-CONTEXT
   BND [: STATES ;] IR-CTX:WITH-CONTEXT
   BND [: OWNER ;] IR-CTX:WITH-CONTEXT
   BND [: CAPACITY ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;
;package
NFEED-TEST:RUN
