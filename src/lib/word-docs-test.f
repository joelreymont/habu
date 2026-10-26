\ word-docs-test.f - package WORD-DOCS through its public API: an inline fixture
\ for each recording rule, the refusals, storage beyond one chunk and one index
\ capacity, and a real source file.
\ Run: bin/hb --load src/lib/word-docs-test.f

require src/lib/string.f
require src/lib/memory.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/test.f
require src/lib/word-docs.f

package WORD-DOCS-TEST
private

$40000 constant FIX-CAP          \ room for the largest generated text
1500 constant MANY               \ past one chunk of records and the first index capacity
1200 constant LONG-LINES         \ one comment longer than a chunk
58 constant LONG-WIDTH           \ bytes of each of its lines after `\ `
120 constant X-BYTE              \ `x`, which fills them

TYPED-VARIABLE FIX ptr u8
variable FIX-U

: FIX@ ( -- ptr u8 )
   FIX @ ;

: FIX$ ( -- ptr u8 n )
   FIX@ FIX-U @ ;

: FIX-OPEN ( -- )
   FIX-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop FIX !
   0 FIX-U ! ;

: FIX-CLOSE ( -- )
   FIX@ FIX-CAP MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: +FIX ( ptr u8 n -- ) {: a:ptr u:n :}
   FIX-U @ u + FIX-CAP > if E-STR-CAPACITY throw then
   a FIX@ FIX-U @ + u BYTE-COPY
   FIX-U @ u + FIX-U ! ;

: +SB ( -- )
   SB$ +FIX ;

: DOCS ( ptr u8 n -- )
   WORD-DOCS:RESET WORD-DOCS:FILE ;

\ Record k is named a u, in package p pu, with effect e eu.
: REC= ( n ptr u8 n ptr u8 n ptr u8 n -- ) {: k:n a:ptr u:n p:ptr pu:n e:ptr eu:n :}
   k WORD-DOCS:NAME$ a u T$=
   k WORD-DOCS:PACKAGE$ p pu T$=
   k WORD-DOCS:EFFECT$ e eu T$= ;

\ ---- the recording rules -------------------------------------------------------

: RULES-FIXTURE ( -- )
   0 FIX-U !
   s\" \\ Stray, cut off by the blank line below.\n\n" +FIX
   s\" \\ Top note.\n: TOP ( a -- b ) ;\n" +FIX
   s\" package ALPHA\n\\ First line.\n\\\n\\    indented.\n: HIDDEN ( x -- ) drop ;\n" +FIX
   s\" public\n\\ Detached by the blank line.\n\n: SHOWN ( -- n ) 1 ;\n" +FIX
   s\" : BARE 2 ;\n: LATE\n   ( -- n ) 3 ;\n: NOTED \\ beside\n   ( n -- n ) 1+ ;\n" +FIX
   s\" : SKIPPED\n\\ between\n   ( n -- n ) 1+ ;\n: SPACED\n\n   ( -- n ) 4 ;\n" +FIX
   s\" variable V ( note )\n;package\n" +FIX
   s\" package ALPHA\n: AGAIN ( -- ) ;\npublic\n: OPEN ( -- ) ;\n;package\n" +FIX
   s\" package BETA public\n\\ Re-exported.\nEXPORT ALPHA:SHOWN\n;package\n" +FIX
   s\" EXPORT ALPHA:OPEN\ns\" one\n\\ two\"\n: AFTER ( -- ) ;\n" +FIX ;

: RULES ( -- )
   s" rules" T-LABEL
   RULES-FIXTURE FIX$ DOCS
   WORD-DOCS:COUNT 12 T=
   0 s" TOP" s" " s" a -- b" REC=
   0 WORD-DOCS:COMMENT$ s" Top note." T$=
   0 WORD-DOCS:PUBLIC? TFALSE
   0 WORD-DOCS:EXPORT? TFALSE
   1 s" HIDDEN" s" ALPHA" s" x --" REC=
   1 WORD-DOCS:COMMENT$ s\" First line.\n\n   indented." T$=
   1 WORD-DOCS:PUBLIC? TFALSE
   2 s" SHOWN" s" ALPHA" s" -- n" REC=
   2 WORD-DOCS:COMMENT$ nip 0 T=
   2 WORD-DOCS:PUBLIC? TTRUE
   3 s" BARE" s" ALPHA" s" " REC=
   3 WORD-DOCS:PUBLIC? TTRUE
   4 s" LATE" s" ALPHA" s" -- n" REC=
   \ A `\` comment between the name and `( ... )` leaves no declared effect,
   \ beside the name or on a line of its own; a blank line does not.
   5 s" NOTED" s" ALPHA" s" " REC=
   5 WORD-DOCS:COMMENT$ nip 0 T=
   6 s" SKIPPED" s" ALPHA" s" " REC=
   7 s" SPACED" s" ALPHA" s" -- n" REC=
   8 s" AGAIN" s" ALPHA" s" --" REC=
   8 WORD-DOCS:PUBLIC? TFALSE
   9 s" OPEN" s" ALPHA" s" --" REC=
   9 WORD-DOCS:PUBLIC? TTRUE
   10 s" ALPHA:SHOWN" s" BETA" s" " REC=
   10 WORD-DOCS:COMMENT$ s" Re-exported." T$=
   10 WORD-DOCS:PUBLIC? TTRUE
   10 WORD-DOCS:EXPORT? TTRUE
   11 s" AFTER" s" " s" --" REC=
   11 WORD-DOCS:COMMENT$ nip 0 T=
   11 WORD-DOCS:EXPORT? TFALSE
   [: 12 WORD-DOCS:NAME$ 2drop ;] E-VEC-BOUNDS TTHROWSQ ;

\ A run whose first lines are bare `\` keeps a newline for each.
: BARE-NOTES ( -- )
   s" bare notes" T-LABEL
   s\" \\\n\\ x\n: Z1 ( -- ) ;\n\\\n\\\n\\ y\n: Z2 ( -- ) ;\n" DOCS
   WORD-DOCS:COUNT 2 T=
   0 WORD-DOCS:COMMENT$ s\" \nx" T$=
   1 WORD-DOCS:COMMENT$ s\" \n\ny" T$=
   WORD-DOCS:RESET ;

\ Successive texts append; RESET drops them all.
: APPEND ( -- )
   s" append" T-LABEL
   s" : ONE ( -- ) ;" DOCS
   s" package GAMMA public : TWO ( -- ) ; ;package" WORD-DOCS:FILE
   WORD-DOCS:COUNT 2 T=
   1 s" TWO" s" GAMMA" s" --" REC=
   WORD-DOCS:RESET
   WORD-DOCS:COUNT 0 T=
   [: 0 WORD-DOCS:NAME$ 2drop ;] E-VEC-STATE TTHROWSQ ;

: REFUSALS ( -- )
   s" refusals" T-LABEL
   [: s" : " DOCS ;] WORD-DOCS:E-SOURCE TTHROWSQ
   [: s" package" DOCS ;] WORD-DOCS:E-SOURCE TTHROWSQ
   [: s" package P public EXPORT" DOCS ;] WORD-DOCS:E-SOURCE TTHROWSQ
   [: s\" s\" open" DOCS ;] WORD-DOCS:E-SOURCE TTHROWSQ
   \ The records read before the refusal stay until RESET.
   [: s" : KEPT ( -- ) ; :" DOCS ;] WORD-DOCS:E-SOURCE TTHROWSQ
   WORD-DOCS:COUNT 1 T=
   0 s" KEPT" s" " s" --" REC=
   WORD-DOCS:RESET ;

\ ---- storage past one chunk and one index capacity ------------------------------

: MANY-FIXTURE ( -- )
   0 FIX-U !
   MANY 0 ?do
      SB-RESET s" \ note " SB-APPEND i FMT:SB-U STR-LF SB-APPEND-C
      s" : W" SB-APPEND i FMT:SB-U s"  ( -- ) ;" SB-APPEND STR-LF SB-APPEND-C +SB
   loop
   LONG-LINES 0 ?do
      SB-RESET s" \ " SB-APPEND LONG-WIDTH 0 ?do X-BYTE SB-APPEND-C loop STR-LF SB-APPEND-C +SB
   loop
   s" : LONG ( -- ) ;" +FIX ;

: MANY-RECORDS ( -- )
   s" many" T-LABEL
   MANY-FIXTURE FIX$ DOCS
   WORD-DOCS:COUNT MANY 1+ T=
   0 s" W0" s" " s" --" REC=
   0 WORD-DOCS:COMMENT$ s" note 0" T$=
   MANY 1- WORD-DOCS:NAME$ s" W1499" T$=
   MANY 1- WORD-DOCS:COMMENT$ s" note 1499" T$=
   MANY s" LONG" s" " s" --" REC=
   MANY WORD-DOCS:COMMENT$ {: c:ptr cu:n :}
   cu LONG-LINES LONG-WIDTH 1+ * 1- T=
   c c@ X-BYTE T=
   c LONG-WIDTH + c@ STR-LF T=
   c cu + 1- c@ X-BYTE T=
   WORD-DOCS:RESET ;

\ ---- a real source file --------------------------------------------------------

: READ-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu FS:OPEN-SIZED {: fd size:n ok :}
   ok TTRUE
   size FIX-CAP > if E-STR-CAPACITY throw then
   fd FIX@ size FS:READ-SIZED
   size FIX-U ! ;

\ Record k named a u.
: FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   WORD-DOCS:COUNT 0 ?do
      i WORD-DOCS:NAME$ a u STR= if i unloop exit then
   loop
   -1 ;

: REAL-FILE ( -- )
   s" src/lib/unicode.f" T-LABEL
   s" src/lib/unicode.f" READ-FILE FIX$ DOCS
   s" CASEFOLD=" FIND {: k:n :}
   k s" CASEFOLD=" s" UNICODE" s" ptr u8 n ptr u8 n -- bool" REC=
   k WORD-DOCS:PUBLIC? TTRUE
   s" RESULT" FIND {: r:n :}
   r s" RESULT" s" UNICODE" s" -- ptr n" REC=
   r WORD-DOCS:PUBLIC? TFALSE
   s" Slot 15 is task-local FFI scratch, beyond either call's arguments."
   {: first:ptr fu:n :}
   r WORD-DOCS:COMMENT$ drop {: c:ptr :}
   c fu first fu T$=
   c fu + c@ STR-LF T=
   s" UNICODE-CLASS:WHITE-SPACE?" FIND {: w:n :}
   w s" UNICODE-CLASS:WHITE-SPACE?" s" UNICODE" s" " REC=
   w WORD-DOCS:PUBLIC? TTRUE
   w WORD-DOCS:EXPORT? TTRUE
   WORD-DOCS:RESET ;

public

: RUN ( -- )
   FIX-OPEN
   RULES BARE-NOTES APPEND REFUSALS MANY-RECORDS REAL-FILE
   FIX-CLOSE ;

;package

T-RESET
WORD-DOCS-TEST:RUN
T-REPORT
