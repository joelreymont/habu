\ json-write-test.f - focused tests for the caller-owned checked JSON writer.
\ Run: bin/hb --load src/lib/json-write-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/property.f
require src/lib/json.f
require src/lib/byte-buffer.f
require src/lib/memory.f
require src/lib/test/guard-page.f

package JSON-TEST

10 constant JWT-LF
128 constant JWT-CAP   \ holds the indented nested document
8 constant JWT-SMALL-CAP
20 constant JWT-FILL-N

JWT-CAP BUFFER: JWT-BUF-A
JWT-CAP BUFFER: JWT-BUF-B

TYPED-VARIABLE JWT-A JSON:writer
TYPED-VARIABLE JWT-B JSON:writer
TYPED-VARIABLE JWT-FRESH JSON:writer   \ never opened: keeps the definer's zero image

create JWT-GROW-BUF BUF:HDR-BYTES allot
TYPED-VARIABLE JWT-GROW-W JSON:writer
1024 BUFFER: JWT-LARGE-CHUNK

\ Round-trip fixture storage: the writer's bytes, the reader's decode buffer,
\ and the reader's own caller-owned state block.
$400 constant JWT-RT-CAP
$40 constant JWT-RT-MAX-N
JWT-RT-CAP BUFFER: JWT-RT-OUT
JWT-RT-CAP BUFFER: JWT-RT-DECODE
TYPED-VARIABLE JWT-RT-W JSON:writer
TYPED-VARIABLE JWT-RT-READER JSON:cursor

\ Every escape kind: a quote, a backslash, a named control, and \u00XX with a
\ digit and with a letter.
create JWT-ESC-IN
   65 c, char " c, char \ c, JWT-LF c, 1 c, $1F c, 66 c,

create JWT-ESC-WANT
   char " c, 65 c, char \ c, char " c, char \ c, char \ c,
   char \ c, 110 c, char \ c, 117 c, char 0 c, char 0 c,
   char 0 c, 49 c, char \ c, 117 c, char 0 c, char 0 c, 49 c, 102 c,
   66 c, char " c,

create JWT-NAME
   65 c, char " c, 66 c,

\ A quoted and backslashed UTF-8 source crosses both grow thresholds when it
\ aliases the output: once for its plain width, again for its escaped width.
create JWT-RUN-IN
   65 c, char " c, $C3 c, $A9 c, char \ c, 66 c, 67 c,
create JWT-RUN-KEY-WANT
   65 c, char " c, $C3 c, $A9 c, char \ c, 66 c, 67 c,
   char " c, 65 c, char \ c, char " c, $C3 c, $A9 c,
   char \ c, char \ c, 66 c, 67 c, char " c, char : c,

: JWT-ESC-IN$ ( -- ptr u8 n )
   JWT-ESC-IN 7 ;

: JWT-ESC-WANT$ ( -- ptr u8 n )
   JWT-ESC-WANT 22 ;

: JWT-NAME$ ( -- ptr u8 n )
   JWT-NAME 3 ;

: JWT-TRUE ( -- bool )
   0 0= ;

: JWT-FALSE ( -- bool )
   JWT-TRUE 0= ;

: JWT-CHECK-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: JWT-CHECK-ACCEPTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: JWT-OPEN-A ( -- ptr JSON:writer )
   JWT-A JWT-BUF-A JWT-CAP JSON:WRITE-OPEN ;

: JWT-OPEN-B ( -- ptr JSON:writer )
   JWT-B JWT-BUF-B JWT-CAP JSON:WRITE-OPEN ;

: JWT-OPEN-SMALL ( -- ptr JSON:writer )
   JWT-A JWT-BUF-A JWT-SMALL-CAP JSON:WRITE-OPEN ;

: JWT-EXPECTED-OBJECT$ ( -- ptr u8 n )
   SB-RESET
   [char] { SB-APPEND-C
   [char] " SB-APPEND-C s" name" SB-APPEND [char] " SB-APPEND-C [char] : SB-APPEND-C
   [char] " SB-APPEND-C 65 SB-APPEND-C [char] \ SB-APPEND-C [char] " SB-APPEND-C
   66 SB-APPEND-C [char] " SB-APPEND-C
   [char] , SB-APPEND-C
   [char] " SB-APPEND-C s" count" SB-APPEND [char] " SB-APPEND-C [char] : SB-APPEND-C
   s" 42" SB-APPEND
   [char] , SB-APPEND-C
   [char] " SB-APPEND-C s" ok" SB-APPEND [char] " SB-APPEND-C [char] : SB-APPEND-C
   s" true" SB-APPEND
   [char] , SB-APPEND-C
   [char] " SB-APPEND-C s" none" SB-APPEND [char] " SB-APPEND-C [char] : SB-APPEND-C
   s" null" SB-APPEND
   [char] } SB-APPEND-C
   SB$ ;

: JWT-EXPECTED-ARRAY$ ( -- ptr u8 n )
   SB-RESET
   [char] [ SB-APPEND-C
   s" 1" SB-APPEND
   [char] , SB-APPEND-C
   [char] " SB-APPEND-C 120 SB-APPEND-C [char] " SB-APPEND-C
   [char] , SB-APPEND-C
   s" false" SB-APPEND
   [char] ] SB-APPEND-C
   SB$ ;

: JWT-BUILD-OBJECT ( ptr JSON:writer -- ptr JSON:writer )
   JSON:WRITE-OBJECT-START
   s" name" JWT-NAME$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" count" 42 JSON:WRITE-FIELD-U
   JSON:WRITE-COMMA
   s" ok" JWT-TRUE JSON:WRITE-FIELD-BOOL
   JSON:WRITE-COMMA
   s" none" JSON:WRITE-FIELD-NULL
   JSON:WRITE-OBJECT-END ;

: JWT-BUILD-ARRAY ( ptr JSON:writer -- ptr JSON:writer )
   JSON:WRITE-ARRAY-START
   1 JSON:WRITE-U
   JSON:WRITE-COMMA
   s" x" JSON:WRITE-STRING
   JSON:WRITE-COMMA
   JWT-FALSE JSON:WRITE-BOOL
   JSON:WRITE-ARRAY-END ;

: JWT-TEST-STRING-ESCAPE ( -- )
   JWT-OPEN-A JWT-ESC-IN$ JSON:WRITE-STRING
   JSON:WRITTEN$ JWT-ESC-WANT$ T$= ;

: JWT-TEST-OBJECT ( -- )
   JWT-OPEN-A JWT-BUILD-OBJECT
   JSON:WRITTEN$ JWT-EXPECTED-OBJECT$ T$= ;

: JWT-TEST-ARRAY ( -- )
   JWT-OPEN-A JWT-BUILD-ARRAY
   JSON:WRITTEN$ JWT-EXPECTED-ARRAY$ T$= ;

\ The point of caller-owned storage: two writers are alive at the same time over
\ different buffers, each document is interleaved with the other, and neither
\ sees the other's bytes.
: JWT-TEST-TWO-WRITERS ( -- )
   JWT-OPEN-A JSON:WRITE-OBJECT-START s" count" 42 JSON:WRITE-FIELD-U drop
   JWT-OPEN-B JSON:WRITE-ARRAY-START 1 JSON:WRITE-U JSON:WRITE-COMMA drop
   JWT-A JSON:WRITE-COMMA s" ok" JWT-TRUE JSON:WRITE-FIELD-BOOL JSON:WRITE-OBJECT-END drop
   JWT-B 2 JSON:WRITE-U JSON:WRITE-ARRAY-END drop
   JWT-A JSON:WRITTEN$ {: a:ptr au:n :}
   JWT-B JSON:WRITTEN$ {: b:ptr bu:n :}
   a au s\" {\"count\":42,\"ok\":true}" T$=
   b bu s" [1,2]" T$=
   a b <> TTRUE ;

: JWT-TEST-RESET ( -- )
   JWT-OPEN-A s" abc" JSON:WRITE-RAW drop
   JWT-A JSON:WRITE-RESET drop
   JWT-A JSON:WRITTEN$ nip 0 T=
   JWT-A s" z" JSON:WRITE-RAW JSON:WRITTEN$ s" z" T$= ;

: JWT-GROW-OPEN ( -- ptr JSON:writer )
   JWT-GROW-BUF 8 BUF:N>BLEN BUF:INIT
   JWT-GROW-W JWT-GROW-BUF JSON:WRITE-OPEN-BUF ;

: JWT-TEST-GROW-LIFECYCLE ( -- )
   JWT-GROW-BUF 8 BUF:N>BLEN BUF:INIT
   s" stale" BUF:N>BLEN JWT-GROW-BUF BUF:APPEND-SPAN
   JWT-GROW-W JWT-GROW-BUF JSON:WRITE-OPEN-BUF JSON:WRITTEN$ nip 0 T=
   JWT-GROW-W s" one" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITE-RESET JSON:WRITTEN$ nip 0 T=
   JWT-GROW-W s" two" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITE-CLOSE
   JWT-GROW-BUF BUF:SPAN$ BUF:BLEN>N s" two" T$=
   JWT-GROW-BUF BUF:DISPOSE
   [: JWT-GROW-W JSON:WRITTEN$ 2drop ;] E-JSON-WRITE-STATE TTHROWSQ ;

: JWT-TEST-GROW-ESCAPE ( -- )
   JWT-GROW-OPEN JWT-ESC-IN$ JSON:WRITE-STRING
   JSON:WRITTEN$ JWT-ESC-WANT$ T$=
   JWT-GROW-W JSON:WRITE-RESET
   s" é" JSON:WRITE-STRING JSON:WRITTEN$ s\" \"é\"" T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-OBJECT ( -- )
   JWT-GROW-OPEN JWT-BUILD-OBJECT
   JSON:WRITTEN$ JWT-EXPECTED-OBJECT$ T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-RAW-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a 1+ 5 JSON:WRITE-RAW
   JSON:WRITTEN$ s" abcdefbcdef" T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-STRING-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a 1+ 5 JSON:WRITE-STRING
   JSON:WRITTEN$ s\" abcdef\"bcdef\"" T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-FIELD-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a 3 a 3 + 3 JSON:WRITE-FIELD-S
   JSON:WRITTEN$ s\" abcdef\"abc\":\"def\"" T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-KEY-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a u JSON:WRITE-KEY JSON:WRITTEN$ s\" abcdef\"abcdef\":" T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-ESC-ALIAS ( -- )
   JWT-GROW-OPEN JWT-RUN-IN 7 JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a u JSON:WRITE-STRING JSON:WRITTEN$ JWT-RUN-KEY-WANT 18 T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE
   JWT-GROW-OPEN JWT-RUN-IN 7 JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: key:ptr keyu:n :}
   JWT-GROW-W key keyu JSON:WRITE-KEY JSON:WRITTEN$ JWT-RUN-KEY-WANT 19 T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-FIELD-RAW-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a 3 a 3 + 3 JSON:WRITE-FIELD-RAW
   JSON:WRITTEN$ s\" abcdef\"abc\":def" T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-LARGE ( -- )
   1024 0 ?do 65 JWT-LARGE-CHUNK i + c! loop
   JWT-GROW-OPEN JSON:WRITE-STRING-START
   1025 0 ?do JWT-LARGE-CHUNK 1024 JSON:WRITE-STRING-PART loop
   JSON:WRITE-STRING-END
   JSON:WRITTEN$ {: a:ptr u:n :}
   u 1025 1024 * 2 + T=
   a c@ [char] " T=
   a 1+ c@ 65 T=
   a u 2 - + c@ 65 T=
   a u 1- + c@ [char] " T=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

\ Every ASCII byte value escaped into fixed output and into a BUF it outgrows, as
\ a string and as a key: the BUF holds the fixed output after its prefix.
$80 constant JWT-EVERY-N
JWT-EVERY-N 6 * 3 + constant JWT-EVERY-CAP   \ at most six bytes each, quotes, colon
JWT-EVERY-CAP BUFFER: JWT-EVERY-OUT

: JWT-EVERY-EMIT ( ptr JSON:writer bool -- ptr JSON:writer ) {: key:bool :}
   JWT-LARGE-CHUNK JWT-EVERY-N key if JSON:WRITE-KEY else JSON:WRITE-STRING then ;

: JWT-EVERY ( bool -- ) {: key:bool :}
   JWT-EVERY-N 0 ?do i JWT-LARGE-CHUNK i + c! loop
   JWT-A JWT-EVERY-OUT JWT-EVERY-CAP JSON:WRITE-OPEN key JWT-EVERY-EMIT JSON:WRITTEN$ {: f:ptr fu:n :}
   JWT-GROW-OPEN s" pre" JSON:WRITE-RAW key JWT-EVERY-EMIT JSON:WRITTEN$ {: g:ptr gu:n :}
   gu fu 3 + T=
   g 3 s" pre" T$=
   g 3 + gu 3 - f fu T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-EVERY-BYTE ( -- )
   JWT-FALSE JWT-EVERY
   JWT-TRUE JWT-EVERY ;

: JWT-TEST-GROW-ROOM ( -- )
   JWT-GROW-OPEN s" abcdefgh" JSON:WRITE-RAW s" i" JSON:WRITE-RAW
   JSON:WRITTEN$ s" abcdefghi" T$=
   JWT-GROW-BUF BUF:CAP@ BUF:BLEN>N 16 T=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-GROW-NULL ( -- )
   JWT-GROW-W NULL-PTR JSON:WRITE-OPEN-BUF drop ;

: JWT-TEST-GROW-STATE ( -- )
   [: JWT-GROW-NULL ;] E-JSON-WRITE-OUTPUT TTHROWSQ ;

: JWT-TEST-GROW-DISPOSED-CLOSE ( -- )
   JWT-GROW-OPEN s" x" JSON:WRITE-RAW drop
   JWT-GROW-BUF BUF:DISPOSE
   JWT-GROW-W JSON:WRITE-CLOSE
   JWT-GROW-BUF 8 BUF:N>BLEN BUF:INIT
   [: JWT-GROW-W s" y" JSON:WRITE-RAW drop ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-GROW-W JSON:WRITE-CLOSE ;] E-JSON-WRITE-STATE TTHROWSQ
   JWT-GROW-BUF BUF:DISPOSE ;

: JWT-FILL-A ( -- )
   JWT-OPEN-A
   JWT-FILL-N 0 ?do s" a" JSON:WRITE-RAW loop
   drop ;

: JWT-TEST-SELF-RAW ( -- )
   JWT-FILL-A
   JWT-A JSON:WRITTEN$ {: a:ptr u:n :}
   JWT-A a u JSON:WRITE-RAW drop
   JWT-A JSON:WRITTEN$ {: b:ptr v:n :}
   v JWT-FILL-N 2 * T=
   b c@ 97 T=
   b JWT-FILL-N + c@ 97 T=
   b v 1 - + c@ 97 T= ;

: JWT-TEST-SELF-STRING ( -- )
   JWT-FILL-A
   JWT-A JSON:WRITTEN$ {: a:ptr u:n :}
   JWT-A a u JSON:WRITE-STRING drop
   JWT-A JSON:WRITTEN$ {: b:ptr v:n :}
   v JWT-FILL-N 2 * 2 + T=
   b c@ 97 T=
   b JWT-FILL-N 1 - + c@ 97 T=
   b JWT-FILL-N + c@ [char] " T=
   b JWT-FILL-N 1+ + c@ 97 T=
   b v 1 - + c@ [char] " T= ;

\ A value that does not fit is refused by name and appends nothing. The escaped
\ cases leave room for their unescaped width, but not the escaped width.
: JWT-FULL-RAW ( -- )
   JWT-OPEN-SMALL s" abcde" JSON:WRITE-RAW s" fghij" JSON:WRITE-RAW drop ;

: JWT-FULL-STRING ( -- )
   JWT-OPEN-SMALL s" abcde" JSON:WRITE-RAW s" xy" JSON:WRITE-STRING drop ;

: JWT-FULL-STRING-ESC ( -- )
   JWT-OPEN-SMALL s" abcd" JSON:WRITE-RAW JWT-RUN-IN 2 JSON:WRITE-STRING drop ;

: JWT-FULL-KEY-ESC ( -- )
   JWT-OPEN-SMALL s" abc" JSON:WRITE-RAW JWT-RUN-IN 2 JSON:WRITE-KEY drop ;

: JWT-FULL-U ( -- )
   JWT-OPEN-SMALL s" abcde" JSON:WRITE-RAW 1000 JSON:WRITE-U drop ;

: JWT-FULL-KEY ( -- )
   JWT-OPEN-SMALL s" abcde" JSON:WRITE-RAW s" k" JSON:WRITE-KEY drop ;

: JWT-FULL-INT ( -- )
   JWT-OPEN-SMALL s" abcde" JSON:WRITE-RAW -123 JSON:WRITE-INT drop ;

: JWT-FULL-INT-MIN ( -- )
   JWT-OPEN-SMALL s" abcde" JSON:WRITE-RAW STR-MIN-I64 JSON:WRITE-INT drop ;

: JWT-ZERO-CAP ( -- )
   JWT-A JWT-BUF-A 0 JSON:WRITE-OPEN s" x" JSON:WRITE-RAW drop ;

: JWT-REFUSED-KEEPS$ ( [ -- ] ptr u8 n -- )   \ refusal left the document untouched
   {: want:ptr wu:n :}
   E-JSON-WRITE-CAPACITY TTHROWSQ
   JWT-A JSON:WRITTEN$ want wu T$= ;

: JWT-TEST-FULL ( -- )
   [: JWT-FULL-RAW ;] s" abcde" JWT-REFUSED-KEEPS$
   [: JWT-FULL-STRING ;] s" abcde" JWT-REFUSED-KEEPS$
   [: JWT-FULL-STRING-ESC ;] s" abcd" JWT-REFUSED-KEEPS$
   [: JWT-FULL-KEY-ESC ;] s" abc" JWT-REFUSED-KEEPS$
   [: JWT-FULL-U ;] s" abcde" JWT-REFUSED-KEEPS$
   [: JWT-FULL-KEY ;] s" abcde" JWT-REFUSED-KEEPS$
   [: JWT-FULL-INT ;] s" abcde" JWT-REFUSED-KEEPS$
   [: JWT-FULL-INT-MIN ;] s" abcde" JWT-REFUSED-KEEPS$
   [: JWT-ZERO-CAP ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   JWT-A JSON:WRITTEN$ nip 0 T= ;

: JWT-FIELD-S-BAD-VALUE ( -- )
   JWT-OPEN-SMALL s" ab" JSON:WRITE-RAW s" k" s" v" drop -1 JSON:WRITE-FIELD-S drop ;

: JWT-FIELD-RAW-BAD-VALUE ( -- )
   JWT-OPEN-SMALL s" ab" JSON:WRITE-RAW s" k" s" v" drop -1 JSON:WRITE-FIELD-RAW drop ;

: JWT-FIELD-S-FULL-KEY ( -- )
   JWT-OPEN-SMALL s" ab" JSON:WRITE-RAW s" longkey" s" v" drop -1 JSON:WRITE-FIELD-S drop ;

: JWT-FIELD-RAW-FULL-KEY ( -- )
   JWT-OPEN-SMALL s" ab" JSON:WRITE-RAW s" longkey" s" v" drop -1 JSON:WRITE-FIELD-RAW drop ;

: JWT-TEST-FIELD-REFUSAL ( -- )
   [: JWT-FIELD-S-BAD-VALUE ;] E-JSON-WRITE-SOURCE TTHROWSQ
   JWT-A JSON:WRITTEN$ s\" ab\"k\":" T$=
   [: JWT-FIELD-RAW-BAD-VALUE ;] E-JSON-WRITE-SOURCE TTHROWSQ
   JWT-A JSON:WRITTEN$ s\" ab\"k\":" T$=
   [: JWT-FIELD-S-FULL-KEY ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   JWT-A JSON:WRITTEN$ s" ab" T$=
   [: JWT-FIELD-RAW-FULL-KEY ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   JWT-A JSON:WRITTEN$ s" ab" T$= ;

\ A writer that was never opened and one that was closed are both refused by
\ name; neither reads or writes the caller's bytes.
: JWT-FRESH-RAW ( -- )
   JWT-FRESH s" x" JSON:WRITE-RAW drop ;

: JWT-FRESH-$ ( -- )
   JWT-FRESH JSON:WRITTEN$ 2drop ;

: JWT-FRESH-FIELD-S ( -- )
   JWT-FRESH s" k" s" v" drop -1 JSON:WRITE-FIELD-S drop ;

: JWT-FRESH-FIELD-RAW ( -- )
   JWT-FRESH s" k" s" v" drop -1 JSON:WRITE-FIELD-RAW drop ;

: JWT-CLOSED-FIELD-S-NEG-KEY ( -- )
   JWT-OPEN-B JSON:WRITE-CLOSE
   JWT-B s" k" drop -1 s" v" JSON:WRITE-FIELD-S drop ;

: JWT-CLOSED-FIELD-RAW-NEG-KEY ( -- )
   JWT-OPEN-B JSON:WRITE-CLOSE
   JWT-B s" k" drop -1 s" v" JSON:WRITE-FIELD-RAW drop ;

: JWT-CLOSED-RAW ( -- )
   JWT-OPEN-B JSON:WRITE-CLOSE
   JWT-B s" x" JSON:WRITE-RAW drop ;

: JWT-CLOSED-TWICE ( -- )
   JWT-OPEN-B JSON:WRITE-CLOSE
   JWT-B JSON:WRITE-CLOSE ;

: JWT-NULL-OUT ( -- ptr u8 )   \ the zero image's null output pointer
   JWT-FRESH @ JSON-WRITER:UNMAKE 2drop 2drop 2drop ;

: JWT-CLOSED-FIELD-S-NULL-KEY ( -- )
   JWT-OPEN-B JSON:WRITE-CLOSE
   JWT-B JWT-NULL-OUT 1 s" v" JSON:WRITE-FIELD-S drop ;

: JWT-CLOSED-FIELD-RAW-NULL-KEY ( -- )
   JWT-OPEN-B JSON:WRITE-CLOSE
   JWT-B JWT-NULL-OUT 1 s" v" JSON:WRITE-FIELD-RAW drop ;

: JWT-OPEN-NULL ( -- )
   JWT-A JWT-NULL-OUT JWT-CAP JSON:WRITE-OPEN drop ;

: JWT-OPEN-NEG ( -- )
   JWT-A JWT-BUF-A -1 JSON:WRITE-OPEN drop ;

: JWT-NULL-SOURCE ( -- )
   JWT-OPEN-A JWT-NULL-OUT 3 JSON:WRITE-RAW drop ;

: JWT-TEST-STATE ( -- )
   [: JWT-FRESH-RAW ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-FRESH-$ ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-FRESH-FIELD-S ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-FRESH-FIELD-RAW ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-CLOSED-FIELD-S-NEG-KEY ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-CLOSED-FIELD-RAW-NEG-KEY ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-CLOSED-FIELD-S-NULL-KEY ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-CLOSED-FIELD-RAW-NULL-KEY ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-CLOSED-RAW ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-CLOSED-TWICE ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-OPEN-NULL ;] E-JSON-WRITE-OUTPUT TTHROWSQ
   [: JWT-OPEN-NEG ;] E-JSON-WRITE-OUTPUT TTHROWSQ
   [: JWT-NULL-SOURCE ;] E-JSON-WRITE-SOURCE TTHROWSQ
   JWT-OPEN-B s" ok" JSON:WRITE-RAW JSON:WRITTEN$ s" ok" T$= ;

: JWT-RAW-NEG ( -- )
   JWT-OPEN-A s" x" drop -1 JSON:WRITE-RAW drop ;

: JWT-STRING-NEG ( -- )
   JWT-OPEN-A s" x" drop -1 JSON:WRITE-STRING drop ;

: JWT-U-NEG ( -- )
   JWT-OPEN-A -1 JSON:WRITE-U drop ;

: JWT-STRING-MIN ( -- )
   JWT-OPEN-A s" x" drop MEM-MAX-N negate 1- JSON:WRITE-STRING drop ;

\ Every source byte takes at least one byte of output, so text longer than the
\ room is refused before a byte of it is read: these bytes end at an
\ inaccessible page, and the claim runs one past them.
: JWT-STRING-UNREAD ( n -- ) {: u:n :}
   JWT-OPEN-SMALL JWT-SMALL-CAP [char] a GUARD-PAGE:TAIL u JSON:WRITE-STRING drop ;

: JWT-KEY-UNREAD ( n -- ) {: u:n :}
   JWT-OPEN-SMALL JWT-SMALL-CAP [char] a GUARD-PAGE:TAIL u JSON:WRITE-KEY drop ;

: JWT-TEST-ERRORS ( -- )
   [: JWT-RAW-NEG ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-STRING-NEG ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-STRING-MIN ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-U-NEG ;] E-JSON-WRITE-BYTE TTHROWSQ
   [: JWT-SMALL-CAP 1+ JWT-STRING-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   [: MEM-MAX-N JWT-STRING-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   [: JWT-SMALL-CAP 1+ JWT-KEY-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   [: MEM-MAX-N JWT-KEY-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ ;

\ WRITE-INT writes every cell: zero, both ends of the range, and negatives such
\ as JSON-RPC's error codes, each compared with its exact decimal text.
: JWT-INT$ ( n -- ptr u8 n )
   {: v:n :}
   JWT-OPEN-A v JSON:WRITE-INT JSON:WRITTEN$ ;

: JWT-INT= ( n ptr u8 n -- )
   {: v:n a:ptr u:n :}
   v JWT-INT$ a u T$= ;

: JWT-TEST-INT ( -- )
   0 s" 0" JWT-INT=
   7 s" 7" JWT-INT=
   -7 s" -7" JWT-INT=
   -32601 s" -32601" JWT-INT=
   STR-MAX-I64 s" 9223372036854775807" JWT-INT=
   STR-MIN-I64 s" -9223372036854775808" JWT-INT=
   STR-MIN-I64 1+ s" -9223372036854775807" JWT-INT= ;

\ The room WRITE-INT reserves is exact: a number that just fits is written
\ whole, the minimum cell's twenty bytes included.
: JWT-TEST-INT-FIT ( -- )
   JWT-OPEN-SMALL s" abcde" JSON:WRITE-RAW -12 JSON:WRITE-INT JSON:WRITTEN$ s" abcde-12" T$=
   JWT-A JWT-BUF-A 20 JSON:WRITE-OPEN STR-MIN-I64 JSON:WRITE-INT JSON:WRITTEN$ s" -9223372036854775808" T$= ;

: JWT-TEST-FIELD-INT ( -- )
   JWT-OPEN-A JSON:WRITE-OBJECT-START s" code" -32601 JSON:WRITE-FIELD-INT JSON:WRITE-OBJECT-END
   JSON:WRITTEN$ s\" {\"code\":-32601}" T$= ;

\ One random source string: plain ASCII, a control byte, a quote, a backslash,
\ or a two-byte UTF-8 scalar, so every escape width is exercised.
: JWT-RT-BYTE+ ( -- )
   8 PROP:RND% {: pick:n :}
   pick 0 = if 32 PROP:RND% PROP:BUF-C+ exit then
   pick 1 = if [char] " PROP:BUF-C+ exit then
   pick 2 = if [char] \ PROP:BUF-C+ exit then
   pick 3 = if
      30 PROP:RND% $C2 + PROP:BUF-C+
      64 PROP:RND% $80 + PROP:BUF-C+ exit
   then
   95 PROP:RND% 32 + PROP:BUF-C+ ;

: JWT-RT-CASE$ ( -- ptr u8 n )
   PROP:BUF-RESET
   JWT-RT-MAX-N PROP:RND% 0 ?do JWT-RT-BYTE+ loop
   PROP:BUF$ ;

: JWT-RT-ENCODE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   JWT-RT-W JWT-RT-OUT JWT-RT-CAP JSON:WRITE-OPEN a u JSON:WRITE-STRING JSON:WRITTEN$ ;

: JWT-RT-DECODE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   JWT-RT-READER a u JSON:READ-INIT
   JSON:READ-NEXT JSON:T-STR T=
   JWT-RT-DECODE JWT-RT-CAP JSON:READ-STR {: got:n :}
   JSON:READ-CLOSE
   JWT-RT-DECODE got ;

: JWT-RT-FIT ( ptr u8 n n -- ) {: a:ptr u:n cap:n :}
   JWT-RT-W JWT-RT-OUT cap JSON:WRITE-OPEN a u JSON:WRITE-STRING drop ;

: JWT-RT-ONE ( -- )
   JWT-RT-CASE$ {: a:ptr u:n :}
   a u JWT-RT-ENCODE {: e:ptr eu:n :}
   e eu JWT-RT-DECODE$ a u T$=
   [: a u eu JWT-RT-FIT ;] 0 TTHROWSQ
   [: a u eu 1- JWT-RT-FIT ;] E-JSON-WRITE-CAPACITY TTHROWSQ ;

\ 2048 random strings: what WRITE-STRING reserves is what it writes, the output
\ is a well-formed JSON string to the strict reader, and it decodes back
\ unchanged.
: JWT-TEST-ROUNDTRIP ( -- )
   $517A32D PROP:SEED!
   2048 0 ?do JWT-RT-ONE loop ;

\ WRITE-STRING and WRITE-KEY take UTF-8 text: a malformed sequence is E-JSON-WRITE-BYTE and appends
\ nothing. Valid two-, three- and four-byte scalars read back unchanged.
create JWT-FFFE $FF c, $FE c,
create JWT-LONE-CONT $80 c,
create JWT-TRUNCATED $E2 c, $82 c,
create JWT-OVERLONG $C0 c, $AF c,
create JWT-SURROGATE $ED c, $A0 c, $80 c,
create JWT-UTF8-OK $C3 c, $A9 c, $E2 c, $82 c, $AC c, $F0 c, $9F c, $98 c, $80 c,

: JWT-OPEN-AB ( -- ptr JSON:writer )
   JWT-OPEN-A s" ab" JSON:WRITE-RAW ;

: JWT-BAD-FFFE ( -- ) JWT-OPEN-AB JWT-FFFE 2 JSON:WRITE-STRING drop ;
: JWT-BAD-CONT ( -- ) JWT-OPEN-AB JWT-LONE-CONT 1 JSON:WRITE-STRING drop ;
: JWT-BAD-TRUNCATED ( -- ) JWT-OPEN-AB JWT-TRUNCATED 2 JSON:WRITE-STRING drop ;
: JWT-BAD-OVERLONG ( -- ) JWT-OPEN-AB JWT-OVERLONG 2 JSON:WRITE-STRING drop ;
: JWT-BAD-SURROGATE ( -- ) JWT-OPEN-AB JWT-SURROGATE 3 JSON:WRITE-STRING drop ;
: JWT-BAD-KEY ( -- ) JWT-OPEN-AB JWT-FFFE 1 JSON:WRITE-KEY drop ;

: JWT-BYTE-REFUSED ( [ -- ] -- )
   E-JSON-WRITE-BYTE TTHROWSQ
   JWT-A JSON:WRITTEN$ s" ab" T$= ;

: JWT-TEST-UTF8 ( -- )
   [: JWT-BAD-FFFE ;] JWT-BYTE-REFUSED
   [: JWT-BAD-CONT ;] JWT-BYTE-REFUSED
   [: JWT-BAD-TRUNCATED ;] JWT-BYTE-REFUSED
   [: JWT-BAD-OVERLONG ;] JWT-BYTE-REFUSED
   [: JWT-BAD-SURROGATE ;] JWT-BYTE-REFUSED
   [: JWT-BAD-KEY ;] JWT-BYTE-REFUSED
   JWT-UTF8-OK 9 JWT-RT-ENCODE JWT-RT-DECODE$ JWT-UTF8-OK 9 T$= ;

\ One nested document: a sub-object, an array of two numbers, an empty object
\ and an empty array. Width 2 breaks after each START and COMMA and closes on
\ its own line, except the empty pair; width 0 makes the same calls compact.
: JWT-NESTED ( ptr JSON:writer -- ptr JSON:writer )
   JSON:WRITE-OBJECT-START
   s" a" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
   s" x" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA s" y" JWT-TRUE JSON:WRITE-FIELD-BOOL
   JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
   s" b" JSON:WRITE-KEY JSON:WRITE-ARRAY-START 2 JSON:WRITE-U JSON:WRITE-COMMA 3 JSON:WRITE-U JSON:WRITE-ARRAY-END
   JSON:WRITE-COMMA
   s" c" JSON:WRITE-KEY JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
   s" d" JSON:WRITE-KEY JSON:WRITE-ARRAY-START JSON:WRITE-ARRAY-END
   JSON:WRITE-OBJECT-END ;

: JWT-INDENTED$ ( -- ptr u8 n )
   s\" {\n  \"a\": {\n    \"x\": 1,\n    \"y\": true\n  },\n  \"b\": [\n    2,\n    3\n  ],\n  \"c\": {},\n  \"d\": []\n}" ;

: JWT-TEST-INDENTED ( -- )
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JWT-NESTED JSON:WRITTEN$ JWT-INDENTED$ T$=
   JWT-GROW-OPEN 2 JSON:WRITE-INDENT! JWT-NESTED JSON:WRITTEN$ JWT-INDENTED$ T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

\ Opening a writer again makes it compact, whatever width it had.
: JWT-COMPACT$ ( -- ptr u8 n )
   s\" {\"a\":{\"x\":1,\"y\":true},\"b\":[2,3],\"c\":{},\"d\":[]}" ;

: JWT-TEST-COMPACT ( -- )
   JWT-OPEN-A 2 JSON:WRITE-INDENT! drop
   JWT-OPEN-A JWT-NESTED JSON:WRITTEN$ JWT-COMPACT$ T$= ;

: JWT-TEST-OPEN-BUF-COMPACT ( -- )
   JWT-GROW-OPEN 2 JSON:WRITE-INDENT! drop
   JWT-GROW-W JWT-GROW-BUF JSON:WRITE-OPEN-BUF JWT-NESTED JSON:WRITTEN$ JWT-COMPACT$ T$=
   JWT-GROW-W JSON:WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

\ A string in parts never breaks, even after a part ending in a comma; the
\ COMMA after it puts the number on its own line.
: JWT-TEST-PART-LAYOUT ( -- )
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JSON:WRITE-ARRAY-START
   JSON:WRITE-STRING-START s" a," JSON:WRITE-STRING-PART
   s" b" JSON:WRITE-STRING-PART JSON:WRITE-STRING-END
   JSON:WRITE-COMMA 1 JSON:WRITE-U JSON:WRITE-ARRAY-END
   JSON:WRITTEN$ s\" [\n  \"a,b\",\n  1\n]" T$= ;

\ WRITE-RESET right after a START keeps the width and forgets the open object
\ and its due break.
: JWT-TEST-INDENT-RESET ( -- )
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JSON:WRITE-OBJECT-START JSON:WRITE-RESET
   JWT-NESTED JSON:WRITTEN$ JWT-INDENTED$ T$=
   [: JWT-A JSON:WRITE-OBJECT-END drop ;] E-JSON-WRITE-STATE TTHROWSQ
   JWT-A JSON:WRITTEN$ JWT-INDENTED$ T$= ;

\ a"b, LF, then é: the parts are its first three bytes, the next, the last two.
create JWT-PARTS 97 c, char " c, 98 c, JWT-LF c, $C3 c, $A9 c,
create JWT-PARTS-WANT
   char " c, 97 c, char \ c, char " c, 98 c, char \ c, 110 c,
   $C3 c, $A9 c, char " c,

: JWT-TEST-PARTS ( -- )
   JWT-OPEN-A JSON:WRITE-STRING-START
   JWT-PARTS 3 JSON:WRITE-STRING-PART
   JWT-PARTS 3 + 1 JSON:WRITE-STRING-PART
   JWT-PARTS 4 + 2 JSON:WRITE-STRING-PART
   JSON:WRITE-STRING-END JSON:WRITTEN$ JWT-PARTS-WANT 10 T$=
   JWT-OPEN-B JWT-PARTS 6 JSON:WRITE-STRING JSON:WRITTEN$ JWT-PARTS-WANT 10 T$= ;

: JWT-PART-FF ( -- )
   JWT-OPEN-A JSON:WRITE-STRING-START JWT-FFFE 1 JSON:WRITE-STRING-PART drop ;
: JWT-PART-HALF ( -- )
   JWT-OPEN-A JSON:WRITE-STRING-START JWT-PARTS 4 + 1 JSON:WRITE-STRING-PART drop ;
: JWT-INDENT!-NEG ( -- ) JWT-OPEN-AB -1 JSON:WRITE-INDENT! drop ;
: JWT-PART-FULL ( -- )   \ room for the two raw bytes, not their three escaped
   JWT-OPEN-SMALL s" abcde" JSON:WRITE-RAW JSON:WRITE-STRING-START
   JWT-RUN-IN 2 JSON:WRITE-STRING-PART drop ;
: JWT-KEY-FULL ( -- )   \ room for the break before the key, not for the key
   JWT-OPEN-SMALL 2 JSON:WRITE-INDENT! JSON:WRITE-OBJECT-START s" k" JSON:WRITE-KEY drop ;
: JWT-BREAK-WRAP ( -- )   \ two levels of MEM-MAX-N spaces overflow a cell
   JWT-OPEN-AB JSON:WRITE-ARRAY-START JSON:WRITE-ARRAY-START MEM-MAX-N JSON:WRITE-INDENT!
   1 JSON:WRITE-U drop ;
: JWT-OBJECT-END-TOP ( -- ) JWT-OPEN-AB JSON:WRITE-OBJECT-END drop ;
: JWT-ARRAY-END-TOP ( -- ) JWT-OPEN-AB JSON:WRITE-ARRAY-END drop ;

: JWT-STATE-REFUSED ( [ -- ] -- )
   E-JSON-WRITE-STATE TTHROWSQ
   JWT-A JSON:WRITTEN$ s" ab" T$= ;

: JWT-FRESH-INDENT! ( -- ) JWT-FRESH 2 JSON:WRITE-INDENT! drop ;
: JWT-FRESH-START ( -- ) JWT-FRESH JSON:WRITE-STRING-START drop ;
: JWT-FRESH-PART ( -- ) JWT-FRESH s" x" JSON:WRITE-STRING-PART drop ;
: JWT-FRESH-END ( -- ) JWT-FRESH JSON:WRITE-STRING-END drop ;

: JWT-CLOSED ( -- ptr JSON:writer ) JWT-OPEN-B JSON:WRITE-CLOSE JWT-B ;
: JWT-CLOSED-INDENT! ( -- ) JWT-CLOSED 2 JSON:WRITE-INDENT! drop ;
: JWT-CLOSED-START ( -- ) JWT-CLOSED JSON:WRITE-STRING-START drop ;
: JWT-CLOSED-PART ( -- ) JWT-CLOSED s" x" JSON:WRITE-STRING-PART drop ;
: JWT-CLOSED-END ( -- ) JWT-CLOSED JSON:WRITE-STRING-END drop ;

\ A refused key keeps its depth and due break: the null that fits the same
\ eight bytes writes after the break.
: JWT-TEST-KEY-FULL ( -- )
   [: JWT-KEY-FULL ;] s" {" JWT-REFUSED-KEEPS$
   JWT-A JSON:WRITE-NULL JSON:WRITTEN$ s\" {\n  null" T$= ;

: JWT-TEST-LAYOUT-REFUSALS ( -- )
   [: JWT-PART-FF ;] E-JSON-WRITE-BYTE TTHROWSQ
   JWT-A JSON:WRITTEN$ s\" \"" T$=
   [: JWT-PART-HALF ;] E-JSON-WRITE-BYTE TTHROWSQ
   JWT-A JSON:WRITTEN$ s\" \"" T$=
   [: JWT-INDENT!-NEG ;] JWT-BYTE-REFUSED
   [: JWT-PART-FULL ;] s\" abcde\"" JWT-REFUSED-KEEPS$
   [: JWT-BREAK-WRAP ;] s" ab[[" JWT-REFUSED-KEEPS$
   [: JWT-OBJECT-END-TOP ;] JWT-STATE-REFUSED
   [: JWT-ARRAY-END-TOP ;] JWT-STATE-REFUSED
   [: JWT-FRESH-INDENT! ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-FRESH-START ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-FRESH-PART ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-FRESH-END ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-CLOSED-INDENT! ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-CLOSED-START ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-CLOSED-PART ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-CLOSED-END ;] E-JSON-WRITE-STATE TTHROWSQ ;


\ WRITE-COMMA before every element, the first included, writes what the same
\ calls without the first comma write.
: JWT-COMMA-ARRAY ( ptr JSON:writer -- ptr JSON:writer )
   JSON:WRITE-ARRAY-START JSON:WRITE-COMMA 1 JSON:WRITE-U JSON:WRITE-COMMA 2 JSON:WRITE-U
   JSON:WRITE-ARRAY-END ;


: JWT-COMMA-OBJECT ( ptr JSON:writer -- ptr JSON:writer )
   JSON:WRITE-OBJECT-START JSON:WRITE-COMMA s" a" 1 JSON:WRITE-FIELD-U
   JSON:WRITE-COMMA s" b" 2 JSON:WRITE-FIELD-U JSON:WRITE-OBJECT-END ;


: JWT-TEST-COMMA-FIRST ( -- )
   JWT-OPEN-A JWT-COMMA-ARRAY JSON:WRITTEN$ s" [1,2]" T$=
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JWT-COMMA-ARRAY JSON:WRITTEN$ s\" [\n  1,\n  2\n]" T$=
   JWT-OPEN-A JWT-COMMA-OBJECT JSON:WRITTEN$ s\" {\"a\":1,\"b\":2}" T$=
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JWT-COMMA-OBJECT JSON:WRITTEN$
   s\" {\n  \"a\": 1,\n  \"b\": 2\n}" T$= ;


: JWT-STATE-KEEPS$ ( [ -- ] ptr u8 n -- )   \ refusal left the document untouched
   {: want:ptr wu:n :}
   E-JSON-WRITE-STATE TTHROWSQ
   JWT-A JSON:WRITTEN$ want wu T$= ;


: JWT-COMMA-TWICE ( -- )
   JWT-OPEN-A JSON:WRITE-ARRAY-START 1 JSON:WRITE-U JSON:WRITE-COMMA JSON:WRITE-COMMA drop ;


: JWT-ARRAY-END-COMMA ( -- )
   JWT-OPEN-A JSON:WRITE-ARRAY-START 1 JSON:WRITE-U JSON:WRITE-COMMA JSON:WRITE-ARRAY-END drop ;


: JWT-OBJECT-END-COMMA ( -- )
   JWT-OPEN-A JSON:WRITE-OBJECT-START s" a" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   JSON:WRITE-OBJECT-END drop ;


\ A writer closed right after a START still refuses a comma.
: JWT-CLOSED-OPEN-COMMA ( -- )
   JWT-OPEN-B JSON:WRITE-ARRAY-START JSON:WRITE-CLOSE JWT-B JSON:WRITE-COMMA drop ;


: JWT-TEST-COMMA-REFUSALS ( -- )
   [: JWT-COMMA-TWICE ;] s" [1," JWT-STATE-KEEPS$
   [: JWT-ARRAY-END-COMMA ;] s" [1," JWT-STATE-KEEPS$
   [: JWT-OBJECT-END-COMMA ;] s\" {\"a\":1," JWT-STATE-KEEPS$
   [: JWT-CLOSED-OPEN-COMMA ;] E-JSON-WRITE-STATE TTHROWSQ ;


\ Calls the layout refuses, each on JWT-A; JWT-KEYED$ and JWT-QUOTING$ are
\ what JWT-A holds when the refused call comes.
: JWT-EMPTY$ ( -- ptr u8 n ) JWT-NAME 0 ;
: JWT-KEYED ( -- ptr JSON:writer ) JWT-OPEN-A JSON:WRITE-OBJECT-START s" k" JSON:WRITE-KEY ;
: JWT-KEYED$ ( -- ptr u8 n ) s\" {\"k\":" ;
: JWT-QUOTING ( -- ptr JSON:writer )
   JWT-OPEN-A JSON:WRITE-STRING-START s" ab" JSON:WRITE-STRING-PART ;
: JWT-QUOTING$ ( -- ptr u8 n ) s\" \"ab" ;
: JWT-COMMA-FRESH ( -- ) JWT-OPEN-A JSON:WRITE-COMMA drop ;
: JWT-COMMA-RESET ( -- )
   JWT-OPEN-A s" x" JSON:WRITE-RAW JSON:WRITE-RESET JSON:WRITE-COMMA drop ;
: JWT-COMMA-KEYED ( -- ) JWT-KEYED JSON:WRITE-COMMA drop ;
: JWT-COMMA-QUOTING ( -- ) JWT-QUOTING JSON:WRITE-COMMA drop ;
: JWT-PART-FRESH ( -- ) JWT-OPEN-A s" x" JSON:WRITE-STRING-PART drop ;
: JWT-END-FRESH ( -- ) JWT-OPEN-A JSON:WRITE-STRING-END drop ;
: JWT-PART-VALUED ( -- ) JWT-OPEN-A 1 JSON:WRITE-U s" x" JSON:WRITE-STRING-PART drop ;
: JWT-END-VALUED ( -- ) JWT-OPEN-A 1 JSON:WRITE-U JSON:WRITE-STRING-END drop ;
: JWT-OBJECT-END-KEYED ( -- ) JWT-KEYED JSON:WRITE-OBJECT-END drop ;
: JWT-NULL-QUOTING ( -- ) JWT-QUOTING JSON:WRITE-NULL drop ;
: JWT-KEY-QUOTING ( -- ) JWT-QUOTING s" k" JSON:WRITE-KEY drop ;

: JWT-PIECED ( ptr JSON:writer -- ptr JSON:writer )
   JSON:WRITE-OBJECT-START s" k" JSON:WRITE-KEY JSON:WRITE-STRING-START
   s" a" JSON:WRITE-STRING-PART s" b" JSON:WRITE-STRING-PART JSON:WRITE-STRING-END
   JSON:WRITE-OBJECT-END ;

: JWT-PAIR ( ptr JSON:writer -- ptr JSON:writer )
   JSON:WRITE-ARRAY-START 1 JSON:WRITE-U JSON:WRITE-COMMA s" x" JSON:WRITE-STRING JSON:WRITE-ARRAY-END ;


\ A comma, string piece, END or token where JSON takes none is refused whole.
: JWT-TEST-LAYOUT-STATES ( -- )
   [: JWT-COMMA-FRESH ;] JWT-EMPTY$ JWT-STATE-KEEPS$
   [: JWT-COMMA-RESET ;] JWT-EMPTY$ JWT-STATE-KEEPS$
   [: JWT-COMMA-KEYED ;] JWT-KEYED$ JWT-STATE-KEEPS$
   [: JWT-COMMA-QUOTING ;] JWT-QUOTING$ JWT-STATE-KEEPS$
   [: JWT-PART-FRESH ;] JWT-EMPTY$ JWT-STATE-KEEPS$
   [: JWT-END-FRESH ;] JWT-EMPTY$ JWT-STATE-KEEPS$
   [: JWT-PART-VALUED ;] s" 1" JWT-STATE-KEEPS$
   [: JWT-END-VALUED ;] s" 1" JWT-STATE-KEEPS$
   [: JWT-OBJECT-END-KEYED ;] JWT-KEYED$ JWT-STATE-KEEPS$
   [: JWT-NULL-QUOTING ;] JWT-QUOTING$ JWT-STATE-KEEPS$
   [: JWT-KEY-QUOTING ;] JWT-QUOTING$ JWT-STATE-KEEPS$
   JWT-OPEN-A JWT-PIECED JSON:WRITTEN$ s\" {\"k\":\"ab\"}" T$=
   JWT-OPEN-A JWT-PAIR JSON:WRITTEN$ s\" [1,\"x\"]" T$=
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JWT-PAIR JSON:WRITTEN$ s\" [\n  1,\n  \"x\"\n]" T$= ;


\ The handle is nominal: a raw cell is not a writer.
: JWT-TEST-CHECKER ( -- )
   s" JWT-OK ( ptr JSON:writer -- ptr JSON:writer ) JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END" JWT-CHECK-ACCEPTS
   s" JWT-GROW-OK ( ptr JSON:writer ptr n -- ptr JSON:writer ) JSON:WRITE-OPEN-BUF" JWT-CHECK-ACCEPTS
   s" JWT-GROW-BAD ( ptr JSON:writer ptr u8 -- ptr JSON:writer ) JSON:WRITE-OPEN-BUF" JWT-CHECK-REJECTS
   s" JWT-BAD-CELL ( n ptr u8 n -- n ) JSON:WRITE-RAW" JWT-CHECK-REJECTS
   s" JWT-BAD-BUF ( ptr u8 ptr u8 n -- ptr u8 ) JSON:WRITE-RAW" JWT-CHECK-REJECTS ;

: JWT-MAIN ( -- )
   T-RESET
   JWT-TEST-STRING-ESCAPE
   JWT-TEST-OBJECT
   JWT-TEST-ARRAY
   JWT-TEST-TWO-WRITERS
   JWT-TEST-RESET
   JWT-TEST-GROW-LIFECYCLE
   JWT-TEST-GROW-ESCAPE
   JWT-TEST-GROW-OBJECT
   JWT-TEST-GROW-RAW-ALIAS
   JWT-TEST-GROW-STRING-ALIAS
   JWT-TEST-GROW-FIELD-ALIAS
   JWT-TEST-GROW-KEY-ALIAS
   JWT-TEST-GROW-ESC-ALIAS
   JWT-TEST-GROW-FIELD-RAW-ALIAS
   JWT-TEST-GROW-LARGE
   JWT-TEST-GROW-EVERY-BYTE
   JWT-TEST-GROW-ROOM
   JWT-TEST-GROW-STATE
   JWT-TEST-GROW-DISPOSED-CLOSE
   JWT-TEST-SELF-RAW
   JWT-TEST-SELF-STRING
   JWT-TEST-FULL
   JWT-TEST-FIELD-REFUSAL
   JWT-TEST-STATE
   JWT-TEST-ERRORS
   JWT-TEST-INT
   JWT-TEST-INT-FIT
   JWT-TEST-FIELD-INT
   JWT-TEST-ROUNDTRIP
   JWT-TEST-UTF8
   JWT-TEST-INDENTED
   JWT-TEST-COMPACT
   JWT-TEST-OPEN-BUF-COMPACT
   JWT-TEST-PART-LAYOUT
   JWT-TEST-INDENT-RESET
   JWT-TEST-PARTS
   JWT-TEST-KEY-FULL
   JWT-TEST-LAYOUT-REFUSALS
   JWT-TEST-COMMA-FIRST
   JWT-TEST-COMMA-REFUSALS
   JWT-TEST-LAYOUT-STATES
   JWT-TEST-CHECKER
   T-REPORT
   s" json-write-test: ok" type cr ;

JWT-MAIN

false [if] \ off until dot habu-add-the-checks-a49d054d
: JWT-GROW-DISPOSED ( -- )
   JWT-GROW-BUF 8 BUF:N>BLEN BUF:INIT
   JWT-GROW-BUF BUF:DISPOSE
   JWT-GROW-W JWT-GROW-BUF JSON:WRITE-OPEN-BUF drop ;

: JWT-TEST-GROW-DISPOSED ( -- )
   [: JWT-GROW-DISPOSED ;] E-BUF-STATE TTHROWSQ ;

T-RESET
JWT-TEST-GROW-DISPOSED
T-REPORT
[then]

;package
