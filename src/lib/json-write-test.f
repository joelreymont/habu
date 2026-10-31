\ json-write-test.f - focused tests for the caller-owned checked JSON writer.
\ Run: bin/hb --load src/lib/json-write-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/property.f
require src/lib/json/read.f       \ the files, not src/lib/json.f, which seals JSON
require src/lib/json/write.f
require src/lib/byte-buffer.f
require src/lib/memory.f
require src/lib/test/guard-page.f

\ White-box test: reopen package JSON so the fixtures reach write.f's
\ private byte plumbing (the byte constants, JW-C and JW-ESC-N) and call
\ the public emitters by their bare package-local tails.
package JSON

128 constant JWT-CAP   \ holds the indented nested document
8 constant JWT-SMALL-CAP
20 constant JWT-FILL-N

JWT-CAP BUFFER: JWT-BUF-A
JWT-CAP BUFFER: JWT-BUF-B

TYPED-VARIABLE JWT-A writer
TYPED-VARIABLE JWT-B writer
TYPED-VARIABLE JWT-FRESH writer   \ never opened: keeps the definer's zero image

create JWT-GROW-BUF BUF:HDR-BYTES allot
TYPED-VARIABLE JWT-GROW-W writer
1024 BUFFER: JWT-LARGE-CHUNK

\ Round-trip fixture storage: the writer's bytes, the reader's decode buffer,
\ and the reader's own caller-owned state block.
$400 constant JWT-RT-CAP
$40 constant JWT-RT-MAX-N
JWT-RT-CAP BUFFER: JWT-RT-OUT
JWT-RT-CAP BUFFER: JWT-RT-DECODE
TYPED-VARIABLE JWT-RT-W writer
TYPED-VARIABLE JWT-RT-READER JSON:cursor

\ Every escape kind: a quote, a backslash, a named control, and \u00XX with a
\ digit and with a letter.
create JWT-ESC-IN
   65 c, JW-DQ c, JW-BACKSLASH c, JW-LF c, 1 c, $1F c, 66 c,

create JWT-ESC-WANT
   JW-DQ c, 65 c, JW-BACKSLASH c, JW-DQ c, JW-BACKSLASH c, JW-BACKSLASH c,
   JW-BACKSLASH c, 110 c, JW-BACKSLASH c, 117 c, JW-ZERO c, JW-ZERO c,
   JW-ZERO c, 49 c, JW-BACKSLASH c, 117 c, JW-ZERO c, JW-ZERO c, 49 c, 102 c,
   66 c, JW-DQ c,

create JWT-NAME
   65 c, JW-DQ c, 66 c,

\ A quoted and backslashed UTF-8 source crosses both grow thresholds when it
\ aliases the output: once for its plain width, again for its escaped width.
create JWT-RUN-IN
   65 c, JW-DQ c, $C3 c, $A9 c, JW-BACKSLASH c, 66 c, 67 c,
create JWT-RUN-KEY-WANT
   65 c, JW-DQ c, $C3 c, $A9 c, JW-BACKSLASH c, 66 c, 67 c,
   JW-DQ c, 65 c, JW-BACKSLASH c, JW-DQ c, $C3 c, $A9 c,
   JW-BACKSLASH c, JW-BACKSLASH c, 66 c, 67 c, JW-DQ c, JW-COLON-C c,

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

: JWT-OPEN-A ( -- ptr writer )
   JWT-A JWT-BUF-A JWT-CAP WRITE-OPEN ;

: JWT-OPEN-B ( -- ptr writer )
   JWT-B JWT-BUF-B JWT-CAP WRITE-OPEN ;

: JWT-OPEN-SMALL ( -- ptr writer )
   JWT-A JWT-BUF-A JWT-SMALL-CAP WRITE-OPEN ;

: JWT-EXPECTED-OBJECT$ ( -- ptr u8 n )
   SB-RESET
   JW-LBRACE SB-APPEND-C
   JW-DQ SB-APPEND-C s" name" SB-APPEND JW-DQ SB-APPEND-C JW-COLON-C SB-APPEND-C
   JW-DQ SB-APPEND-C 65 SB-APPEND-C JW-BACKSLASH SB-APPEND-C JW-DQ SB-APPEND-C
   66 SB-APPEND-C JW-DQ SB-APPEND-C
   JW-COMMA-C SB-APPEND-C
   JW-DQ SB-APPEND-C s" count" SB-APPEND JW-DQ SB-APPEND-C JW-COLON-C SB-APPEND-C
   s" 42" SB-APPEND
   JW-COMMA-C SB-APPEND-C
   JW-DQ SB-APPEND-C s" ok" SB-APPEND JW-DQ SB-APPEND-C JW-COLON-C SB-APPEND-C
   s" true" SB-APPEND
   JW-COMMA-C SB-APPEND-C
   JW-DQ SB-APPEND-C s" none" SB-APPEND JW-DQ SB-APPEND-C JW-COLON-C SB-APPEND-C
   s" null" SB-APPEND
   JW-RBRACE SB-APPEND-C
   SB$ ;

: JWT-EXPECTED-ARRAY$ ( -- ptr u8 n )
   SB-RESET
   JW-LBRACK SB-APPEND-C
   s" 1" SB-APPEND
   JW-COMMA-C SB-APPEND-C
   JW-DQ SB-APPEND-C 120 SB-APPEND-C JW-DQ SB-APPEND-C
   JW-COMMA-C SB-APPEND-C
   s" false" SB-APPEND
   JW-RBRACK SB-APPEND-C
   SB$ ;

: JWT-BUILD-OBJECT ( ptr writer -- ptr writer )
   WRITE-OBJECT-START
   s" name" JWT-NAME$ WRITE-FIELD-S
   WRITE-COMMA
   s" count" 42 WRITE-FIELD-U
   WRITE-COMMA
   s" ok" JWT-TRUE WRITE-FIELD-BOOL
   WRITE-COMMA
   s" none" WRITE-FIELD-NULL
   WRITE-OBJECT-END ;

: JWT-BUILD-ARRAY ( ptr writer -- ptr writer )
   WRITE-ARRAY-START
   1 WRITE-U
   WRITE-COMMA
   s" x" WRITE-STRING
   WRITE-COMMA
   JWT-FALSE WRITE-BOOL
   WRITE-ARRAY-END ;

: JWT-TEST-STRING-ESCAPE ( -- )
   JWT-OPEN-A JWT-ESC-IN$ WRITE-STRING
   WRITTEN$ JWT-ESC-WANT$ T$= ;

: JWT-TEST-OBJECT ( -- )
   JWT-OPEN-A JWT-BUILD-OBJECT
   WRITTEN$ JWT-EXPECTED-OBJECT$ T$= ;

: JWT-TEST-ARRAY ( -- )
   JWT-OPEN-A JWT-BUILD-ARRAY
   WRITTEN$ JWT-EXPECTED-ARRAY$ T$= ;

\ The point of caller-owned storage: two writers are alive at the same time over
\ different buffers, each document is interleaved with the other, and neither
\ sees the other's bytes.
: JWT-TEST-TWO-WRITERS ( -- )
   JWT-OPEN-A WRITE-OBJECT-START s" count" 42 WRITE-FIELD-U drop
   JWT-OPEN-B WRITE-ARRAY-START 1 WRITE-U WRITE-COMMA drop
   JWT-A WRITE-COMMA s" ok" JWT-TRUE WRITE-FIELD-BOOL WRITE-OBJECT-END drop
   JWT-B 2 WRITE-U WRITE-ARRAY-END drop
   JWT-A WRITTEN$ {: a:ptr au:n :}
   JWT-B WRITTEN$ {: b:ptr bu:n :}
   a au s\" {\"count\":42,\"ok\":true}" T$=
   b bu s" [1,2]" T$=
   a b <> TTRUE ;

: JWT-TEST-RESET ( -- )
   JWT-OPEN-A s" abc" WRITE-RAW drop
   JWT-A WRITE-RESET drop
   JWT-A WRITTEN$ nip 0 T=
   JWT-A s" z" WRITE-RAW WRITTEN$ s" z" T$= ;

: JWT-GROW-OPEN ( -- ptr writer )
   JWT-GROW-BUF 8 BUF:N>BLEN BUF:INIT
   JWT-GROW-W JWT-GROW-BUF WRITE-OPEN-BUF ;

: JWT-TEST-GROW-LIFECYCLE ( -- )
   JWT-GROW-BUF 8 BUF:N>BLEN BUF:INIT
   s" stale" BUF:N>BLEN JWT-GROW-BUF BUF:APPEND-SPAN
   JWT-GROW-W JWT-GROW-BUF WRITE-OPEN-BUF WRITTEN$ nip 0 T=
   JWT-GROW-W s" one" WRITE-RAW drop
   JWT-GROW-W WRITE-RESET WRITTEN$ nip 0 T=
   JWT-GROW-W s" two" WRITE-RAW drop
   JWT-GROW-W WRITE-CLOSE
   JWT-GROW-BUF BUF:SPAN$ BUF:BLEN>N s" two" T$=
   JWT-GROW-BUF BUF:DISPOSE
   [: JWT-GROW-W WRITTEN$ 2drop ;] E-JSON-WRITE-STATE TTHROWSQ ;

: JWT-TEST-GROW-ESCAPE ( -- )
   JWT-GROW-OPEN JWT-ESC-IN$ WRITE-STRING
   WRITTEN$ JWT-ESC-WANT$ T$=
   JWT-GROW-W WRITE-RESET
   s" é" WRITE-STRING WRITTEN$ s\" \"é\"" T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-OBJECT ( -- )
   JWT-GROW-OPEN JWT-BUILD-OBJECT
   WRITTEN$ JWT-EXPECTED-OBJECT$ T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-RAW-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" WRITE-RAW drop
   JWT-GROW-W WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a 1+ 5 WRITE-RAW
   WRITTEN$ s" abcdefbcdef" T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-STRING-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" WRITE-RAW drop
   JWT-GROW-W WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a 1+ 5 WRITE-STRING
   WRITTEN$ s\" abcdef\"bcdef\"" T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-FIELD-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" WRITE-RAW drop
   JWT-GROW-W WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a 3 a 3 + 3 WRITE-FIELD-S
   WRITTEN$ s\" abcdef\"abc\":\"def\"" T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-KEY-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" WRITE-RAW drop
   JWT-GROW-W WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a u WRITE-KEY WRITTEN$ s\" abcdef\"abcdef\":" T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-ESC-ALIAS ( -- )
   JWT-GROW-OPEN JWT-RUN-IN 7 WRITE-RAW drop
   JWT-GROW-W WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a u WRITE-STRING WRITTEN$ JWT-RUN-KEY-WANT 18 T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE
   JWT-GROW-OPEN JWT-RUN-IN 7 WRITE-RAW drop
   JWT-GROW-W WRITTEN$ {: key:ptr keyu:n :}
   JWT-GROW-W key keyu WRITE-KEY WRITTEN$ JWT-RUN-KEY-WANT 19 T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-FIELD-RAW-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" WRITE-RAW drop
   JWT-GROW-W WRITTEN$ {: a:ptr u:n :}
   JWT-GROW-W a 3 a 3 + 3 WRITE-FIELD-RAW
   WRITTEN$ s\" abcdef\"abc\":def" T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-LARGE ( -- )
   1024 0 ?do 65 JWT-LARGE-CHUNK i + c! loop
   JWT-GROW-OPEN JW-DQ JW-C
   1025 0 ?do JWT-LARGE-CHUNK 1024 WRITE-RAW loop
   JW-DQ JW-C
   WRITTEN$ {: a:ptr u:n :}
   u 1025 1024 * 2 + T=
   a c@ JW-DQ T=
   a 1+ c@ 65 T=
   a u 2 - + c@ 65 T=
   a u 1- + c@ JW-DQ T=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

\ Every ASCII byte value escaped into fixed output and into a BUF it outgrows, as
\ a string and as a key: the BUF holds the fixed output after its prefix.
$80 constant JWT-EVERY-N
JWT-EVERY-N 6 * 3 + constant JWT-EVERY-CAP   \ at most six bytes each, quotes, colon
JWT-EVERY-CAP BUFFER: JWT-EVERY-OUT

: JWT-EVERY-EMIT ( ptr writer bool -- ptr writer ) {: key:bool :}
   JWT-LARGE-CHUNK JWT-EVERY-N key if WRITE-KEY else WRITE-STRING then ;

: JWT-EVERY ( bool -- ) {: key:bool :}
   JWT-EVERY-N 0 ?do i JWT-LARGE-CHUNK i + c! loop
   JWT-A JWT-EVERY-OUT JWT-EVERY-CAP WRITE-OPEN key JWT-EVERY-EMIT WRITTEN$ {: f:ptr fu:n :}
   JWT-GROW-OPEN s" pre" WRITE-RAW key JWT-EVERY-EMIT WRITTEN$ {: g:ptr gu:n :}
   gu fu 3 + T=
   g 3 s" pre" T$=
   g 3 + gu 3 - f fu T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-TEST-GROW-EVERY-BYTE ( -- )
   JWT-FALSE JWT-EVERY
   JWT-TRUE JWT-EVERY ;

: JWT-TEST-GROW-ROOM ( -- )
   JWT-GROW-OPEN s" abcdefgh" WRITE-RAW s" i" WRITE-RAW
   WRITTEN$ s" abcdefghi" T$=
   JWT-GROW-BUF BUF:CAP@ BUF:BLEN>N 16 T=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

: JWT-GROW-DISPOSED ( -- )
   JWT-GROW-BUF 8 BUF:N>BLEN BUF:INIT
   JWT-GROW-BUF BUF:DISPOSE
   JWT-GROW-W JWT-GROW-BUF WRITE-OPEN-BUF drop ;

: JWT-GROW-NULL ( -- )
   JWT-GROW-W NULL-PTR WRITE-OPEN-BUF drop ;

: JWT-TEST-GROW-STATE ( -- )
   [: JWT-GROW-DISPOSED ;] E-BUF-STATE TTHROWSQ
   [: JWT-GROW-NULL ;] E-JSON-WRITE-OUTPUT TTHROWSQ ;

: JWT-TEST-GROW-DISPOSED-CLOSE ( -- )
   JWT-GROW-OPEN s" x" WRITE-RAW drop
   JWT-GROW-BUF BUF:DISPOSE
   JWT-GROW-W WRITE-CLOSE
   JWT-GROW-BUF 8 BUF:N>BLEN BUF:INIT
   [: JWT-GROW-W s" y" WRITE-RAW drop ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-GROW-W WRITE-CLOSE ;] E-JSON-WRITE-STATE TTHROWSQ
   JWT-GROW-BUF BUF:DISPOSE ;

: JWT-FILL-A ( -- )
   JWT-OPEN-A
   JWT-FILL-N 0 ?do s" a" WRITE-RAW loop
   drop ;

: JWT-TEST-SELF-RAW ( -- )
   JWT-FILL-A
   JWT-A WRITTEN$ {: a:ptr u:n :}
   JWT-A a u WRITE-RAW drop
   JWT-A WRITTEN$ {: b:ptr v:n :}
   v JWT-FILL-N 2 * T=
   b c@ 97 T=
   b JWT-FILL-N + c@ 97 T=
   b v 1 - + c@ 97 T= ;

: JWT-TEST-SELF-STRING ( -- )
   JWT-FILL-A
   JWT-A WRITTEN$ {: a:ptr u:n :}
   JWT-A a u WRITE-STRING drop
   JWT-A WRITTEN$ {: b:ptr v:n :}
   v JWT-FILL-N 2 * 2 + T=
   b c@ 97 T=
   b JWT-FILL-N 1 - + c@ 97 T=
   b JWT-FILL-N + c@ JW-DQ T=
   b JWT-FILL-N 1+ + c@ 97 T=
   b v 1 - + c@ JW-DQ T= ;

\ A value that does not fit is refused by name and appends nothing. The escaped
\ cases leave room for their unescaped width, but not the escaped width.
: JWT-FULL-RAW ( -- )
   JWT-OPEN-SMALL s" abcde" WRITE-RAW s" fghij" WRITE-RAW drop ;

: JWT-FULL-STRING ( -- )
   JWT-OPEN-SMALL s" abcde" WRITE-RAW s" xy" WRITE-STRING drop ;

: JWT-FULL-STRING-ESC ( -- )
   JWT-OPEN-SMALL s" abcd" WRITE-RAW JWT-RUN-IN 2 WRITE-STRING drop ;

: JWT-FULL-KEY-ESC ( -- )
   JWT-OPEN-SMALL s" abc" WRITE-RAW JWT-RUN-IN 2 WRITE-KEY drop ;

: JWT-FULL-U ( -- )
   JWT-OPEN-SMALL s" abcde" WRITE-RAW 1000 WRITE-U drop ;

: JWT-FULL-KEY ( -- )
   JWT-OPEN-SMALL s" abcde" WRITE-RAW s" k" WRITE-KEY drop ;

: JWT-FULL-INT ( -- )
   JWT-OPEN-SMALL s" abcde" WRITE-RAW -123 WRITE-INT drop ;

: JWT-FULL-INT-MIN ( -- )
   JWT-OPEN-SMALL s" abcde" WRITE-RAW STR-MIN-I64 WRITE-INT drop ;

: JWT-ZERO-CAP ( -- )
   JWT-A JWT-BUF-A 0 WRITE-OPEN s" x" WRITE-RAW drop ;

: JWT-REFUSED-KEEPS$ ( [ -- ] ptr u8 n -- )   \ refusal left the document untouched
   {: want:ptr wu:n :}
   E-JSON-WRITE-CAPACITY TTHROWSQ
   JWT-A WRITTEN$ want wu T$= ;

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
   JWT-A WRITTEN$ nip 0 T= ;

: JWT-FIELD-S-BAD-VALUE ( -- )
   JWT-OPEN-SMALL s" ab" WRITE-RAW s" k" s" v" drop -1 WRITE-FIELD-S drop ;

: JWT-FIELD-RAW-BAD-VALUE ( -- )
   JWT-OPEN-SMALL s" ab" WRITE-RAW s" k" s" v" drop -1 WRITE-FIELD-RAW drop ;

: JWT-FIELD-S-FULL-KEY ( -- )
   JWT-OPEN-SMALL s" ab" WRITE-RAW s" longkey" s" v" drop -1 WRITE-FIELD-S drop ;

: JWT-FIELD-RAW-FULL-KEY ( -- )
   JWT-OPEN-SMALL s" ab" WRITE-RAW s" longkey" s" v" drop -1 WRITE-FIELD-RAW drop ;

: JWT-TEST-FIELD-REFUSAL ( -- )
   [: JWT-FIELD-S-BAD-VALUE ;] E-JSON-WRITE-SOURCE TTHROWSQ
   JWT-A WRITTEN$ s\" ab\"k\":" T$=
   [: JWT-FIELD-RAW-BAD-VALUE ;] E-JSON-WRITE-SOURCE TTHROWSQ
   JWT-A WRITTEN$ s\" ab\"k\":" T$=
   [: JWT-FIELD-S-FULL-KEY ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   JWT-A WRITTEN$ s" ab" T$=
   [: JWT-FIELD-RAW-FULL-KEY ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   JWT-A WRITTEN$ s" ab" T$= ;

\ A writer that was never opened and one that was closed are both refused by
\ name; neither reads or writes the caller's bytes.
: JWT-FRESH-RAW ( -- )
   JWT-FRESH s" x" WRITE-RAW drop ;

: JWT-FRESH-$ ( -- )
   JWT-FRESH WRITTEN$ 2drop ;

: JWT-FRESH-FIELD-S ( -- )
   JWT-FRESH s" k" s" v" drop -1 WRITE-FIELD-S drop ;

: JWT-FRESH-FIELD-RAW ( -- )
   JWT-FRESH s" k" s" v" drop -1 WRITE-FIELD-RAW drop ;

: JWT-CLOSED-FIELD-S-NEG-KEY ( -- )
   JWT-OPEN-B WRITE-CLOSE
   JWT-B s" k" drop -1 s" v" WRITE-FIELD-S drop ;

: JWT-CLOSED-FIELD-RAW-NEG-KEY ( -- )
   JWT-OPEN-B WRITE-CLOSE
   JWT-B s" k" drop -1 s" v" WRITE-FIELD-RAW drop ;

: JWT-CLOSED-RAW ( -- )
   JWT-OPEN-B WRITE-CLOSE
   JWT-B s" x" WRITE-RAW drop ;

: JWT-CLOSED-TWICE ( -- )
   JWT-OPEN-B WRITE-CLOSE
   JWT-B WRITE-CLOSE ;

: JWT-NULL-OUT ( -- ptr u8 )   \ the zero image's null output pointer
   JWT-FRESH @ JSON-WRITER:UNMAKE 2drop 2drop 2drop ;

: JWT-CLOSED-FIELD-S-NULL-KEY ( -- )
   JWT-OPEN-B WRITE-CLOSE
   JWT-B JWT-NULL-OUT 1 s" v" WRITE-FIELD-S drop ;

: JWT-CLOSED-FIELD-RAW-NULL-KEY ( -- )
   JWT-OPEN-B WRITE-CLOSE
   JWT-B JWT-NULL-OUT 1 s" v" WRITE-FIELD-RAW drop ;

: JWT-OPEN-NULL ( -- )
   JWT-A JWT-NULL-OUT JWT-CAP WRITE-OPEN drop ;

: JWT-OPEN-NEG ( -- )
   JWT-A JWT-BUF-A -1 WRITE-OPEN drop ;

: JWT-NULL-SOURCE ( -- )
   JWT-OPEN-A JWT-NULL-OUT 3 WRITE-RAW drop ;

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
   JWT-OPEN-B s" ok" WRITE-RAW WRITTEN$ s" ok" T$= ;

: JWT-RAW-NEG ( -- )
   JWT-OPEN-A s" x" drop -1 WRITE-RAW drop ;

: JWT-STRING-NEG ( -- )
   JWT-OPEN-A s" x" drop -1 WRITE-STRING drop ;

: JWT-C-NEG ( -- )
   JWT-OPEN-A -1 JW-C drop ;

: JWT-C-HIGH ( -- )
   JWT-OPEN-A 256 JW-C drop ;

: JWT-U-NEG ( -- )
   JWT-OPEN-A -1 WRITE-U drop ;

: JWT-STRING-MIN ( -- )
   JWT-OPEN-A s" x" drop MEM-MAX-N negate 1- WRITE-STRING drop ;

\ Every source byte takes at least one byte of output, so text longer than the
\ room is refused before a byte of it is read: these bytes end at an
\ inaccessible page, and the claim runs one past them.
: JWT-STRING-UNREAD ( n -- ) {: u:n :}
   JWT-OPEN-SMALL JWT-SMALL-CAP [char] a GUARD-PAGE:TAIL u WRITE-STRING drop ;

: JWT-KEY-UNREAD ( n -- ) {: u:n :}
   JWT-OPEN-SMALL JWT-SMALL-CAP [char] a GUARD-PAGE:TAIL u WRITE-KEY drop ;

: JWT-TEST-ERRORS ( -- )
   [: JWT-RAW-NEG ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-STRING-NEG ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-STRING-MIN ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-C-NEG ;] E-JSON-WRITE-BYTE TTHROWSQ
   [: JWT-C-HIGH ;] E-JSON-WRITE-BYTE TTHROWSQ
   [: JWT-U-NEG ;] E-JSON-WRITE-BYTE TTHROWSQ
   [: JWT-SMALL-CAP 1+ JWT-STRING-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   [: MEM-MAX-N JWT-STRING-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   [: JWT-SMALL-CAP 1+ JWT-KEY-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   [: MEM-MAX-N JWT-KEY-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ ;

\ WRITE-INT writes every cell: zero, both ends of the range, and negatives such
\ as JSON-RPC's error codes, each compared with its exact decimal text.
: JWT-INT$ ( n -- ptr u8 n )
   {: v:n :}
   JWT-OPEN-A v WRITE-INT WRITTEN$ ;

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
   JWT-OPEN-SMALL s" abcde" WRITE-RAW -12 WRITE-INT WRITTEN$ s" abcde-12" T$=
   JWT-A JWT-BUF-A 20 WRITE-OPEN STR-MIN-I64 WRITE-INT WRITTEN$ s" -9223372036854775808" T$= ;

: JWT-TEST-FIELD-INT ( -- )
   JWT-OPEN-A WRITE-OBJECT-START s" code" -32601 WRITE-FIELD-INT WRITE-OBJECT-END
   WRITTEN$ s\" {\"code\":-32601}" T$= ;

\ One random source string: plain ASCII, a control byte, a quote, a backslash,
\ or a two-byte UTF-8 scalar, so every escape width is exercised.
: JWT-RT-BYTE+ ( -- )
   8 PROP:RND% {: pick:n :}
   pick 0 = if 32 PROP:RND% PROP:BUF-C+ exit then
   pick 1 = if JW-DQ PROP:BUF-C+ exit then
   pick 2 = if JW-BACKSLASH PROP:BUF-C+ exit then
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
   JWT-RT-W JWT-RT-OUT JWT-RT-CAP WRITE-OPEN a u WRITE-STRING WRITTEN$ ;

: JWT-RT-DECODE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   JWT-RT-READER a u READ-INIT
   READ-NEXT T-STR T=
   JWT-RT-DECODE JWT-RT-CAP READ-STR {: got:n :}
   READ-CLOSE
   JWT-RT-DECODE got ;

: JWT-RT-ONE ( -- )
   JWT-RT-CASE$ {: a:ptr u:n :}
   a u JWT-RT-ENCODE {: e:ptr eu:n :}
   eu a u JW-ESC-N JW-QUOTE-N + T=   \ the reserved width is the emitted width
   e eu JWT-RT-DECODE$ a u T$= ;

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

: JWT-OPEN-AB ( -- ptr writer )
   JWT-OPEN-A s" ab" WRITE-RAW ;

: JWT-BAD-FFFE ( -- ) JWT-OPEN-AB JWT-FFFE 2 WRITE-STRING drop ;
: JWT-BAD-CONT ( -- ) JWT-OPEN-AB JWT-LONE-CONT 1 WRITE-STRING drop ;
: JWT-BAD-TRUNCATED ( -- ) JWT-OPEN-AB JWT-TRUNCATED 2 WRITE-STRING drop ;
: JWT-BAD-OVERLONG ( -- ) JWT-OPEN-AB JWT-OVERLONG 2 WRITE-STRING drop ;
: JWT-BAD-SURROGATE ( -- ) JWT-OPEN-AB JWT-SURROGATE 3 WRITE-STRING drop ;
: JWT-BAD-KEY ( -- ) JWT-OPEN-AB JWT-FFFE 1 WRITE-KEY drop ;

: JWT-BYTE-REFUSED ( [ -- ] -- )
   E-JSON-WRITE-BYTE TTHROWSQ
   JWT-A WRITTEN$ s" ab" T$= ;

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
: JWT-NESTED ( ptr writer -- ptr writer )
   WRITE-OBJECT-START
   s" a" WRITE-KEY WRITE-OBJECT-START
   s" x" 1 WRITE-FIELD-U WRITE-COMMA s" y" JWT-TRUE WRITE-FIELD-BOOL
   WRITE-OBJECT-END WRITE-COMMA
   s" b" WRITE-KEY WRITE-ARRAY-START 2 WRITE-U WRITE-COMMA 3 WRITE-U WRITE-ARRAY-END
   WRITE-COMMA
   s" c" WRITE-KEY WRITE-OBJECT-START WRITE-OBJECT-END WRITE-COMMA
   s" d" WRITE-KEY WRITE-ARRAY-START WRITE-ARRAY-END
   WRITE-OBJECT-END ;

: JWT-INDENTED$ ( -- ptr u8 n )
   s\" {\n  \"a\": {\n    \"x\": 1,\n    \"y\": true\n  },\n  \"b\": [\n    2,\n    3\n  ],\n  \"c\": {},\n  \"d\": []\n}" ;

: JWT-TEST-INDENTED ( -- )
   JWT-OPEN-A 2 WRITE-INDENT! JWT-NESTED WRITTEN$ JWT-INDENTED$ T$=
   JWT-GROW-OPEN 2 WRITE-INDENT! JWT-NESTED WRITTEN$ JWT-INDENTED$ T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

\ Opening a writer again makes it compact, whatever width it had.
: JWT-COMPACT$ ( -- ptr u8 n )
   s\" {\"a\":{\"x\":1,\"y\":true},\"b\":[2,3],\"c\":{},\"d\":[]}" ;

: JWT-TEST-COMPACT ( -- )
   JWT-OPEN-A 2 WRITE-INDENT! drop
   JWT-OPEN-A JWT-NESTED WRITTEN$ JWT-COMPACT$ T$= ;

: JWT-TEST-OPEN-BUF-COMPACT ( -- )
   JWT-GROW-OPEN 2 WRITE-INDENT! drop
   JWT-GROW-W JWT-GROW-BUF WRITE-OPEN-BUF JWT-NESTED WRITTEN$ JWT-COMPACT$ T$=
   JWT-GROW-W WRITE-CLOSE JWT-GROW-BUF BUF:DISPOSE ;

\ A string in parts never breaks, even after a part ending in a comma and RAW
\ inside it; the COMMA after it puts the number on its own line.
: JWT-TEST-PART-LAYOUT ( -- )
   JWT-OPEN-A 2 WRITE-INDENT! WRITE-ARRAY-START
   WRITE-STRING-START s" a," WRITE-STRING-PART s" x" WRITE-RAW
   s" b" WRITE-STRING-PART WRITE-STRING-END
   WRITE-COMMA 1 WRITE-U WRITE-ARRAY-END
   WRITTEN$ s\" [\n  \"a,xb\",\n  1\n]" T$= ;

\ WRITE-RESET right after a START keeps the width and forgets the open object
\ and its due break.
: JWT-TEST-INDENT-RESET ( -- )
   JWT-OPEN-A 2 WRITE-INDENT! WRITE-OBJECT-START WRITE-RESET
   JWT-NESTED WRITTEN$ JWT-INDENTED$ T$=
   [: JWT-A WRITE-OBJECT-END drop ;] E-JSON-WRITE-STATE TTHROWSQ
   JWT-A WRITTEN$ JWT-INDENTED$ T$= ;

\ a"b, LF, then é: the parts are its first three bytes, the next, the last two.
create JWT-PARTS 97 c, JW-DQ c, 98 c, JW-LF c, $C3 c, $A9 c,
create JWT-PARTS-WANT
   JW-DQ c, 97 c, JW-BACKSLASH c, JW-DQ c, 98 c, JW-BACKSLASH c, 110 c,
   $C3 c, $A9 c, JW-DQ c,

: JWT-TEST-PARTS ( -- )
   JWT-OPEN-A WRITE-STRING-START
   JWT-PARTS 3 WRITE-STRING-PART
   JWT-PARTS 3 + 1 WRITE-STRING-PART
   JWT-PARTS 4 + 2 WRITE-STRING-PART
   WRITE-STRING-END WRITTEN$ JWT-PARTS-WANT 10 T$=
   JWT-OPEN-B JWT-PARTS 6 WRITE-STRING WRITTEN$ JWT-PARTS-WANT 10 T$= ;

: JWT-PART-FF ( -- ) JWT-OPEN-AB JWT-FFFE 1 WRITE-STRING-PART drop ;
: JWT-PART-HALF ( -- ) JWT-OPEN-AB JWT-PARTS 4 + 1 WRITE-STRING-PART drop ;
: JWT-INDENT!-NEG ( -- ) JWT-OPEN-AB -1 WRITE-INDENT! drop ;
: JWT-PART-FULL ( -- )   \ room for the two raw bytes, not their three escaped
   JWT-OPEN-SMALL s" abcdef" WRITE-RAW JWT-RUN-IN 2 WRITE-STRING-PART drop ;
: JWT-KEY-FULL ( -- )   \ room for the break before the key, not for the key
   JWT-OPEN-SMALL 2 WRITE-INDENT! WRITE-OBJECT-START s" k" WRITE-KEY drop ;
: JWT-BREAK-WRAP ( -- )   \ two levels of MEM-MAX-N spaces overflow a cell
   JWT-OPEN-AB WRITE-ARRAY-START WRITE-ARRAY-START MEM-MAX-N WRITE-INDENT!
   1 WRITE-U drop ;
: JWT-OBJECT-END-TOP ( -- ) JWT-OPEN-AB WRITE-OBJECT-END drop ;
: JWT-ARRAY-END-TOP ( -- ) JWT-OPEN-AB WRITE-ARRAY-END drop ;

: JWT-STATE-REFUSED ( [ -- ] -- )
   E-JSON-WRITE-STATE TTHROWSQ
   JWT-A WRITTEN$ s" ab" T$= ;

: JWT-FRESH-INDENT! ( -- ) JWT-FRESH 2 WRITE-INDENT! drop ;
: JWT-FRESH-START ( -- ) JWT-FRESH WRITE-STRING-START drop ;
: JWT-FRESH-PART ( -- ) JWT-FRESH s" x" WRITE-STRING-PART drop ;
: JWT-FRESH-END ( -- ) JWT-FRESH WRITE-STRING-END drop ;

: JWT-CLOSED ( -- ptr writer ) JWT-OPEN-B WRITE-CLOSE JWT-B ;
: JWT-CLOSED-INDENT! ( -- ) JWT-CLOSED 2 WRITE-INDENT! drop ;
: JWT-CLOSED-START ( -- ) JWT-CLOSED WRITE-STRING-START drop ;
: JWT-CLOSED-PART ( -- ) JWT-CLOSED s" x" WRITE-STRING-PART drop ;
: JWT-CLOSED-END ( -- ) JWT-CLOSED WRITE-STRING-END drop ;

\ A refused key keeps its depth and pending break: the null that fits the same
\ eight bytes writes after the break.
: JWT-TEST-KEY-FULL ( -- )
   [: JWT-KEY-FULL ;] s" {" JWT-REFUSED-KEEPS$
   JWT-A WRITE-NULL WRITTEN$ s\" {\n  null" T$= ;

: JWT-TEST-LAYOUT-REFUSALS ( -- )
   [: JWT-PART-FF ;] JWT-BYTE-REFUSED
   [: JWT-PART-HALF ;] JWT-BYTE-REFUSED
   [: JWT-INDENT!-NEG ;] JWT-BYTE-REFUSED
   [: JWT-PART-FULL ;] s" abcdef" JWT-REFUSED-KEEPS$
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

\ The handle is nominal: a raw cell is not a writer.
: JWT-TEST-CHECKER ( -- )
   s" JWT-OK ( ptr writer -- ptr writer ) WRITE-OBJECT-START WRITE-OBJECT-END" JWT-CHECK-ACCEPTS
   s" JWT-GROW-OK ( ptr writer ptr n -- ptr writer ) WRITE-OPEN-BUF" JWT-CHECK-ACCEPTS
   s" JWT-GROW-BAD ( ptr writer ptr u8 -- ptr writer ) WRITE-OPEN-BUF" JWT-CHECK-REJECTS
   s" JWT-BAD-CELL ( n ptr u8 n -- n ) WRITE-RAW" JWT-CHECK-REJECTS
   s" JWT-BAD-BUF ( ptr u8 ptr u8 n -- ptr u8 ) WRITE-RAW" JWT-CHECK-REJECTS ;

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
   JWT-TEST-CHECKER
   T-REPORT
   s" json-write-test: ok" type cr ;

JWT-MAIN

;package
