\ json-write-test.f - focused tests for the caller-owned checked JSON writer.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/json-write.f
\ lib/json-write-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/property.f
require lib/json-write.f
require lib/json-read.f
require test/checker-assert.f

\ White-box test: reopen the module's package so the fixtures reach json-write's
\ private record and byte plumbing (JW-LIVE, JW-APPEND-LEN, the byte constants)
\ and call the public emitters by their bare package-local tails.
package JSON-WRITE

64 constant JWT-CAP
8 constant JWT-SMALL-CAP
20 constant JWT-FILL-N

JWT-CAP BUFFER: JWT-BUF-A
JWT-CAP BUFFER: JWT-BUF-B

TYPED-VARIABLE JWT-A writer
TYPED-VARIABLE JWT-B writer
TYPED-VARIABLE JWT-FRESH writer   \ never opened: keeps the definer's zero image

\ Round-trip fixture storage: the writer's bytes, the reader's decode buffer,
\ and the reader's own caller-owned state block.
$400 constant JWT-RT-CAP
$40 constant JWT-RT-MAX-N
JWT-RT-CAP BUFFER: JWT-RT-OUT
JWT-RT-CAP BUFFER: JWT-RT-DECODE
TYPED-VARIABLE JWT-RT-W writer
here CELL 1- and CELL swap - CELL 1- and allot
create JWT-RT-READER JR:STORAGE-BYTES allot

create JWT-ESC-IN
   65 c, JW-DQ c, JW-BACKSLASH c, JW-LF c, 1 c, 66 c,

create JWT-ESC-WANT
   JW-DQ c, 65 c, JW-BACKSLASH c, JW-DQ c, JW-BACKSLASH c, JW-BACKSLASH c,
   JW-BACKSLASH c, 110 c, JW-BACKSLASH c, 117 c, JW-ZERO c, JW-ZERO c,
   JW-ZERO c, 49 c, 66 c, JW-DQ c,

create JWT-NAME
   65 c, JW-DQ c, 66 c,

: JWT-ESC-IN$ ( -- ptr u8 n )
   JWT-ESC-IN 6 ;

: JWT-ESC-WANT$ ( -- ptr u8 n )
   JWT-ESC-WANT 16 ;

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
   JWT-A JWT-BUF-A JWT-CAP OPEN ;

: JWT-OPEN-B ( -- ptr writer )
   JWT-B JWT-BUF-B JWT-CAP OPEN ;

: JWT-OPEN-SMALL ( -- ptr writer )
   JWT-A JWT-BUF-A JWT-SMALL-CAP OPEN ;

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
   OBJECT-START
   s" name" JWT-NAME$ FIELD-S
   COMMA
   s" count" 42 FIELD-U
   COMMA
   s" ok" JWT-TRUE FIELD-BOOL
   COMMA
   s" none" FIELD-NULL
   OBJECT-END ;

: JWT-BUILD-ARRAY ( ptr writer -- ptr writer )
   ARRAY-START
   1 U
   COMMA
   s" x" STRING
   COMMA
   JWT-FALSE BOOL
   ARRAY-END ;

: JWT-TEST-STRING-ESCAPE ( -- )
   JWT-OPEN-A JWT-ESC-IN$ STRING
   $ JWT-ESC-WANT$ T$= ;

: JWT-TEST-OBJECT ( -- )
   JWT-OPEN-A JWT-BUILD-OBJECT
   $ JWT-EXPECTED-OBJECT$ T$= ;

: JWT-TEST-ARRAY ( -- )
   JWT-OPEN-A JWT-BUILD-ARRAY
   $ JWT-EXPECTED-ARRAY$ T$= ;

\ The point of caller-owned storage: two writers are alive at the same time over
\ different buffers, each document is interleaved with the other, and neither
\ sees the other's bytes.
: JWT-TEST-TWO-WRITERS ( -- )
   JWT-OPEN-A OBJECT-START s" count" 42 FIELD-U drop
   JWT-OPEN-B ARRAY-START 1 U COMMA drop
   JWT-A COMMA s" ok" JWT-TRUE FIELD-BOOL OBJECT-END drop
   JWT-B 2 U ARRAY-END drop
   JWT-A $ {: a:ptr au:n :}
   JWT-B $ {: b:ptr bu:n :}
   a au s\" {\"count\":42,\"ok\":true}" T$=
   b bu s" [1,2]" T$=
   a b <> TTRUE ;

: JWT-TEST-RECORD ( -- )
   JWT-OPEN-A s" ab" RAW drop
   JWT-A JW-LIVE {: vp:ptr cap:n used:n :}
   cap JWT-CAP T=
   used 2 T=
   vp JWT-BUF-A = TTRUE
   vp c@ 97 T= ;

: JWT-TEST-RESET ( -- )
   JWT-OPEN-A s" abc" RAW drop
   JWT-A RESET drop
   JWT-A $ nip 0 T=
   JWT-A s" z" RAW $ s" z" T$= ;

: JWT-FILL-A ( -- )
   JWT-OPEN-A
   JWT-FILL-N 0 ?do s" a" RAW loop
   drop ;

: JWT-TEST-SELF-RAW ( -- )
   JWT-FILL-A
   JWT-A $ {: a:ptr u:n :}
   JWT-A a u RAW drop
   JWT-A $ {: b:ptr v:n :}
   v JWT-FILL-N 2 * T=
   b c@ 97 T=
   b JWT-FILL-N + c@ 97 T=
   b v 1 - + c@ 97 T= ;

: JWT-TEST-SELF-STRING ( -- )
   JWT-FILL-A
   JWT-A $ {: a:ptr u:n :}
   JWT-A a u STRING drop
   JWT-A $ {: b:ptr v:n :}
   v JWT-FILL-N 2 * 2 + T=
   b c@ 97 T=
   b JWT-FILL-N 1 - + c@ 97 T=
   b JWT-FILL-N + c@ JW-DQ T=
   b JWT-FILL-N 1+ + c@ 97 T=
   b v 1 - + c@ JW-DQ T= ;

\ A value that does not fit is refused by name and appends nothing: each of the
\ three emitters below is tried against a buffer with three bytes left.
: JWT-FULL-RAW ( -- )
   JWT-OPEN-SMALL s" abcde" RAW s" fghij" RAW drop ;

: JWT-FULL-STRING ( -- )
   JWT-OPEN-SMALL s" abcde" RAW s" xy" STRING drop ;

: JWT-FULL-U ( -- )
   JWT-OPEN-SMALL s" abcde" RAW 1000 U drop ;

: JWT-FULL-KEY ( -- )
   JWT-OPEN-SMALL s" abcde" RAW s" k" KEY drop ;

: JWT-ZERO-CAP ( -- )
   JWT-A JWT-BUF-A 0 OPEN s" x" RAW drop ;

: JWT-REFUSED-KEEPS$ ( [ -- ] -- )   \ the refusal left the document untouched
   E-JW-CAPACITY TTHROWSQ
   JWT-A $ s" abcde" T$= ;

: JWT-TEST-FULL ( -- )
   [: JWT-FULL-RAW ;] JWT-REFUSED-KEEPS$
   [: JWT-FULL-STRING ;] JWT-REFUSED-KEEPS$
   [: JWT-FULL-U ;] JWT-REFUSED-KEEPS$
   [: JWT-FULL-KEY ;] JWT-REFUSED-KEEPS$
   [: JWT-ZERO-CAP ;] E-JW-CAPACITY TTHROWSQ
   JWT-A $ nip 0 T= ;

\ A writer that was never opened and one that was closed are both refused by
\ name; neither reads or writes the caller's bytes.
: JWT-FRESH-RAW ( -- )
   JWT-FRESH s" x" RAW drop ;

: JWT-FRESH-$ ( -- )
   JWT-FRESH $ 2drop ;

: JWT-CLOSED-RAW ( -- )
   JWT-OPEN-B CLOSE
   JWT-B s" x" RAW drop ;

: JWT-CLOSED-TWICE ( -- )
   JWT-OPEN-B CLOSE
   JWT-B CLOSE ;

: JWT-NULL-OUT ( -- ptr u8 )   \ the zero image's null output pointer
   JWT-FRESH @ JSON--WRITE-WRITER:UNMAKE 2drop ;

: JWT-OPEN-NULL ( -- )
   JWT-A JWT-NULL-OUT JWT-CAP OPEN drop ;

: JWT-OPEN-NEG ( -- )
   JWT-A JWT-BUF-A -1 OPEN drop ;

: JWT-NULL-SOURCE ( -- )
   JWT-OPEN-A JWT-NULL-OUT 3 RAW drop ;

: JWT-TEST-STATE ( -- )
   [: JWT-FRESH-RAW ;] E-JW-STATE TTHROWSQ
   [: JWT-FRESH-$ ;] E-JW-STATE TTHROWSQ
   [: JWT-CLOSED-RAW ;] E-JW-STATE TTHROWSQ
   [: JWT-CLOSED-TWICE ;] E-JW-STATE TTHROWSQ
   [: JWT-OPEN-NULL ;] E-JW-OUTPUT TTHROWSQ
   [: JWT-OPEN-NEG ;] E-JW-OUTPUT TTHROWSQ
   [: JWT-NULL-SOURCE ;] E-JW-SOURCE TTHROWSQ
   JWT-OPEN-B s" ok" RAW $ s" ok" T$= ;

: JWT-RAW-NEG ( -- )
   JWT-OPEN-A s" x" drop -1 RAW drop ;

: JWT-STRING-NEG ( -- )
   JWT-OPEN-A s" x" drop -1 STRING drop ;

: JWT-C-NEG ( -- )
   JWT-OPEN-A -1 JW-C drop ;

: JWT-C-HIGH ( -- )
   JWT-OPEN-A 256 JW-C drop ;

: JWT-U-NEG ( -- )
   JWT-OPEN-A -1 U drop ;

: JWT-TEST-ERRORS ( -- )
   [: JWT-RAW-NEG ;] E-JW-SOURCE TTHROWSQ
   [: JWT-STRING-NEG ;] E-JW-SOURCE TTHROWSQ
   [: JWT-C-NEG ;] E-JW-BYTE TTHROWSQ
   [: JWT-C-HIGH ;] E-JW-BYTE TTHROWSQ
   [: JWT-U-NEG ;] E-JW-BYTE TTHROWSQ ;

\ PROP:RND% takes an LCG's low bits, which cycle in step with the draws: with a
\ power-of-two bound a fixed draw pattern locks onto a few residues and never
\ generates whole byte classes (measured: no control byte and no quote in 2048
\ cases). Draw from the high bits instead.
: JWT-RND% ( n -- n ) {: bound:n :}
   PROP:RND 8 rshift bound mod ;

\ One random source string: plain ASCII, a control byte, a quote, a backslash,
\ or a two-byte UTF-8 scalar, so every escape width is exercised.
: JWT-RT-BYTE+ ( -- )
   8 JWT-RND% {: pick:n :}
   pick 0 = if 32 JWT-RND% PROP:BUF-C+ exit then
   pick 1 = if JW-DQ PROP:BUF-C+ exit then
   pick 2 = if JW-BACKSLASH PROP:BUF-C+ exit then
   pick 3 = if
      30 JWT-RND% $C2 + PROP:BUF-C+
      64 JWT-RND% $80 + PROP:BUF-C+ exit
   then
   95 JWT-RND% 32 + PROP:BUF-C+ ;

: JWT-RT-CASE$ ( -- ptr u8 n )
   PROP:BUF-RESET
   JWT-RT-MAX-N JWT-RND% 0 ?do JWT-RT-BYTE+ loop
   PROP:BUF$ ;

: JWT-RT-ENCODE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   JWT-RT-W JWT-RT-OUT JWT-RT-CAP OPEN a u STRING $ ;

: JWT-RT-DECODE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   JWT-RT-READER JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT JR:T-STR T=
   JWT-RT-DECODE JWT-RT-CAP JR:STR {: got:n :}
   JR:CLOSE
   JWT-RT-DECODE got ;

: JWT-RT-ONE ( -- )
   JWT-RT-CASE$ {: a:ptr u:n :}
   a u JWT-RT-ENCODE {: e:ptr eu:n :}
   eu a u JW-STR-N T=                 \ the reserved width is the emitted width
   e eu JWT-RT-DECODE$ a u T$= ;

\ 2048 random strings: what STRING reserves is what it writes, the output is a
\ well-formed JSON string to the strict reader, and it decodes back unchanged.
: JWT-TEST-ROUNDTRIP ( -- )
   $517A32D PROP:SEED!
   2048 0 ?do JWT-RT-ONE loop ;

\ The handle is nominal: a raw cell is not a writer, and the length role is not
\ an offset role.
: JWT-TEST-CHECKER ( -- )
   s" JWT-OK ( ptr writer -- ptr writer ) OBJECT-START OBJECT-END" JWT-CHECK-ACCEPTS
   s" JWT-BAD-CELL ( n ptr u8 n -- n ) RAW" JWT-CHECK-REJECTS
   s" JWT-BAD-BUF ( ptr u8 ptr u8 n -- ptr u8 ) RAW" JWT-CHECK-REJECTS
   s" JWT-BAD-LEN ( ptr writer ptr u8 off -- ptr writer ) JW-APPEND-LEN"
      JWT-CHECK-REJECTS ;

: JWT-MAIN ( -- )
   T-RESET
   JWT-TEST-STRING-ESCAPE
   JWT-TEST-OBJECT
   JWT-TEST-ARRAY
   JWT-TEST-TWO-WRITERS
   JWT-TEST-RECORD
   JWT-TEST-RESET
   JWT-TEST-SELF-RAW
   JWT-TEST-SELF-STRING
   JWT-TEST-FULL
   JWT-TEST-STATE
   JWT-TEST-ERRORS
   JWT-TEST-ROUNDTRIP
   JWT-TEST-CHECKER
   T-REPORT
   s" json-write-test: ok" type cr ;

JWT-MAIN

;package
