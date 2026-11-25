\ json-write-test.f - focused tests for the caller-owned checked JSON writer.
\ Run: bin/hb --load src/lib/json-write-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/property.f
require src/lib/json.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/memory.f
require src/lib/test/guard-page.f
require src/lib/json/write.f
require src/lib/json/read.f

package JSON-TEST

10 constant JWT-LF
8 constant JWT-TAIL-N   \ the bytes before the guard page
20 constant JWT-FILL-N

\ JWT-MAIN makes the outputs of JWT-A, JWT-B and JWT-RT-W and deletes them;
\ each JWT-GROW-OPEN makes JWT-GROW-W's, which JWT-GROW-DONE deletes.
TYPED-VARIABLE JWT-OUT-A ^VEC:vector<u8>
TYPED-VARIABLE JWT-OUT-B ^VEC:vector<u8>
TYPED-VARIABLE JWT-A JSON:writer
TYPED-VARIABLE JWT-B JSON:writer
TYPED-VARIABLE JWT-FRESH JSON:writer   \ never opened: keeps the definer's zero image

TYPED-VARIABLE JWT-GROW-OUT ^VEC:vector<u8>
TYPED-VARIABLE JWT-GROW-W JSON:writer
TYPED-VARIABLE JWT-GROW-BASE ^u8   \ the output's block when JWT-GROW-OPEN made it
TYPED-VARIABLE JWT-MOVER ALLOC:allocator
1024 BUFFER: JWT-LARGE-CHUNK

\ Round-trip fixture storage: the writer's output, the reader's decode buffer,
\ and the reader's own caller-owned state block.
$40 constant JWT-RT-MAX-N
TYPED-VARIABLE JWT-RT-OUT ^VEC:vector<u8>
TYPED-VARIABLE JWT-RT-DECODE ^VEC:vector<u8>
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

: JWT-ESC-IN$ ( -- str )
   JWT-ESC-IN 7 >str ;

: JWT-ESC-WANT$ ( -- str )
   JWT-ESC-WANT 22 >str ;

: JWT-NAME$ ( -- str )
   JWT-NAME 3 >str ;

: JWT-TRUE ( -- bool )
   0 0= ;

: JWT-FALSE ( -- bool )
   JWT-TRUE 0= ;

: JWT-CHECK-REJECTS ( str -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: JWT-CHECK-ACCEPTS ( str -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: JWT-NEW ( -- ^VEC:vector<u8> )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

: JWT-OPEN-A ( -- ^JSON:writer )
   JWT-A JWT-OUT-A @ JSON:OPEN ;

: JWT-OPEN-B ( -- ^JSON:writer )
   JWT-B JWT-OUT-B @ JSON:OPEN ;

: JWT-EXPECTED-OBJECT$ ( -- str )
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

: JWT-EXPECTED-ARRAY$ ( -- str )
   SB-RESET
   [char] [ SB-APPEND-C
   s" 1" SB-APPEND
   [char] , SB-APPEND-C
   [char] " SB-APPEND-C 120 SB-APPEND-C [char] " SB-APPEND-C
   [char] , SB-APPEND-C
   s" false" SB-APPEND
   [char] ] SB-APPEND-C
   SB$ ;

: JWT-BUILD-OBJECT ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" name" JWT-NAME$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" count" 42 JSON:WRITE-FIELD-U
   JSON:WRITE-COMMA
   s" ok" JWT-TRUE JSON:WRITE-FIELD-BOOL
   JSON:WRITE-COMMA
   s" none" JSON:WRITE-FIELD-NULL
   JSON:WRITE-OBJECT-END ;

: JWT-BUILD-ARRAY ( ^JSON:writer -- ^JSON:writer )
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
\ different vectors, each document is interleaved with the other, and neither
\ sees the other's bytes.
: JWT-TEST-TWO-WRITERS ( -- )
   JWT-OPEN-A JSON:WRITE-OBJECT-START s" count" 42 JSON:WRITE-FIELD-U drop
   JWT-OPEN-B JSON:WRITE-ARRAY-START 1 JSON:WRITE-U JSON:WRITE-COMMA drop
   JWT-A JSON:WRITE-COMMA s" ok" JWT-TRUE JSON:WRITE-FIELD-BOOL JSON:WRITE-OBJECT-END drop
   JWT-B 2 JSON:WRITE-U JSON:WRITE-ARRAY-END drop
   JWT-A JSON:WRITTEN$ {: a:str :}
   JWT-B JSON:WRITTEN$ {: b:str :}
   a s\" {\"count\":42,\"ok\":true}" T$=
   b s" [1,2]" T$=
   a STR:DATA b STR:DATA <> TTRUE ;

: JWT-TEST-RESET ( -- )
   JWT-OPEN-A s" abc" JSON:WRITE-RAW drop
   JWT-A JSON:RESET drop
   JWT-A JSON:WRITTEN$ STR:LEN 0 T=
   JWT-A s" z" JSON:WRITE-RAW JSON:WRITTEN$ s" z" T$= ;

\ JWT-MOVER: an allocator whose every resize moves the block to a new heap
\ block and fills the old one with $EE before freeing it.
: JWT-MOVE-ALLOC ( ^u8 n -- ^u8 )
   {: st:^ len:n :}
   HEAP:ALLOCATOR len ALLOC:BYTES drop ;

: JWT-MOVE-RESIZE ( ^u8 ^u8 n n -- ^u8 )
   {: st:^ block:^ old:n new:n :}
   HEAP:ALLOCATOR new ALLOC:BYTES drop {: moved:^ :}
   block moved old new min BYTE-COPY
   $EE block old SPAN:MAKE SPAN:FILL
   HEAP:ALLOCATOR block ALLOC:FREE
   moved ;

: JWT-MOVE-FREE ( ^u8 ^u8 -- )
   {: st:^ p:^ :}
   HEAP:ALLOCATOR p ALLOC:FREE ;

: JWT-MOVER-FILL ( -- )
   NULL-PTR [: JWT-MOVE-ALLOC ;] [: JWT-MOVE-RESIZE ;] [: JWT-MOVE-FREE ;]
   JWT-MOVER ALLOC:INIT ;

: JWT-GROW-AT ( -- ^u8 ) JWT-GROW-OUT @ VEC:BYTES STR:DATA ;

\ A vector on JWT-MOVER with room for eight bytes, so the cases outgrow it
\ and each growth moves the block.
: JWT-GROW-OPEN ( -- ^JSON:writer )
   JWT-MOVER 0 [type] u8 VEC:NEW {: v:^ :}
   v 8 VEC:RESERVE
   v JWT-GROW-OUT !
   JWT-GROW-AT JWT-GROW-BASE !
   JWT-GROW-W v JSON:OPEN ;

\ The output's block is not the one JWT-GROW-OPEN made.
: JWT-GROW-MOVED ( -- ) JWT-GROW-AT JWT-GROW-BASE @ <> TTRUE ;

: JWT-GROW-DONE ( -- )
   JWT-GROW-W JSON:CLOSE
   JWT-GROW-OUT @ VEC:DELETE ;

: JWT-TEST-GROW-LIFECYCLE ( -- )
   JWT-NEW {: v:^ :}
   s" stale" v VEC:APPEND
   JWT-GROW-W v JSON:OPEN JSON:WRITTEN$ STR:LEN 0 T=
   JWT-GROW-W s" one" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:RESET JSON:WRITTEN$ STR:LEN 0 T=
   JWT-GROW-W s" two" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:CLOSE
   v VEC:BYTES s" two" T$=
   v VEC:DELETE
   [: JWT-GROW-W JSON:WRITTEN$ drop ;] E-JSON-WRITE-STATE TTHROWSQ ;

: JWT-TEST-GROW-ESCAPE ( -- )
   JWT-GROW-OPEN JWT-ESC-IN$ JSON:WRITE-STRING
   JSON:WRITTEN$ JWT-ESC-WANT$ T$=
   JWT-GROW-W JSON:RESET
   s" é" JSON:WRITE-STRING JSON:WRITTEN$ s\" \"é\"" T$=
   JWT-GROW-DONE ;

: JWT-TEST-GROW-OBJECT ( -- )
   JWT-GROW-OPEN JWT-BUILD-OBJECT
   JSON:WRITTEN$ JWT-EXPECTED-OBJECT$ T$=
   JWT-GROW-DONE ;

: JWT-TEST-GROW-RAW-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:str :}
   JWT-GROW-W a STR:DATA 1+ 5 >str JSON:WRITE-RAW
   JSON:WRITTEN$ s" abcdefbcdef" T$=
   JWT-GROW-MOVED JWT-GROW-DONE ;

: JWT-TEST-GROW-STRING-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:str :}
   JWT-GROW-W a STR:DATA 1+ 5 >str JSON:WRITE-STRING
   JSON:WRITTEN$ s\" abcdef\"bcdef\"" T$=
   JWT-GROW-MOVED JWT-GROW-DONE ;

: JWT-TEST-GROW-FIELD-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:str :}
   JWT-GROW-W a STR:DATA 3 >str a STR:DATA 3 + 3 >str
   JSON:WRITE-FIELD-S
   JSON:WRITTEN$ s\" abcdef\"abc\":\"def\"" T$=
   JWT-GROW-MOVED JWT-GROW-DONE ;

: JWT-TEST-GROW-KEY-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:str :}
   JWT-GROW-W a JSON:WRITE-KEY JSON:WRITTEN$ s\" abcdef\"abcdef\":" T$=
   JWT-GROW-MOVED JWT-GROW-DONE ;

: JWT-TEST-GROW-ESC-ALIAS ( -- )
   JWT-GROW-OPEN JWT-RUN-IN 7 >str JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:str :}
   JWT-GROW-W a JSON:WRITE-STRING JSON:WRITTEN$
   JWT-RUN-KEY-WANT 18 >str T$=
   JWT-GROW-MOVED JWT-GROW-DONE
   JWT-GROW-OPEN JWT-RUN-IN 7 >str JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: key:str :}
   JWT-GROW-W key JSON:WRITE-KEY JSON:WRITTEN$
   JWT-RUN-KEY-WANT 19 >str T$=
   JWT-GROW-MOVED JWT-GROW-DONE ;

: JWT-TEST-GROW-FIELD-RAW-ALIAS ( -- )
   JWT-GROW-OPEN s" abcdef" JSON:WRITE-RAW drop
   JWT-GROW-W JSON:WRITTEN$ {: a:str :}
   JWT-GROW-W a STR:DATA 3 >str a STR:DATA 3 + 3 >str
   JSON:WRITE-FIELD-RAW
   JSON:WRITTEN$ s\" abcdef\"abc\":def" T$=
   JWT-GROW-MOVED JWT-GROW-DONE ;

: JWT-TEST-GROW-LARGE ( -- )
   1024 0 ?do 65 JWT-LARGE-CHUNK i + c! loop
   JWT-GROW-OPEN JSON:WRITE-STRING-START
   1025 0 ?do JWT-LARGE-CHUNK 1024 >str JSON:WRITE-STRING-PART loop
   JSON:WRITE-STRING-END
   JSON:WRITTEN$ {: s:str :}
   s STR:LEN 1025 1024 * 2 + T=
   s STR:DATA c@ [char] " T=
   s STR:DATA 1+ c@ 65 T=
   s STR:DATA s STR:LEN 2 - + c@ 65 T=
   s STR:DATA s STR:LEN 1- + c@ [char] " T=
   JWT-GROW-DONE ;

\ Every ASCII byte value escaped, as a string and as a key, alone and after a
\ prefix in a vector it outgrows: the second output holds the first after its
\ prefix.
$80 constant JWT-EVERY-N

: JWT-EVERY-EMIT ( ^JSON:writer bool -- ^JSON:writer ) {: key:bool :}
   JWT-LARGE-CHUNK JWT-EVERY-N >str
   key if JSON:WRITE-KEY else JSON:WRITE-STRING then ;

: JWT-EVERY ( bool -- ) {: key:bool :}
   JWT-EVERY-N 0 ?do i JWT-LARGE-CHUNK i + c! loop
   JWT-OPEN-A key JWT-EVERY-EMIT JSON:WRITTEN$ {: f:str :}
   JWT-GROW-OPEN s" pre" JSON:WRITE-RAW key JWT-EVERY-EMIT
   JSON:WRITTEN$ {: g:str :}
   g STR:LEN f STR:LEN 3 + T=
   g STR:DATA 3 >str s" pre" T$=
   g STR:DATA 3 + g STR:LEN 3 - >str f T$=
   JWT-GROW-DONE ;

: JWT-TEST-GROW-EVERY-BYTE ( -- )
   JWT-FALSE JWT-EVERY
   JWT-TRUE JWT-EVERY ;

: JWT-TEST-GROW-ROOM ( -- )
   JWT-GROW-OPEN s" abcdefgh" JSON:WRITE-RAW s" i" JSON:WRITE-RAW
   JSON:WRITTEN$ s" abcdefghi" T$=
   JWT-GROW-OUT @ VEC:CAP 16 T=
   JWT-GROW-DONE ;

\ CLOSE after the caller deleted the vector; the writer stays closed.
: JWT-TEST-GROW-DISPOSED-CLOSE ( -- )
   JWT-GROW-OPEN s" x" JSON:WRITE-RAW drop
   JWT-GROW-OUT @ VEC:DELETE
   JWT-GROW-W JSON:CLOSE
   JWT-NEW JWT-GROW-OUT !
   [: JWT-GROW-W s" y" JSON:WRITE-RAW drop ;] E-JSON-WRITE-STATE TTHROWSQ
   [: JWT-GROW-W JSON:CLOSE ;] E-JSON-WRITE-STATE TTHROWSQ
   JWT-GROW-OUT @ VEC:DELETE ;

: JWT-FILL-A ( -- )
   JWT-OPEN-A
   JWT-FILL-N 0 ?do s" a" JSON:WRITE-RAW loop
   drop ;

: JWT-TEST-SELF-RAW ( -- )
   JWT-FILL-A
   JWT-A JSON:WRITTEN$ {: a:str :}
   JWT-A a JSON:WRITE-RAW drop
   JWT-A JSON:WRITTEN$ {: b:str :}
   b STR:LEN JWT-FILL-N 2 * T=
   b STR:DATA c@ 97 T=
   b STR:DATA JWT-FILL-N + c@ 97 T=
   b STR:DATA b STR:LEN 1 - + c@ 97 T= ;

: JWT-TEST-SELF-STRING ( -- )
   JWT-FILL-A
   JWT-A JSON:WRITTEN$ {: a:str :}
   JWT-A a JSON:WRITE-STRING drop
   JWT-A JSON:WRITTEN$ {: b:str :}
   b STR:LEN JWT-FILL-N 2 * 2 + T=
   b STR:DATA c@ 97 T=
   b STR:DATA JWT-FILL-N 1 - + c@ 97 T=
   b STR:DATA JWT-FILL-N + c@ [char] " T=
   b STR:DATA JWT-FILL-N 1+ + c@ 97 T=
   b STR:DATA b STR:LEN 1 - + c@ [char] " T= ;

: JWT-REJECTED-KEEPS$ ( [ -- ] str -- )   \ rejection left the document untouched
   {: want:str :}
   E-JSON-WRITE-CAPACITY TTHROWSQ
   JWT-A JSON:WRITTEN$ want T$= ;

: JWT-FIELD-S-BAD-VALUE ( -- )
   JWT-OPEN-A s" ab" JSON:WRITE-RAW
   s" k" s" v" STR:DATA -1 >str JSON:WRITE-FIELD-S drop ;

: JWT-FIELD-RAW-BAD-VALUE ( -- )
   JWT-OPEN-A s" ab" JSON:WRITE-RAW
   s" k" s" v" STR:DATA -1 >str JSON:WRITE-FIELD-RAW drop ;

: JWT-TEST-FIELD-REJECTION ( -- )
   [: JWT-FIELD-S-BAD-VALUE ;] E-JSON-WRITE-SOURCE TTHROWSQ
   JWT-A JSON:WRITTEN$ s\" ab\"k\":" T$=
   [: JWT-FIELD-RAW-BAD-VALUE ;] E-JSON-WRITE-SOURCE TTHROWSQ
   JWT-A JSON:WRITTEN$ s\" ab\"k\":" T$= ;

\ A writer that was never opened and one that was closed are both rejected by
\ name; neither reads or writes the caller's bytes.
: JWT-FRESH-RAW ( -- )
   JWT-FRESH s" x" JSON:WRITE-RAW drop ;

: JWT-FRESH-$ ( -- )
   JWT-FRESH JSON:WRITTEN$ drop ;

: JWT-FRESH-FIELD-S ( -- )
   JWT-FRESH s" k" s" v" STR:DATA -1 >str JSON:WRITE-FIELD-S drop ;

: JWT-FRESH-FIELD-RAW ( -- )
   JWT-FRESH s" k" s" v" STR:DATA -1 >str JSON:WRITE-FIELD-RAW drop ;

: JWT-CLOSED-FIELD-S-NEG-KEY ( -- )
   JWT-OPEN-B JSON:CLOSE
   JWT-B s" k" STR:DATA -1 >str s" v" JSON:WRITE-FIELD-S drop ;

: JWT-CLOSED-FIELD-RAW-NEG-KEY ( -- )
   JWT-OPEN-B JSON:CLOSE
   JWT-B s" k" STR:DATA -1 >str s" v" JSON:WRITE-FIELD-RAW drop ;

: JWT-CLOSED-RAW ( -- )
   JWT-OPEN-B JSON:CLOSE
   JWT-B s" x" JSON:WRITE-RAW drop ;

: JWT-CLOSED-TWICE ( -- )
   JWT-OPEN-B JSON:CLOSE
   JWT-B JSON:CLOSE ;

: JWT-NULL-SRC ( -- ^u8 ) NULL-PTR ;

: JWT-CLOSED-FIELD-S-NULL-KEY ( -- )
   JWT-OPEN-B JSON:CLOSE
   JWT-B JWT-NULL-SRC 1 >str s" v" JSON:WRITE-FIELD-S drop ;

: JWT-CLOSED-FIELD-RAW-NULL-KEY ( -- )
   JWT-OPEN-B JSON:CLOSE
   JWT-B JWT-NULL-SRC 1 >str s" v" JSON:WRITE-FIELD-RAW drop ;

: JWT-OPEN-NULL ( -- )
   JWT-A NULL-PTR JSON:OPEN drop ;

: JWT-NULL-SOURCE ( -- )
   JWT-OPEN-A JWT-NULL-SRC 3 >str JSON:WRITE-RAW drop ;

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
   [: JWT-NULL-SOURCE ;] E-JSON-WRITE-SOURCE TTHROWSQ
   JWT-OPEN-B s" ok" JSON:WRITE-RAW JSON:WRITTEN$ s" ok" T$= ;

: JWT-RAW-NEG ( -- )
   JWT-OPEN-A s" x" STR:DATA -1 >str JSON:WRITE-RAW drop ;

: JWT-STRING-NEG ( -- )
   JWT-OPEN-A s" x" STR:DATA -1 >str JSON:WRITE-STRING drop ;

: JWT-U-NEG ( -- )
   JWT-OPEN-A -1 JSON:WRITE-U drop ;

: JWT-STRING-MIN ( -- )
   JWT-OPEN-A s" x" STR:DATA MEM-MAX-N negate 1- >str
   JSON:WRITE-STRING drop ;

\ Every source byte takes at least one byte of output, so text longer than the
\ output can grow to is rejected before a byte of it is read: these bytes end at
\ an inaccessible page, and the claim runs past them.
: JWT-STRING-UNREAD ( n -- ) {: u:n :}
   JWT-OPEN-A JWT-TAIL-N [char] a GUARD-PAGE:TAIL u >str
   JSON:WRITE-STRING drop ;

: JWT-KEY-UNREAD ( n -- ) {: u:n :}
   JWT-OPEN-A JWT-TAIL-N [char] a GUARD-PAGE:TAIL u >str
   JSON:WRITE-KEY drop ;

: JWT-TEST-ERRORS ( -- )
   [: JWT-RAW-NEG ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-STRING-NEG ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-STRING-MIN ;] E-JSON-WRITE-SOURCE TTHROWSQ
   [: JWT-U-NEG ;] E-JSON-WRITE-BYTE TTHROWSQ
   [: MEM-MAX-N JWT-STRING-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ
   [: MEM-MAX-N JWT-KEY-UNREAD ;] E-JSON-WRITE-CAPACITY TTHROWSQ ;

\ WRITE-INT writes every cell: zero, both ends of the range, and negatives such
\ as JSON-RPC's error codes, each compared with its exact decimal text.
: JWT-INT$ ( n -- str )
   {: v:n :}
   JWT-OPEN-A v JSON:WRITE-INT JSON:WRITTEN$ ;

: JWT-INT= ( n str -- )
   {: v:n s:str :}
   v JWT-INT$ s T$= ;

: JWT-TEST-INT ( -- )
   0 s" 0" JWT-INT=
   7 s" 7" JWT-INT=
   -7 s" -7" JWT-INT=
   -32601 s" -32601" JWT-INT=
   STR:MAX-I64 s" 9223372036854775807" JWT-INT=
   STR:MIN-I64 s" -9223372036854775808" JWT-INT=
   STR:MIN-I64 1+ s" -9223372036854775807" JWT-INT= ;

\ WRITE-U64 reads a cell's 64 bits unsigned: STR:MIN-I64 is 2^63 and -1
\ is 2^64-1.
: JWT-U64= ( n str -- )
   {: v:n s:str :}
   JWT-OPEN-A v JSON:WRITE-U64 JSON:WRITTEN$ s T$= ;

: JWT-TEST-U64 ( -- )
   0 s" 0" JWT-U64=
   STR:MAX-I64 s" 9223372036854775807" JWT-U64=
   STR:MIN-I64 s" 9223372036854775808" JWT-U64=
   -1 s" 18446744073709551615" JWT-U64= ;

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

: JWT-RT-CASE$ ( -- str )
   PROP:BUF-RESET
   JWT-RT-MAX-N PROP:RND% 0 ?do JWT-RT-BYTE+ loop
   PROP:BUF$ ;

: JWT-RT-ENCODE ( str -- str ) {: s:str :}
   JWT-RT-W JWT-RT-OUT @ JSON:OPEN s JSON:WRITE-STRING JSON:WRITTEN$ ;

: JWT-RT-DECODE$ ( str -- str ) {: s:str :}
   JWT-RT-READER s JSON:READ-INIT
   JSON:READ-NEXT JSON:T-STR T=
   JWT-RT-DECODE @ JSON:READ-STR {: got:str :}
   JSON:READ-CLOSE
   got ;

: JWT-RT-ONE ( -- )
   JWT-RT-CASE$ {: s:str :}
   s JWT-RT-ENCODE JWT-RT-DECODE$ s T$= ;

\ 2048 random strings: the output is a well-formed JSON string to the strict
\ reader, and it decodes back unchanged.
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

: JWT-OPEN-AB ( -- ^JSON:writer )
   JWT-OPEN-A s" ab" JSON:WRITE-RAW ;

: JWT-BAD-FFFE ( -- )
   JWT-OPEN-AB JWT-FFFE 2 >str JSON:WRITE-STRING drop ;
: JWT-BAD-CONT ( -- )
   JWT-OPEN-AB JWT-LONE-CONT 1 >str JSON:WRITE-STRING drop ;
: JWT-BAD-TRUNCATED ( -- )
   JWT-OPEN-AB JWT-TRUNCATED 2 >str JSON:WRITE-STRING drop ;
: JWT-BAD-OVERLONG ( -- )
   JWT-OPEN-AB JWT-OVERLONG 2 >str JSON:WRITE-STRING drop ;
: JWT-BAD-SURROGATE ( -- )
   JWT-OPEN-AB JWT-SURROGATE 3 >str JSON:WRITE-STRING drop ;
: JWT-BAD-KEY ( -- )
   JWT-OPEN-AB JWT-FFFE 1 >str JSON:WRITE-KEY drop ;

: JWT-BYTE-REJECTED ( [ -- ] -- )
   E-JSON-WRITE-BYTE TTHROWSQ
   JWT-A JSON:WRITTEN$ s" ab" T$= ;

: JWT-TEST-UTF8 ( -- )
   [: JWT-BAD-FFFE ;] JWT-BYTE-REJECTED
   [: JWT-BAD-CONT ;] JWT-BYTE-REJECTED
   [: JWT-BAD-TRUNCATED ;] JWT-BYTE-REJECTED
   [: JWT-BAD-OVERLONG ;] JWT-BYTE-REJECTED
   [: JWT-BAD-SURROGATE ;] JWT-BYTE-REJECTED
   [: JWT-BAD-KEY ;] JWT-BYTE-REJECTED
   JWT-UTF8-OK 9 >str JWT-RT-ENCODE JWT-RT-DECODE$
   JWT-UTF8-OK 9 >str T$= ;

\ é (2 bytes), € (3), 😀 (4), DEL, a quote, a tab, U+FFFF and U+10000: in
\ ASCII mode each scalar above U+007F and DEL is \uXXXX, one above U+FFFF a
\ surrogate pair.
: JWT-WIDE$ ( -- str )
   S\" \xC3\xA9\xE2\x82\xAC\xF0\x9F\x98\x80\x7F\"\t\xEF\xBF\xBF\xF0\x90\x80\x80" ;

: JWT-WIDE-ESC$ ( -- str )
   S\" \\u00e9\\u20ac\\ud83d\\ude00\\u007f\\\"\\t\\uffff\\ud800\\udc00" ;

: JWT-WIDE-WANT$ ( -- str )
   SB-RESET [char] { SB-APPEND-C
   [char] " SB-APPEND-C JWT-WIDE-ESC$ SB-APPEND [char] " SB-APPEND-C
   [char] : SB-APPEND-C
   [char] " SB-APPEND-C JWT-WIDE-ESC$ SB-APPEND [char] " SB-APPEND-C
   [char] } SB-APPEND-C SB$ ;

\ The value goes in two parts, split after é and €.
: JWT-TEST-ASCII ( -- )
   JWT-OPEN-A JWT-TRUE JSON:WRITE-ASCII!
   JSON:WRITE-OBJECT-START JWT-WIDE$ JSON:WRITE-KEY
   JSON:WRITE-STRING-START
   JWT-WIDE$ STR:DATA 5 >str JSON:WRITE-STRING-PART
   JWT-WIDE$ dup STR:DATA 5 + swap STR:LEN 5 - >str
   JSON:WRITE-STRING-PART
   JSON:WRITE-STRING-END JSON:WRITE-OBJECT-END
   JSON:WRITTEN$ JWT-WIDE-WANT$ T$= ;

: JWT-QUOTE$ ( str -- str )
   SB-RESET [char] " SB-APPEND-C SB-APPEND [char] " SB-APPEND-C SB$ ;

: JWT-TEST-ASCII-RESET ( -- )
   JWT-OPEN-A JWT-TRUE JSON:WRITE-ASCII! s" x" JSON:WRITE-STRING
   JSON:RESET JWT-WIDE$ JSON:WRITE-STRING
   JSON:WRITTEN$ JWT-WIDE-ESC$ JWT-QUOTE$ T$=
   JWT-OPEN-A JWT-WIDE$ JSON:WRITE-STRING JSON:WRITTEN$
   S\" \xC3\xA9\xE2\x82\xAC\xF0\x9F\x98\x80\x7F\\\"\\t\xEF\xBF\xBF\xF0\x90\x80\x80" JWT-QUOTE$ T$= ;

\ One nested document: a sub-object, an array of two numbers, an empty object
\ and an empty array. Width 2 breaks after each START and COMMA and closes on
\ its own line, except the empty pair; width 0 makes the same calls compact.
: JWT-NESTED ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" a" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
   s" x" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA s" y" JWT-TRUE JSON:WRITE-FIELD-BOOL
   JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
   s" b" JSON:WRITE-KEY JSON:WRITE-ARRAY-START 2 JSON:WRITE-U JSON:WRITE-COMMA 3 JSON:WRITE-U JSON:WRITE-ARRAY-END
   JSON:WRITE-COMMA
   s" c" JSON:WRITE-KEY JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
   s" d" JSON:WRITE-KEY JSON:WRITE-ARRAY-START JSON:WRITE-ARRAY-END
   JSON:WRITE-OBJECT-END ;

: JWT-INDENTED$ ( -- str )
   s\" {\n  \"a\": {\n    \"x\": 1,\n    \"y\": true\n  },\n  \"b\": [\n    2,\n    3\n  ],\n  \"c\": {},\n  \"d\": []\n}" ;

: JWT-TEST-INDENTED ( -- )
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JWT-NESTED JSON:WRITTEN$ JWT-INDENTED$ T$=
   JWT-GROW-OPEN 2 JSON:WRITE-INDENT! JWT-NESTED JSON:WRITTEN$ JWT-INDENTED$ T$=
   JWT-GROW-DONE ;

\ Opening a writer again makes it compact, whatever width it had.
: JWT-COMPACT$ ( -- str )
   s\" {\"a\":{\"x\":1,\"y\":true},\"b\":[2,3],\"c\":{},\"d\":[]}" ;

: JWT-TEST-COMPACT ( -- )
   JWT-OPEN-A 2 JSON:WRITE-INDENT! drop
   JWT-OPEN-A JWT-NESTED JSON:WRITTEN$ JWT-COMPACT$ T$= ;

\ A string in parts never breaks, even after a part ending in a comma; the
\ COMMA after it puts the number on its own line.
: JWT-TEST-PART-LAYOUT ( -- )
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JSON:WRITE-ARRAY-START
   JSON:WRITE-STRING-START s" a," JSON:WRITE-STRING-PART
   s" b" JSON:WRITE-STRING-PART JSON:WRITE-STRING-END
   JSON:WRITE-COMMA 1 JSON:WRITE-U JSON:WRITE-ARRAY-END
   JSON:WRITTEN$ s\" [\n  \"a,b\",\n  1\n]" T$= ;

\ RESET right after a START keeps the width and forgets the open object
\ and its due break.
: JWT-TEST-INDENT-RESET ( -- )
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JSON:WRITE-OBJECT-START JSON:RESET
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
   JWT-PARTS 3 >str JSON:WRITE-STRING-PART
   JWT-PARTS 3 + 1 >str JSON:WRITE-STRING-PART
   JWT-PARTS 4 + 2 >str JSON:WRITE-STRING-PART
   JSON:WRITE-STRING-END JSON:WRITTEN$ JWT-PARTS-WANT 10 >str T$=
   JWT-OPEN-B JWT-PARTS 6 >str JSON:WRITE-STRING JSON:WRITTEN$
   JWT-PARTS-WANT 10 >str T$= ;

: JWT-KEY-BAD ( -- )
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JSON:WRITE-OBJECT-START
   JWT-PARTS 4 + 1 >str JSON:WRITE-KEY drop ;
: JWT-TEST-KEY-REJECTED ( -- )
   [: JWT-KEY-BAD ;] E-JSON-WRITE-BYTE TTHROWSQ
   JWT-A JSON:WRITTEN$ s" {" T$=
   JWT-A JSON:WRITE-NULL JSON:WRITTEN$ s\" {\n  null" T$= ;

: JWT-PART-FF ( -- )
   JWT-OPEN-A JSON:WRITE-STRING-START
   JWT-FFFE 1 >str JSON:WRITE-STRING-PART drop ;
: JWT-PART-HALF ( -- )
   JWT-OPEN-A JSON:WRITE-STRING-START
   JWT-PARTS 4 + 1 >str JSON:WRITE-STRING-PART drop ;
: JWT-INDENT!-NEG ( -- ) JWT-OPEN-AB -1 JSON:WRITE-INDENT! drop ;
: JWT-BREAK-WRAP ( -- )   \ two levels of MEM-MAX-N spaces overflow a cell
   JWT-OPEN-AB JSON:WRITE-ARRAY-START JSON:WRITE-ARRAY-START MEM-MAX-N JSON:WRITE-INDENT!
   1 JSON:WRITE-U drop ;
: JWT-OBJECT-END-TOP ( -- ) JWT-OPEN-AB JSON:WRITE-OBJECT-END drop ;
: JWT-ARRAY-END-TOP ( -- ) JWT-OPEN-AB JSON:WRITE-ARRAY-END drop ;

: JWT-STATE-REJECTED ( [ -- ] -- )
   E-JSON-WRITE-STATE TTHROWSQ
   JWT-A JSON:WRITTEN$ s" ab" T$= ;

: JWT-FRESH-INDENT! ( -- ) JWT-FRESH 2 JSON:WRITE-INDENT! drop ;
: JWT-FRESH-START ( -- ) JWT-FRESH JSON:WRITE-STRING-START drop ;
: JWT-FRESH-PART ( -- ) JWT-FRESH s" x" JSON:WRITE-STRING-PART drop ;
: JWT-FRESH-END ( -- ) JWT-FRESH JSON:WRITE-STRING-END drop ;

: JWT-CLOSED ( -- ^JSON:writer ) JWT-OPEN-B JSON:CLOSE JWT-B ;
: JWT-CLOSED-INDENT! ( -- ) JWT-CLOSED 2 JSON:WRITE-INDENT! drop ;
: JWT-CLOSED-START ( -- ) JWT-CLOSED JSON:WRITE-STRING-START drop ;
: JWT-CLOSED-PART ( -- ) JWT-CLOSED s" x" JSON:WRITE-STRING-PART drop ;
: JWT-CLOSED-END ( -- ) JWT-CLOSED JSON:WRITE-STRING-END drop ;

: JWT-TEST-LAYOUT-REJECTIONS ( -- )
   [: JWT-PART-FF ;] E-JSON-WRITE-BYTE TTHROWSQ
   JWT-A JSON:WRITTEN$ s\" \"" T$=
   [: JWT-PART-HALF ;] E-JSON-WRITE-BYTE TTHROWSQ
   JWT-A JSON:WRITTEN$ s\" \"" T$=
   [: JWT-INDENT!-NEG ;] JWT-BYTE-REJECTED
   [: JWT-BREAK-WRAP ;] s" ab[[" JWT-REJECTED-KEEPS$
   [: JWT-OBJECT-END-TOP ;] JWT-STATE-REJECTED
   [: JWT-ARRAY-END-TOP ;] JWT-STATE-REJECTED
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
: JWT-COMMA-ARRAY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-ARRAY-START JSON:WRITE-COMMA 1 JSON:WRITE-U JSON:WRITE-COMMA 2 JSON:WRITE-U
   JSON:WRITE-ARRAY-END ;


: JWT-COMMA-OBJECT ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START JSON:WRITE-COMMA s" a" 1 JSON:WRITE-FIELD-U
   JSON:WRITE-COMMA s" b" 2 JSON:WRITE-FIELD-U JSON:WRITE-OBJECT-END ;


: JWT-TEST-COMMA-FIRST ( -- )
   JWT-OPEN-A JWT-COMMA-ARRAY JSON:WRITTEN$ s" [1,2]" T$=
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JWT-COMMA-ARRAY JSON:WRITTEN$ s\" [\n  1,\n  2\n]" T$=
   JWT-OPEN-A JWT-COMMA-OBJECT JSON:WRITTEN$ s\" {\"a\":1,\"b\":2}" T$=
   JWT-OPEN-A 2 JSON:WRITE-INDENT! JWT-COMMA-OBJECT JSON:WRITTEN$
   s\" {\n  \"a\": 1,\n  \"b\": 2\n}" T$= ;


: JWT-STATE-KEEPS$ ( [ -- ] str -- )   \ rejection left the document untouched
   {: want:str :}
   E-JSON-WRITE-STATE TTHROWSQ
   JWT-A JSON:WRITTEN$ want T$= ;


: JWT-COMMA-TWICE ( -- )
   JWT-OPEN-A JSON:WRITE-ARRAY-START 1 JSON:WRITE-U JSON:WRITE-COMMA JSON:WRITE-COMMA drop ;


: JWT-ARRAY-END-COMMA ( -- )
   JWT-OPEN-A JSON:WRITE-ARRAY-START 1 JSON:WRITE-U JSON:WRITE-COMMA JSON:WRITE-ARRAY-END drop ;


: JWT-OBJECT-END-COMMA ( -- )
   JWT-OPEN-A JSON:WRITE-OBJECT-START s" a" 1 JSON:WRITE-FIELD-U JSON:WRITE-COMMA
   JSON:WRITE-OBJECT-END drop ;


\ A writer closed right after a START still rejects a comma.
: JWT-CLOSED-OPEN-COMMA ( -- )
   JWT-OPEN-B JSON:WRITE-ARRAY-START JSON:CLOSE JWT-B JSON:WRITE-COMMA drop ;


: JWT-TEST-COMMA-REJECTIONS ( -- )
   [: JWT-COMMA-TWICE ;] s" [1," JWT-STATE-KEEPS$
   [: JWT-ARRAY-END-COMMA ;] s" [1," JWT-STATE-KEEPS$
   [: JWT-OBJECT-END-COMMA ;] s\" {\"a\":1," JWT-STATE-KEEPS$
   [: JWT-CLOSED-OPEN-COMMA ;] E-JSON-WRITE-STATE TTHROWSQ ;


\ Calls the layout rejects, each on JWT-A; JWT-KEYED$ and JWT-QUOTING$ are
\ what JWT-A holds when the rejected call comes.
: JWT-EMPTY$ ( -- str ) JWT-NAME 0 >str ;
: JWT-KEYED ( -- ^JSON:writer ) JWT-OPEN-A JSON:WRITE-OBJECT-START s" k" JSON:WRITE-KEY ;
: JWT-KEYED$ ( -- str ) s\" {\"k\":" ;
: JWT-QUOTING ( -- ^JSON:writer )
   JWT-OPEN-A JSON:WRITE-STRING-START s" ab" JSON:WRITE-STRING-PART ;
: JWT-QUOTING$ ( -- str ) s\" \"ab" ;
: JWT-COMMA-FRESH ( -- ) JWT-OPEN-A JSON:WRITE-COMMA drop ;
: JWT-COMMA-RESET ( -- )
   JWT-OPEN-A s" x" JSON:WRITE-RAW JSON:RESET JSON:WRITE-COMMA drop ;
: JWT-COMMA-KEYED ( -- ) JWT-KEYED JSON:WRITE-COMMA drop ;
: JWT-COMMA-QUOTING ( -- ) JWT-QUOTING JSON:WRITE-COMMA drop ;
: JWT-PART-FRESH ( -- ) JWT-OPEN-A s" x" JSON:WRITE-STRING-PART drop ;
: JWT-END-FRESH ( -- ) JWT-OPEN-A JSON:WRITE-STRING-END drop ;
: JWT-PART-VALUED ( -- ) JWT-OPEN-A 1 JSON:WRITE-U s" x" JSON:WRITE-STRING-PART drop ;
: JWT-END-VALUED ( -- ) JWT-OPEN-A 1 JSON:WRITE-U JSON:WRITE-STRING-END drop ;
: JWT-OBJECT-END-KEYED ( -- ) JWT-KEYED JSON:WRITE-OBJECT-END drop ;
: JWT-NULL-QUOTING ( -- ) JWT-QUOTING JSON:WRITE-NULL drop ;
: JWT-KEY-QUOTING ( -- ) JWT-QUOTING s" k" JSON:WRITE-KEY drop ;

: JWT-PIECED ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START s" k" JSON:WRITE-KEY JSON:WRITE-STRING-START
   s" a" JSON:WRITE-STRING-PART s" b" JSON:WRITE-STRING-PART JSON:WRITE-STRING-END
   JSON:WRITE-OBJECT-END ;

: JWT-PAIR ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-ARRAY-START 1 JSON:WRITE-U JSON:WRITE-COMMA s" x" JSON:WRITE-STRING JSON:WRITE-ARRAY-END ;


\ A comma, string piece, END or token where JSON takes none is rejected whole.
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
   s" JWT-OK ( ^JSON:writer -- ^JSON:writer ) JSON:WRITE-OBJECT-START JSON:WRITE-OBJECT-END" JWT-CHECK-ACCEPTS
   s" JWT-OPEN-OK ( ^JSON:writer ^VEC:vector<u8> -- ^JSON:writer ) JSON:OPEN" JWT-CHECK-ACCEPTS
   s" JWT-OPEN-BAD ( ^JSON:writer ^u8 -- ^JSON:writer ) JSON:OPEN" JWT-CHECK-REJECTS
   s" JWT-BAD-CELL ( n ^u8 n -- n ) JSON:WRITE-RAW" JWT-CHECK-REJECTS
   s" JWT-BAD-BUF ( ^u8 ^u8 n -- ^u8 ) JSON:WRITE-RAW" JWT-CHECK-REJECTS ;

: JWT-MAIN ( -- )
   T-RESET
   JWT-MOVER-FILL
   JWT-NEW JWT-OUT-A !  JWT-NEW JWT-OUT-B !  JWT-NEW JWT-RT-OUT !
   JWT-NEW JWT-RT-DECODE !
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
   JWT-TEST-GROW-DISPOSED-CLOSE
   JWT-TEST-SELF-RAW
   JWT-TEST-SELF-STRING
   JWT-TEST-FIELD-REJECTION
   JWT-TEST-STATE
   JWT-TEST-ERRORS
   JWT-TEST-INT
   JWT-TEST-U64
   JWT-TEST-FIELD-INT
   JWT-TEST-ROUNDTRIP
   JWT-TEST-UTF8
   JWT-TEST-INDENTED
   JWT-TEST-COMPACT
   JWT-TEST-PART-LAYOUT
   JWT-TEST-INDENT-RESET
   JWT-TEST-ASCII
   JWT-TEST-ASCII-RESET
   JWT-TEST-PARTS
   JWT-TEST-KEY-REJECTED
   JWT-TEST-LAYOUT-REJECTIONS
   JWT-TEST-COMMA-FIRST
   JWT-TEST-COMMA-REJECTIONS
   JWT-TEST-LAYOUT-STATES
   JWT-TEST-CHECKER
   JWT-OUT-A @ VEC:DELETE  JWT-OUT-B @ VEC:DELETE  JWT-RT-OUT @ VEC:DELETE
   JWT-RT-DECODE @ VEC:DELETE
   T-REPORT
   s" json-write-test: ok" type cr ;

JWT-MAIN

;package
