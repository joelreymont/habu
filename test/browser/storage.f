\ storage.f - STORAGE-TURN, a browser host module whose dynamic buffer shares
\ the module's allocator with the deliveries the host reserves through it, built
\ by `tools/hb-build.f -- --target wasm`, whose entry is MAIN;
\ test/browser/storage-test.f runs it under src/lib/browser/host-cli.mjs.
\
\ Start reserves two pages, P and P+1, then answers hello, a fetch of `fixture`
\ and the text "loading". The light's delivery takes P+2, which its turn frees.
\ Bytes keeps the fetched bytes for the instance and grows the buffer to four
\ pages, which keeps its last byte and frees P's two, joining P+2; released and
\ reserved again at three pages, which take P to P+2, it then answers the text
\ of the checksum of the fetched bytes. Pointer releases the buffer and reserves
\ two pages, which take P back cleared, and answers "x,y". Typed checks its one
\ byte, 'a' for the first 500 and 'b' after, and frees it: every typed
\ delivery must take the first one's address, P+2, so memory does not grow with
\ keystrokes. The 1000th checks that the fetched bytes kept their checksum and
\ answers "typed 1000". A failed check throws its code, which the host shows.
require src/lib/string.f
require src/lib/browser/event.f
require test/browser/turn-revision.f
package STORAGE-TURN
private

CAST: >BYTES ( n -- ^u8 )
CAST: BYTES>N ( ^u8 -- n )

40 BUFFER: TEXT
variable TEXT-U
DYNAMIC-BUFFER HELD u8
PTR-VARIABLE FIRST
variable FETCH-AT
variable FETCH-LEN
variable FETCH-SUM
variable TYPED-AT                           \ the first typed delivery's address
variable TYPED-N

: FIELD, ( n -- )
   8 0 do  dup $FF and emit  8 rshift  loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD,  a FIELD,  b FIELD,  c FIELD, ;

: CHAR+ ( n -- )
   TEXT TEXT-U @ + c!  TEXT-U @ 1+ TEXT-U ! ;

: DIGITS ( n -- )
   {: n:n :}
   n 10 / {: q:n :}
   q 0 <> if q RECURSE then
   n q 10 * - 48 + CHAR+ ;

: TEXT-RECORD ( -- )
   3 TEXT BYTES>N TEXT-U @ 0 RECORD ;

: SUM ( n n -- n )
   {: at:n u:n :}
   0  u 0 ?do  31 *  at i + >BYTES c@ +  $FFFFFFFF and  loop ;

: START ( -- )
   131072 HELD-RESERVE
   0 HELD FIRST !
   42 131071 HELD c!
   0 BROWSER-EVENT:EVENT BYTES>N TURN-REVISION:PROTOCOL 0 RECORD
   s" fixture" {: path:str :}
   1 path STR:DATA BYTES>N path STR:LEN 0 RECORD
   0 TEXT-U !
   s" loading" {: t:str :}
   t STR:LEN 0 ?do  t STR:DATA i + c@ CHAR+  loop
   TEXT-RECORD ;

: BYTES ( -- )
   1 BROWSER-EVENT:EVENT@ FETCH-AT !
   2 BROWSER-EVENT:EVENT@ FETCH-LEN !
   FETCH-AT @ FETCH-LEN @ SUM FETCH-SUM !
   262144 HELD-RESERVE
   131071 HELD c@ 42 <> if 92 throw then
   HELD-RELEASE
   196608 HELD-RESERVE
   0 TEXT-U !
   1 BROWSER-EVENT:EVENT@ 2 BROWSER-EVENT:EVENT@ SUM DIGITS
   TEXT-RECORD ;

: POINTER ( -- )
   HELD-RELEASE
   131072 HELD-RESERVE
   0 HELD FIRST @ <> if 94 throw then
   131071 HELD c@ 0<> if 95 throw then
   0 TEXT-U !
   1 BROWSER-EVENT:EVENT@ DIGITS  44 CHAR+  2 BROWSER-EVENT:EVENT@ DIGITS
   TEXT-RECORD ;

: TYPED ( -- )
   2 BROWSER-EVENT:EVENT@ 1 <> if 96 throw then
   1 BROWSER-EVENT:EVENT@ >BYTES c@ TYPED-N @ 500 < if 97 else 98 then <> if 97 throw then
   TYPED-N @ 0= if 1 BROWSER-EVENT:EVENT@ TYPED-AT ! then
   1 BROWSER-EVENT:EVENT@ TYPED-AT @ <> if 91 throw then
   BROWSER-EVENT:DELIVERY str> BROWSER-EVENT:FREE
   TYPED-N @ 1+ TYPED-N !
   TYPED-N @ 1000 = if
      FETCH-AT @ FETCH-LEN @ SUM FETCH-SUM @ <> if 99 throw then
      0 TEXT-U !
      s" typed " {: t:str :}
      t STR:LEN 0 ?do t STR:DATA i + c@ CHAR+ loop
      TYPED-N @ DIGITS TEXT-RECORD
   then ;

public

: TURN ( -- )
   0 BROWSER-EVENT:EVENT@ {: kind:n :}
   kind 0 = if START exit then
   kind 1 = if BYTES exit then
   kind 4 = if TYPED exit then
   kind 20 = if BROWSER-EVENT:DELIVERY str> BROWSER-EVENT:FREE exit then
   POINTER ;

;package

: MAIN ( -- ) STORAGE-TURN:TURN ;
