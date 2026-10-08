\ storage.f - STORAGE-TURN, a browser host module (docs/browser-host.md) whose
\ dynamic buffer shares memory with the pages the host adds, built by
\ tools/wasm-build.f with the entry STORAGE-TURN:TURN; test/browser/storage-test.f
\ runs it under lib/browser/host-cli.mjs. Its records are test/browser/echo.f's.
\
\ Start reserves two pages, P and P+1, then answers hello, a fetch of `fixture`
\ and the text "loading"; the host places the fixture's bytes at P+2. Bytes
\ grows the buffer to four pages, which must lie past the host's bytes, and
\ frees P's two. Released and reserved again at three pages, the buffer must
\ again lie past the host's bytes: only the four freed pages hold three, since
\ P's two do not join the host's. Bytes then answers the text of the checksum of
\ the host's bytes, so a host page the module cleared or wrote changes it.
\ Pointer releases the buffer and reserves two pages, which take P back cleared,
\ and answers "x,y". A failed check throws its code, which the host shows.
package STORAGE-TURN
private

CAST: >BYTES ( n -- ptr u8 )
CAST: >CELLS ( ptr u8 -- ptr n )
CAST: BYTES>N ( ptr u8 -- n )

40 BUFFER: EVENT-BUF
40 BUFFER: TEXT
variable TEXT-U
DYNAMIC-BUFFER HELD u8
PTR-VARIABLE FIRST
variable HOST-END                          \ the end of the host's bytes

: FIELD, ( n -- )
   8 0 do  dup $FF and emit  8 rshift  loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD,  a FIELD,  b FIELD,  c FIELD, ;

: EVENT@ ( n -- n )
   8 * EVENT-BUF + >CELLS @ ;

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

\ The buffer lies past the host's bytes, or the run throws code.
: PAST-HOST ( n -- )
   {: code:n :}
   0 HELD BYTES>N HOST-END @ < if code throw then ;

: START ( -- )
   131072 HELD-RESERVE
   0 HELD FIRST !
   42 131071 HELD c!
   0 EVENT-BUF BYTES>N 0 0 RECORD
   s" fixture" {: path:ptr u:n :}
   1 path BYTES>N u 0 RECORD
   0 TEXT-U !
   s" loading" {: t:ptr tu:n :}
   tu 0 ?do  t i + c@ CHAR+  loop
   TEXT-RECORD ;

: BYTES ( -- )
   1 EVENT@ 2 EVENT@ + HOST-END !
   262144 HELD-RESERVE
   91 PAST-HOST
   131071 HELD c@ 42 <> if 92 throw then
   HELD-RELEASE
   196608 HELD-RESERVE
   93 PAST-HOST
   0 TEXT-U !
   1 EVENT@ 2 EVENT@ SUM DIGITS
   TEXT-RECORD ;

: POINTER ( -- )
   HELD-RELEASE
   131072 HELD-RESERVE
   0 HELD FIRST @ <> if 94 throw then
   131071 HELD c@ 0<> if 95 throw then
   0 TEXT-U !
   1 EVENT@ DIGITS  44 CHAR+  2 EVENT@ DIGITS
   TEXT-RECORD ;

public

: TURN ( -- )
   0 EVENT@ {: kind:n :}
   kind 0 = if START exit then
   kind 1 = if BYTES exit then
   POINTER ;

;package
