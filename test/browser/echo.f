\ echo.f - ECHO, the browser host's fixture module (docs/browser-host.md), built
\ by tools/wasm-build.f with the entry ECHO:TURN; test/browser/echo-test.f runs
\ it under lib/browser/host-cli.mjs.
\
\ Each run reads the event the host wrote. Start, the zero event the first run
\ finds, answers hello with the event's address, a fetch of `fixture` and then
\ the text "loading", in the buffer every text record uses, so a host that
\ answers the fetch before it shows that text shows the bytes run's text there
\ instead. Bytes answers the text of a checksum the module computes over the
\ copied payload itself, so a payload the host failed to copy, or copied
\ elsewhere, is caught: h := (31h + byte) mod 2^32 from 0, in decimal. Pointer
\ answers the text "x,y".

package ECHO
private

CAST: >BYTES ( n -- ptr u8 )
CAST: >CELLS ( ptr u8 -- ptr n )
CAST: BYTES>N ( ptr u8 -- n )

\ The event: its kind and four fields, zero, which is start, until the host
\ writes one.
40 BUFFER: EVENT-BUF
\ The text a record names: at most two nonnegative cells' digits and a comma.
40 BUFFER: TEXT
variable TEXT-U

\ A record field, eight bytes low first, appended to OUT.
: FIELD, ( n -- )
   8 0 do  dup $FF and emit  8 rshift  loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD,  a FIELD,  b FIELD,  c FIELD, ;

\ Field i of the event.
: EVENT@ ( n -- n )
   8 * EVENT-BUF + >CELLS @ ;

: CHAR+ ( n -- )
   TEXT TEXT-U @ + c!  TEXT-U @ 1+ TEXT-U ! ;

\ The digits of n, which is not negative.
: DIGITS ( n -- )
   {: n:n :}
   n 10 / {: q:n :}
   q 0 <> if q RECURSE then
   n q 10 * - 48 + CHAR+ ;

: TEXT-RECORD ( -- )
   3 TEXT BYTES>N TEXT-U @ 0 RECORD ;

: START ( -- )
   0 EVENT-BUF BYTES>N 0 0 RECORD
   s" fixture" {: path:ptr u:n :}
   1 path BYTES>N u 0 RECORD
   0 TEXT-U !
   s" loading" {: t:ptr tu:n :}
   tu 0 ?do  t i + c@ CHAR+  loop
   TEXT-RECORD ;

: SUM ( n n -- n )
   {: at:n u:n :}
   0  u 0 ?do  31 *  at i + >BYTES c@ +  $FFFFFFFF and  loop ;

: BYTES ( -- )
   0 TEXT-U !
   1 EVENT@ 2 EVENT@ SUM DIGITS
   TEXT-RECORD ;

: POINTER ( -- )
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
