\ echo.f - ECHO, the browser host's fixture module (src/lib/browser/host.f),
\ built by tools/wasm-build.f with the entry ECHO:TURN; test/browser/echo-test.f
\ runs it under src/lib/browser/host-cli.mjs. Every run frees the event's
\ delivery (src/lib/browser/event.f), so typed text and an answer's body are
\ copies, in REQ and ANSWER.

require src/lib/errors.f
require src/lib/string.f
require src/lib/browser/event.f

package ECHO
private
using BROWSER-EVENT

CAST: >BYTES ( n -- ptr u8 )
CAST: BYTES>N ( ptr u8 -- n )

\ The text a record names: "released ", two signed cells' digits and a comma.
64 BUFFER: TEXT
variable TEXT-U
\ The span both of a typed run's posts name: the path T, then the body T twice.
192 constant REQ-CAP
REQ-CAP BUFFER: REQ
variable REQ-U
DYNAMIC-BUFFER ANSWER u8              \ the last answer's body

\ A record field, eight bytes low first, appended to OUT.
: FIELD, ( n -- )
   8 0 do  dup $FF and emit  8 rshift  loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD,  a FIELD,  b FIELD,  c FIELD, ;

: CHAR+ ( n -- )
   TEXT TEXT-U @ + c!  TEXT-U @ 1+ TEXT-U ! ;

: SAY ( ptr u8 n -- )
   {: t:ptr u:n :}
   u 0 ?do  t i + c@ CHAR+  loop ;

\ The digits of n, which is not negative.
: DIGITS ( n -- )
   {: n:n :}
   n 10 / {: q:n :}
   q 0 <> if q RECURSE then
   n q 10 * - 48 + CHAR+ ;

: SIGNED ( n -- )
   {: n:n :}
   n 0< if  45 CHAR+  n negate DIGITS  else  n DIGITS  then ;

: TEXT-RECORD ( -- )
   3 TEXT BYTES>N TEXT-U @ 0 RECORD ;

: FETCH-FIXTURE ( -- )
   s" fixture" {: path:ptr u:n :}
   1 path BYTES>N u 0 RECORD ;

\ The zero event: hello with the event's address, a fetch of `fixture`, then
\ "loading" in the buffer the bytes, answered and pointer texts share, so a host
\ that answers the fetch before it shows that text shows the bytes run's text.
: START ( -- )
   0 EVENT BYTES>N 0 0 RECORD
   FETCH-FIXTURE
   0 TEXT-U !  s" loading" SAY  TEXT-RECORD ;

: SUM ( n n -- n )
   {: at:n u:n :}
   0  u 0 ?do  31 *  at i + >BYTES c@ +  $FFFFFFFF and  loop ;

\ A checksum the module computes over the copied payload, h := (31h + byte)
\ mod 2^32 from 0, in decimal: a payload not copied, or copied elsewhere, fails.
: BYTES ( -- )
   0 TEXT-U !
   1 EVENT@ 2 EVENT@ SUM DIGITS
   TEXT-RECORD ;

: AT ( ptr u8 n -- )
   0 TEXT-U !  SAY  1 EVENT@ SIGNED  44 CHAR+  2 EVENT@ SIGNED
   TEXT-RECORD ;

\ The u bytes at at are the text t.
: IS? ( n n ptr u8 n -- bool )
   {: at:n u:n t:ptr tu:n :}
   u tu <> if false exit then
   tu 0 ?do  at i + >BYTES c@  t i + c@ <> if false unloop exit then  loop
   true ;

\ The u bytes at at appended to REQ.
: REQ+ ( n n -- )
   {: at:n u:n :}
   REQ-U @ u + REQ-CAP > if E-SPAN-CAPACITY throw then
   u 0 ?do  at i + >BYTES c@  REQ REQ-U @ + i + c!  loop
   REQ-U @ u + REQ-U ! ;

\ Typed text T posts twice into REQ's span, then answers T. The second post
\ waits behind the first, so a host must copy it before it waits. "trap" traps
\ at a load from the null region; "long path" names a path one byte past the
\ span and "neg path" one of -1 bytes.
: TYPED ( -- )
   1 EVENT@ 2 EVENT@ {: at:n u:n :}
   at u s" trap" IS? if  $FFFF >BYTES c@ drop exit  then
   0 REQ-U !  at u REQ+  at u REQ+  at u REQ+
   at u s" long path" IS? if  REQ-U @ 1+
   else  at u s" neg path" IS? if  -1  else  u  then  then {: path:n :}
   4 REQ BYTES>N REQ-U @ path RECORD
   4 REQ BYTES>N REQ-U @ path RECORD
   3 REQ BYTES>N u 0 RECORD ;

\ The status's text, then the body; status 200 fetches `fixture` in the record
\ before them, so its answer would rewrite that status text.
: ANSWERED ( -- )
   3 EVENT@ 200 = if FETCH-FIXTURE then
   0 TEXT-U !  3 EVENT@ DIGITS  TEXT-RECORD
   DELIVERY {: at:ptr u:n :}
   u 1 max ANSWER-RESERVE
   at 0 ANSWER u BYTE-COPY
   3 0 ANSWER BYTES>N u 0 RECORD ;

: DISPATCH ( -- )
   0 EVENT@ {: kind:n :}
   kind 0 = if START exit then
   kind 1 = if BYTES exit then
   kind 2 = if s" " AT exit then
   kind 3 = if ANSWERED exit then
   kind 4 = if TYPED exit then
   kind 5 = if s" moved " AT exit then
   kind 6 = if s" released " AT exit then ;

public

: TURN ( -- )
   [: DISPATCH ;] HANDLE ;

;using
;package
