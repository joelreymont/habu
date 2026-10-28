\ take.f - TAKE-TURN, a browser host module (src/lib/browser/host.f) that keeps
\ a delivery past its turn with BROWSER-EVENT:TAKE (src/lib/browser/event.f),
\ built by tools/wasm-build.f with the entry TAKE-TURN:TURN;
\ test/browser/take-test.f runs it under src/lib/browser/host-cli.mjs. Its
\ records are test/browser/echo.f's.
\
\ Start answers hello. Each typed turn first answers "held T", T the delivery the
\ turn before it took, read from the taken pages, and frees those pages. Typed
\ "take" then takes its delivery, checks that DELIVERY and the event's fields
\ read as they did before TAKE, and answers the text DELIVERY names, the taken
\ pages themselves. Typed "boom" takes its delivery and throws 93. Any other
\ typed text takes nothing, so HANDLE frees it. A failed check throws its code,
\ which the host shows.
require src/lib/string.f
require src/lib/browser/event.f
package TAKE-TURN
private
using BROWSER-EVENT

CAST: BYTES>N ( ptr u8 -- n )

40 BUFFER: TEXT
variable TEXT-U
PTR-VARIABLE HELD                    \ the taken delivery, HELD-U 0 for none
variable HELD-U

: FIELD, ( n -- )
   8 0 do  dup $FF and emit  8 rshift  loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD,  a FIELD,  b FIELD,  c FIELD, ;

\ The bytes appended to TEXT.
: SAY ( ptr u8 n -- )
   {: t:ptr u:n :}
   u 0 ?do  t i + c@  TEXT TEXT-U @ + c!  TEXT-U @ 1+ TEXT-U !  loop ;

: TEXT-RECORD ( ptr u8 n -- )
   {: t:ptr u:n :}
   3 t BYTES>N u 0 RECORD ;

\ Answers "held T" from a copy of the taken bytes, then frees them.
: SHOW-HELD ( -- )
   HELD-U @ 0= if exit then
   0 TEXT-U !  s" held " SAY  HELD @ HELD-U @ SAY
   TEXT TEXT-U @ TEXT-RECORD
   HELD @ HELD-U @ FREE  0 HELD-U ! ;

: SAME? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr au:n b:ptr bu:n :}
   a BYTES>N b BYTES>N =  au bu =  and ;

\ Keeps the delivery in HELD; throws 91 unless TAKE answers DELIVERY and leaves
\ it and the event's kind, address and length as they were.
: HOLD ( -- )
   0 EVENT@ 1 EVENT@ 2 EVENT@ DELIVERY {: kind:n at:n u:n d:ptr du:n :}
   BROWSER-EVENT:TAKE {: k:ptr ku:n :}
   k ku d du SAME? 0= if 91 throw then
   DELIVERY d du SAME? 0= if 91 throw then
   0 EVENT@ kind <>  1 EVENT@ at <> or  2 EVENT@ u <> or if 91 throw then
   k HELD !  ku HELD-U ! ;

: TYPED ( -- )
   SHOW-HELD
   DELIVERY s" take" STR= if  HOLD  DELIVERY TEXT-RECORD exit  then
   DELIVERY s" boom" STR= if  HOLD  93 throw  then ;

: DISPATCH ( -- )
   0 EVENT@ {: kind:n :}
   kind 0 = if  0 EVENT BYTES>N 0 0 RECORD exit  then
   kind 4 = if TYPED then ;

public

: TURN ( -- )
   [: DISPATCH ;] HANDLE ;

;using
;package
