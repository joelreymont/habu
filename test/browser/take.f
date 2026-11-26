\ take.f - TAKE-TURN, a browser host module that keeps a delivery past its turn
\ with BROWSER-EVENT:TAKE, built by `tools/hb-build.f -- --target wasm`, whose
\ entry is MAIN; test/browser/take-test.f runs it under
\ src/lib/browser/host-cli.mjs.
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
require test/browser/turn-revision.f
package TAKE-TURN
private

CAST: BYTES>N ( ^u8 -- n )

40 BUFFER: TEXT
variable TEXT-U
PTR-VARIABLE HELD                    \ the taken delivery, HELD-U 0 for none
variable HELD-U

: FIELD, ( n -- )
   8 0 do  dup $FF and emit  8 rshift  loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD,  a FIELD,  b FIELD,  c FIELD, ;

: SAY ( str -- )
   {: t:str :}
   t STR:LEN 0 ?do  t STR:DATA i + c@  TEXT TEXT-U @ + c!  TEXT-U @ 1+ TEXT-U !  loop ;

: TEXT-RECORD ( str -- )
   {: s:str :}
   3 s STR:DATA BYTES>N s STR:LEN 0 RECORD ;

: SHOW-HELD ( -- )
   HELD-U @ 0= if exit then
   0 TEXT-U !  s" held " SAY  HELD @ HELD-U @ >str SAY
   TEXT TEXT-U @ >str TEXT-RECORD
   HELD @ HELD-U @ BROWSER-EVENT:FREE  0 HELD-U ! ;

: SAME? ( str str -- bool )
   {: a:str b:str :}
   a STR:DATA BYTES>N b STR:DATA BYTES>N =  a STR:LEN b STR:LEN =  and ;

: HOLD ( -- )
   0 BROWSER-EVENT:EVENT@ 1 BROWSER-EVENT:EVENT@ 2 BROWSER-EVENT:EVENT@ BROWSER-EVENT:DELIVERY {: kind:n at:n u:n d:str :}
   BROWSER-EVENT:TAKE {: k:str :}
   k d SAME? 0= if 91 throw then
   BROWSER-EVENT:DELIVERY d SAME? 0= if 91 throw then
   0 BROWSER-EVENT:EVENT@ kind <>  1 BROWSER-EVENT:EVENT@ at <> or  2 BROWSER-EVENT:EVENT@ u <> or if 91 throw then
   k STR:DATA HELD !  k STR:LEN HELD-U ! ;

: TYPED ( -- )
   SHOW-HELD
   BROWSER-EVENT:DELIVERY s" take" STR:= if  HOLD  BROWSER-EVENT:DELIVERY TEXT-RECORD exit  then
   BROWSER-EVENT:DELIVERY s" boom" STR:= if  HOLD  93 throw  then ;

: DISPATCH ( -- )
   0 BROWSER-EVENT:EVENT@ {: kind:n :}
   kind 0 = if  0 BROWSER-EVENT:EVENT BYTES>N TURN-REVISION:PROTOCOL 0 RECORD exit  then
   kind 4 = if TYPED then ;

public

: TURN ( -- )
   [: DISPATCH ;] BROWSER-EVENT:HANDLE ;

;package

: MAIN ( -- ) TAKE-TURN:TURN ;
