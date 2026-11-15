\ event-stream.f - incremental Server-Sent Events framing over arbitrary bytes, by
\ the HTML "Parsing an event stream" algorithm (9.2.6); no socket, reconnect or
\ queue, and comments, unknown fields, id and retry are ignored. FEED's callback
\ borrows the type ("message" by default) and data; invalid UTF-8 is E-UTF8 first.
\ FINISH drops an unterminated event; FEED after it is E-STATE. A callback's throw
\ leaves the decoder failed: CLOSE it. STORAGE CLASS. CALLER-OWNED (STORAGE-BYTES).

require src/lib/byte-buffer.f
require src/lib/string.f
require src/lib/utf8-scalar.f

package SSE
public

-9450 constant E-FIRST
-9459 constant E-LAST
-9450 constant E-STATE
-9451 constant E-STORAGE
-9452 constant E-SOURCE
-9453 constant E-UTF8

NEWTYPE decoder

private

0 constant ST-OFF
1 cells constant BOM-OFF
2 cells constant CR-OFF
3 cells constant DATA-OFF
4 cells constant LINE-OFF
LINE-OFF BUF:HDR-BYTES + constant BODY-OFF
BODY-OFF BUF:HDR-BYTES + constant EVENT-OFF
EVENT-OFF BUF:HDR-BYTES + constant SIZE

0 constant CLOSED
1 constant LIVE
2 constant FINISHED
3 constant FAILED

0 constant BOM-START
1 constant BOM-EF
2 constant BOM-EF-BB
3 constant BOM-DONE
$EF constant BOM-FIRST
$BB constant BOM-SECOND
$BF constant BOM-THIRD
10 constant LF
13 constant CR
32 constant SP
58 constant COLON
64 constant START-CAP

CAST: >DECODER ( ^n -- decoder )
CAST: DECODER>STATE ( decoder -- ^n )

: ST@ ( ^n -- n ) ST-OFF + @ ;
: ST! ( n ^n -- ) ST-OFF + ! ;
: BOM@ ( ^n -- n ) BOM-OFF + @ ;
: BOM! ( n ^n -- ) BOM-OFF + ! ;
: CR@ ( ^n -- n ) CR-OFF + @ ;
: CR! ( n ^n -- ) CR-OFF + ! ;
: DATA? ( ^n -- bool ) DATA-OFF + @ 0<> ;
: DATA! ( n ^n -- ) DATA-OFF + ! ;
: LINE-BUF ( ^n -- ^n ) LINE-OFF + ;
: BODY-BUF ( ^n -- ^n ) BODY-OFF + ;
: EVENT-BUF ( ^n -- ^n ) EVENT-OFF + ;

: CHECK-LIVE ( ^n -- ) ST@ LIVE <> if E-STATE throw then ;
: CHECK-OPEN ( ^n -- )
   ST@ dup CLOSED = if drop E-STATE throw then
   dup LIVE = if drop exit then
   dup FINISHED = if drop exit then
   FAILED = if exit then
   E-STATE throw ;

: ZERO-BUF ( ^n -- )
   {: buf:^ :}
   0 buf ! 0 buf CELL + ! 0 buf 2 cells + ! ;

: INIT-BUF ( ^n -- ) START-CAP BUF:N>BLEN BUF:INIT ;

: INIT-BUFS ( ^n -- ^n )
   {: state:^ :}
   state LINE-BUF INIT-BUF
   state BODY-BUF INIT-BUF
   state EVENT-BUF INIT-BUF
   state ;

: DISPOSE-IF-LIVE ( ^n -- )
   {: buf:^ :}
   buf 2 cells + @ 0<> if buf BUF:DISPOSE then ;

: DISPOSE-BUFS ( ^n -- )
   {: state:^ :}
   state LINE-BUF DISPOSE-IF-LIVE
   state BODY-BUF DISPOSE-IF-LIVE
   state EVENT-BUF DISPOSE-IF-LIVE ;

: RESET-EVENT ( ^n -- )
   {: state:^ :}
   state BODY-BUF BUF:CLEAR
   state EVENT-BUF BUF:CLEAR
   0 state DATA! ;

: DISPATCH ( ^n a [ a ^u8 n ^u8 n -- ] -- )
   {: state:^ ctx:a cb :}
   state DATA? 0= if state RESET-EVENT exit then
   FAILED state ST!
   state EVENT-BUF BUF:SPAN$ BUF:BLEN>N {: kind:^ ku:n :}
   state BODY-BUF BUF:SPAN$ BUF:BLEN>N {: body:^ bu:n :}
   kind ku UTF8:VALID? 0= if E-UTF8 throw then
   body bu 1- UTF8:VALID? 0= if E-UTF8 throw then
   ctx
   ku 0= if s" message" else kind ku then
   body bu 1- cb execute
   state RESET-EVENT
   LIVE state ST! ;

: COLON-AT ( ^u8 n -- n )
   {: src:^ u:n :}
   u 0 ?do src i + c@ COLON = if i unloop exit then loop
   u ;

: SPLIT-FIELD ( ^u8 n -- ^u8 n ^u8 n )
   {: src:^ u:n :}
   src u COLON-AT {: at:n :}
   at u = if src u src u + 0 exit then
   src at
   src at 1+ + u at 1+ - {: value:^ vu:n :}
   vu 0 > if value c@ SP = if value 1+ vu 1- exit then then
   value vu ;

: FIELD-LINE ( ^n -- )
   {: state:^ :}
   state LINE-BUF BUF:SPAN$ BUF:BLEN>N {: src:^ u:n :}
   src c@ COLON = if exit then
   src u SPLIT-FIELD {: name:^ nu:n value:^ vu:n :}
   name nu s" data" STR= if
      value vu BUF:N>BLEN state BODY-BUF BUF:APPEND-SPAN
      LF state BODY-BUF BUF:APPEND-BYTE
      1 state DATA!
      exit
   then
   name nu s" event" STR= if
      value vu BUF:N>BLEN state EVENT-BUF BUF:REPLACE
   then ;

: END-LINE ( ^n a [ a ^u8 n ^u8 n -- ] -- )
   {: state:^ ctx:a cb :}
   state LINE-BUF BUF:LEN@ BUF:BLEN>N 0= if
      state ctx cb DISPATCH
   else
      state FIELD-LINE
   then
   state LINE-BUF BUF:CLEAR ;

: BYTE ( ^n n a [ a ^u8 n ^u8 n -- ] -- )
   {: state:^ byte:n ctx:a cb :}
   state CR@ 0<> if
      0 state CR!
      byte LF = if exit then
   then
   byte CR = if
      state ctx cb END-LINE
      1 state CR!
      exit
   then
   byte LF = if state ctx cb END-LINE exit then
   byte state LINE-BUF BUF:APPEND-BYTE ;

: BOM-BYTE ( ^n n a [ a ^u8 n ^u8 n -- ] -- )
   {: state:^ byte:n ctx:a cb :}
   state BOM@ BOM-START = if
      byte BOM-FIRST = if BOM-EF state BOM! exit then
      BOM-DONE state BOM!
      state byte ctx cb BYTE exit
   then
   state BOM@ BOM-EF = if
      byte BOM-SECOND = if BOM-EF-BB state BOM! exit then
      BOM-DONE state BOM!
      state BOM-FIRST ctx cb BYTE
      state byte ctx cb BYTE exit
   then
   state BOM@ BOM-EF-BB = if
      BOM-DONE state BOM!
      byte BOM-THIRD = if exit then
      state BOM-FIRST ctx cb BYTE
      state BOM-SECOND ctx cb BYTE
      state byte ctx cb BYTE exit
   then
   state byte ctx cb BYTE ;

\ The offset of the first CR or LF in the span, else its length.
: TERM-AT ( ^u8 n -- n )
   {: src:^ u:n :}
   0 begin dup u < while
      dup src + c@ dup LF = swap CR = or if exit then
      1+
   repeat ;

\ Feed the bytes before the next terminator as one append, which is what BYTE
\ does for each of them once the BOM is settled: a pending CR is answered by
\ the first non-LF byte. Terminators and BOM bytes go through BOM-BYTE. The
\ answer is the number of bytes consumed.
: STEP ( ^n ^u8 n a [ a ^u8 n ^u8 n -- ] -- n )
   {: state:^ src:^ u:n ctx:a cb :}
   state BOM@ BOM-DONE = if
      src u TERM-AT {: run:n :}
      run 0 > if
         0 state CR!
         src run BUF:N>BLEN state LINE-BUF BUF:APPEND-SPAN
         run exit
      then
   then
   state src c@ ctx cb BOM-BYTE
   1 ;

public

: STORAGE-BYTES ( -- n ) SIZE ;

\ Storage is zero-initialized before its first OPEN and remains exclusive until
\ CLOSE. CLOSE clears the liveness cell so the same storage can be reopened.
: OPEN ( ^n n -- decoder )
   {: state:^ cap:n :}
   state 0= if E-STORAGE throw then
   cap STORAGE-BYTES < if E-STORAGE throw then
   state ST@ CLOSED <> if E-STATE throw then
   state LINE-BUF ZERO-BUF
   state BODY-BUF ZERO-BUF
   state EVENT-BUF ZERO-BUF
   state [: INIT-BUFS ;] catch {: code:n :} drop
   code 0<> if state DISPOSE-BUFS code throw then
   BOM-START state BOM!
   0 state CR! 0 state DATA!
   LIVE state ST!
   state >DECODER ;

: FEED ( decoder ^u8 n a [ a ^u8 n ^u8 n -- ] -- )
   {: d:decoder src:^ u:n ctx:a cb :}
   d DECODER>STATE {: state:^ :}
   state CHECK-LIVE
   u 0 < if E-SOURCE throw then
   u 0 > src 0= and if E-SOURCE throw then
   0 begin dup u < while
      {: at:n :} state src at + u at - ctx cb STEP at +
   repeat drop ;

: FINISH ( decoder -- )
   DECODER>STATE {: state:^ :}
   state CHECK-LIVE
   FINISHED state ST! ;

: CLOSE ( decoder -- )
   DECODER>STATE {: state:^ :}
   state CHECK-OPEN
   state LINE-BUF BUF:DISPOSE
   state BODY-BUF BUF:DISPOSE
   state EVENT-BUF BUF:DISPOSE
   CLOSED state ST! ;

;package
