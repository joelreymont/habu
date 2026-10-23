\ event-stream.f - incremental Server-Sent Events framing over arbitrary bytes,
\ by the HTML "Parsing an event stream" algorithm (section 9.2.6). No socket,
\ reconnect policy or event queue.
\
\ STORAGE CLASS. CALLER-OWNED: the caller owns the cell-aligned, zeroed
\ STORAGE-BYTES and closes each opened decoder. BUF owns its three growable
\ byte mappings; this package has no shared mutable state.
\
\ FEED's callback gets the event type, "message" by default, and the data as
\ spans borrowed for that call. FINISH discards an event with no closing blank
\ line, and FEED after it is E-STATE. Invalid UTF-8 in a completed event is
\ E-UTF8 before its callback. A callback's throw leaves the decoder failed:
\ close it and never feed it again. CLOSE frees the buffers, after a throw too,
\ and lets the storage open again. Comments, unknown fields, id and retry are
\ ignored.

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

CAST: >DECODER ( ptr n -- decoder )
CAST: DECODER>STATE ( decoder -- ptr n )

: ST@ ( ptr n -- n ) ST-OFF + @ ;
: ST! ( n ptr n -- ) ST-OFF + ! ;
: BOM@ ( ptr n -- n ) BOM-OFF + @ ;
: BOM! ( n ptr n -- ) BOM-OFF + ! ;
: CR@ ( ptr n -- n ) CR-OFF + @ ;
: CR! ( n ptr n -- ) CR-OFF + ! ;
: DATA? ( ptr n -- bool ) DATA-OFF + @ 0<> ;
: DATA! ( n ptr n -- ) DATA-OFF + ! ;
: LINE-BUF ( ptr n -- ptr n ) LINE-OFF + ;
: BODY-BUF ( ptr n -- ptr n ) BODY-OFF + ;
: EVENT-BUF ( ptr n -- ptr n ) EVENT-OFF + ;

: CHECK-LIVE ( ptr n -- ) ST@ LIVE <> if E-STATE throw then ;
: CHECK-OPEN ( ptr n -- )
   ST@ dup CLOSED = if drop E-STATE throw then
   dup LIVE = if drop exit then
   dup FINISHED = if drop exit then
   FAILED = if exit then
   E-STATE throw ;

: ZERO-BUF ( ptr n -- )
   {: buf:ptr :}
   0 buf ! 0 buf CELL + ! 0 buf 2 cells + ! ;

: INIT-BUF ( ptr n -- ) START-CAP BUF:N>BLEN BUF:INIT ;

: INIT-BUFS ( ptr n -- ptr n )
   {: state:ptr :}
   state LINE-BUF INIT-BUF
   state BODY-BUF INIT-BUF
   state EVENT-BUF INIT-BUF
   state ;

: DISPOSE-IF-LIVE ( ptr n -- )
   {: buf:ptr :}
   buf 2 cells + @ 0<> if buf BUF:DISPOSE then ;

: DISPOSE-BUFS ( ptr n -- )
   {: state:ptr :}
   state LINE-BUF DISPOSE-IF-LIVE
   state BODY-BUF DISPOSE-IF-LIVE
   state EVENT-BUF DISPOSE-IF-LIVE ;

: RESET-EVENT ( ptr n -- )
   {: state:ptr :}
   state BODY-BUF BUF:CLEAR
   state EVENT-BUF BUF:CLEAR
   0 state DATA! ;

: DISPATCH ( ptr n ptr a [ ptr a ptr u8 n ptr u8 n -- ] -- )
   {: state:ptr ctx:ptr cb :}
   state DATA? 0= if state RESET-EVENT exit then
   FAILED state ST!
   state EVENT-BUF BUF:SPAN$ BUF:BLEN>N {: kind:ptr ku:n :}
   state BODY-BUF BUF:SPAN$ BUF:BLEN>N {: body:ptr bu:n :}
   kind ku UTF8:VALID? 0= if E-UTF8 throw then
   body bu 1- UTF8:VALID? 0= if E-UTF8 throw then
   ctx
   ku 0= if s" message" else kind ku then
   body bu 1- cb execute
   state RESET-EVENT
   LIVE state ST! ;

: COLON-AT ( ptr u8 n -- n )
   {: src:ptr u:n :}
   u 0 ?do src i + c@ COLON = if i unloop exit then loop
   u ;

: SPLIT-FIELD ( ptr u8 n -- ptr u8 n ptr u8 n )
   {: src:ptr u:n :}
   src u COLON-AT {: at:n :}
   at u = if src u src u + 0 exit then
   src at
   src at 1+ + u at 1+ - {: value:ptr vu:n :}
   vu 0 > if value c@ SP = if value 1+ vu 1- exit then then
   value vu ;

: FIELD-LINE ( ptr n -- )
   {: state:ptr :}
   state LINE-BUF BUF:SPAN$ BUF:BLEN>N {: src:ptr u:n :}
   src c@ COLON = if exit then
   src u SPLIT-FIELD {: name:ptr nu:n value:ptr vu:n :}
   name nu s" data" STR= if
      value vu BUF:N>BLEN state BODY-BUF BUF:APPEND-SPAN
      LF state BODY-BUF BUF:APPEND-BYTE
      1 state DATA!
      exit
   then
   name nu s" event" STR= if
      value vu BUF:N>BLEN state EVENT-BUF BUF:REPLACE
   then ;

: END-LINE ( ptr n ptr a [ ptr a ptr u8 n ptr u8 n -- ] -- )
   {: state:ptr ctx:ptr cb :}
   state LINE-BUF BUF:LEN@ BUF:BLEN>N 0= if
      state ctx cb DISPATCH
   else
      state FIELD-LINE
   then
   state LINE-BUF BUF:CLEAR ;

: BYTE ( ptr n n ptr a [ ptr a ptr u8 n ptr u8 n -- ] -- )
   {: state:ptr byte:n ctx:ptr cb :}
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

: BOM-BYTE ( ptr n n ptr a [ ptr a ptr u8 n ptr u8 n -- ] -- )
   {: state:ptr byte:n ctx:ptr cb :}
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

public

: STORAGE-BYTES ( -- n ) SIZE ;

\ Storage is zero-initialized before its first OPEN and remains exclusive until
\ CLOSE. CLOSE clears the liveness cell so the same storage can be reopened.
: OPEN ( ptr n n -- decoder )
   {: state:ptr cap:n :}
   state 0= if E-STORAGE throw then
   state NULL-PTR - CELL 1- and 0 <> if E-STORAGE throw then
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

: FEED ( decoder ptr u8 n ptr a [ ptr a ptr u8 n ptr u8 n -- ] -- )
   {: d:decoder src:ptr u:n ctx:ptr cb :}
   d DECODER>STATE {: state:ptr :}
   state CHECK-LIVE
   u 0 < if E-SOURCE throw then
   u 0 > src 0= and if E-SOURCE throw then
   u 0 ?do state src i + c@ ctx cb BOM-BYTE loop ;

: FINISH ( decoder -- )
   DECODER>STATE {: state:ptr :}
   state CHECK-LIVE
   FINISHED state ST! ;

: CLOSE ( decoder -- )
   DECODER>STATE {: state:ptr :}
   state CHECK-OPEN
   state LINE-BUF BUF:DISPOSE
   state BODY-BUF BUF:DISPOSE
   state EVENT-BUF BUF:DISPOSE
   CLOSED state ST! ;

;package
