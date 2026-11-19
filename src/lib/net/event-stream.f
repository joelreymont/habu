split-str
\ event-stream.f - incremental Server-Sent Events framing over arbitrary bytes, by
\ the HTML "Parsing an event stream" algorithm (9.2.6); no socket, reconnect or
\ queue, and comments, unknown fields, id and retry are ignored. FEED's callback
\ borrows the type ("message" by default) and data; invalid UTF-8 is E-UTF8 first.
\ FINISH drops an unterminated event; FEED after it is E-STATE. A callback's throw
\ leaves the decoder failed: CLOSE it. STORAGE CLASS. CALLER-OWNED (STORAGE-BYTES).

require src/lib/error.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/string.f
require src/lib/utf8-scalar.f
require src/lib/vector.f

package SSE
public

-9450 constant E-FIRST
-9459 constant E-LAST
-9450 s" a decoder was used out of turn: FEED or FINISH of one not live, CLOSE of one not open, or OPEN of storage still open"
   ERROR:FAULT E-STATE
-9451 s" OPEN was given no storage or fewer than STORAGE-BYTES bytes" ERROR:CODE E-STORAGE
-9452 s" FEED was given a negative length, or bytes at a null address" ERROR:CODE E-SOURCE
-9453 s" an event's type or data is not valid UTF-8" ERROR:CODE E-UTF8

NEWTYPE decoder

private

0 constant ST-OFF
1 cells constant BOM-OFF
2 cells constant CR-OFF
3 cells constant DATA-OFF
4 cells constant VECS-OFF

\ A vector not made is NULL-PTR in its cell.
STRUCTURE vectors DERIVE addr
   FIELD line ^VEC:vector<u8>
   FIELD body ^VEC:vector<u8>
   FIELD event ^VEC:vector<u8>
;STRUCTURE

VECS-OFF VECTORS-CELLS cells + constant SIZE

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

CAST: >DECODER ( ^n -- decoder )
CAST: DECODER>STATE ( decoder -- ^n )
CAST: >VECTORS ( ^n -- ^vectors )

: ST@ ( ^n -- n ) ST-OFF + @ ;
: ST! ( n ^n -- ) ST-OFF + ! ;
: BOM@ ( ^n -- n ) BOM-OFF + @ ;
: BOM! ( n ^n -- ) BOM-OFF + ! ;
: CR@ ( ^n -- n ) CR-OFF + @ ;
: CR! ( n ^n -- ) CR-OFF + ! ;
: DATA? ( ^n -- bool ) DATA-OFF + @ 0<> ;
: DATA! ( n ^n -- ) DATA-OFF + ! ;
: VECS ( ^n -- ^vectors ) VECS-OFF + >VECTORS ;
: LINE ( ^n -- ^VEC:vector<u8> ) VECS VECTORS-LINE @ ;
: BODY ( ^n -- ^VEC:vector<u8> ) VECS VECTORS-BODY @ ;
: EVENT ( ^n -- ^VEC:vector<u8> ) VECS VECTORS-EVENT @ ;

: CHECK-LIVE ( ^n -- ) ST@ LIVE <> if E-STATE throw then ;
: CHECK-OPEN ( ^n -- )
   ST@ dup CLOSED = if drop E-STATE throw then
   dup LIVE = if drop exit then
   dup FINISHED = if drop exit then
   FAILED = if exit then
   E-STATE throw ;

: NULL-VECS ( ^n -- )
   VECS {: v:^ :}
   NULL-PTR v VECTORS-LINE !
   NULL-PTR v VECTORS-BODY !
   NULL-PTR v VECTORS-EVENT ! ;

: FRESH ( ^^VEC:vector<u8> -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW swap ! ;

: MAKE-VECS ( ^n -- ^n )
   {: state:^ :}
   state VECS {: v:^ :}
   v VECTORS-LINE FRESH
   v VECTORS-BODY FRESH
   v VECTORS-EVENT FRESH
   state ;

: DELETE-VECS ( ^n -- )
   {: state:^ :}
   state LINE VEC:DELETE
   state BODY VEC:DELETE
   state EVENT VEC:DELETE ;

: RESET-EVENT ( ^n -- )
   {: state:^ :}
   state BODY 0 VEC:RESIZE
   state EVENT 0 VEC:RESIZE
   0 state DATA! ;

: DISPATCH ( ^n a [ a ^u8 n ^u8 n -- ] -- )
   {: state:^ ctx:a cb :}
   state DATA? 0= if state RESET-EVENT exit then
   FAILED state ST!
   state EVENT VEC:SPAN SPAN:$ {: kind:^ ku:n :}
   state BODY VEC:SPAN SPAN:$ {: body:^ bu:n :}
   kind ku UTF8:VALID? 0= if E-UTF8 throw then
   body bu 1- UTF8:VALID? 0= if E-UTF8 throw then
   ctx
   ku 0= if s" message" else kind ku then
   body bu 1- cb execute
   state RESET-EVENT
   LIVE state ST! ;

: COLON-AT ( str -- n )
   {: src:^ u:n :}
   u 0 ?do src i + c@ COLON = if i unloop exit then loop
   u ;

: SPLIT-FIELD ( str -- str str )
   {: src:^ u:n :}
   src u COLON-AT {: at:n :}
   at u = if src u src u + 0 exit then
   src at
   src at 1+ + u at 1+ - {: value:^ vu:n :}
   vu 0 > if value c@ SP = if value 1+ vu 1- exit then then
   value vu ;

: FIELD-LINE ( ^n -- )
   {: state:^ :}
   state LINE VEC:SPAN SPAN:$ {: src:^ u:n :}
   src c@ COLON = if exit then
   src u SPLIT-FIELD {: name:^ nu:n value:^ vu:n :}
   name nu s" data" STR= if
      value vu state BODY VEC:APPEND
      LF state BODY VEC:PUSH c!
      1 state DATA!
      exit
   then
   name nu s" event" STR= if
      state EVENT 0 VEC:RESIZE
      value vu state EVENT VEC:APPEND
   then ;

: END-LINE ( ^n a [ a str str -- ] -- )
   {: state:^ ctx:a cb :}
   state LINE VEC:COUNT 0= if
      state ctx cb DISPATCH
   else
      state FIELD-LINE
   then
   state LINE 0 VEC:RESIZE ;

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
   byte state LINE VEC:PUSH c! ;

: BOM-BYTE ( ^n n a [ a str str -- ] -- )
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
: TERM-AT ( str -- n )
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
         src run state LINE VEC:APPEND
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
   state NULL-VECS
   state [: MAKE-VECS ;] catch {: code:n :} drop
   code 0<> if state DELETE-VECS code throw then
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
   state DELETE-VECS
   CLOSED state ST! ;

;package
