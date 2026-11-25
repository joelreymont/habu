\ http-arena.f - per-worker storage for the HTTP/1.1 server
\ (src/lib/net/http.f): one OS-backed span per worker, carved into the input
\ buffer, the fixed buffers and the headers of three growing ones; the JSON
\ vector is held beside it, and the request body owns a block while a request
\ is live.
\ STORAGE CLASS. PROCESS-WIDE tables indexed by worker slot, rows of typed
\ buffers and never bare globals; the running worker's slot is a TASK:+USER row.

\ A narrowing past the region throws E-SPAN-RANGE instead of reaching the next
\ slot. No socket here.

require src/lib/error.f
require src/lib/prelude.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/type/deftype.f
require src/lib/task.f
require src/lib/byte-buffer.f
require src/lib/heap.f
require src/lib/vector.f

package HTTP

public
E-HTTP-STATE constant E-STATE
E-HTTP-HANDLE constant E-HANDLE
E-HTTP-WORKERS constant E-WORKERS
E-HTTP-CAPACITY constant E-CAPACITY
E-HTTP-ROUTE constant E-ROUTE
E-HTTP-STATIC constant E-STATIC
E-HTTP-SOCKET constant E-SOCKET

\ A worker slot under two names: the request it is parsing and the response it
\ is building. Both carry the slot's generation, so a handle kept past the
\ request it belonged to is refused instead of addressing the next one.
DEFTYPE REQUEST
DEFTYPE RESPONSE

\ The worker count and the request-head limits below are the server's defence
\ against a hostile client, as nginx's header limits are: they stay fixed.
8 constant MAX-WORKERS
64 constant MAX-HEADERS
8 constant MAX-SEGMENTS

$1C00 constant HEAD-CAP          \ the header block limit answered with 431
$400 constant STREAM-CAP         \ chunk framing and the head of a pipelined request
HEAD-CAP STREAM-CAP + constant IN-CAP
$100000 constant DEFAULT-BODY-CAP
$2000 constant BODY-START-CAP    \ small block, grown only for the current request
$20 constant ID-CAP              \ the request id a fault is reported under
$18 constant NUM-CAP             \ one number on its way into a buffer

private

\ The growing buffers' first capacities. Each grows past its own while one
\ answer needs it; GIVE-BACK says when all three return to them.
$800 constant HDR-START          \ response header lines
$100000 constant JSON-START      \ one generated body: what JSON! writes, or the error text
$1000 constant OUT-START         \ status line plus headers of one response, or a fault line

\ Decoded path segments are never longer than the path they decode, which is
\ part of a head no longer than HEAD-CAP, so this one stays fixed.
HEAD-CAP constant SEG-CAP

IN-CAP constant BODY-OFF
BODY-OFF BUF:HDR-BYTES + constant HDR-OFF
HDR-OFF BUF:HDR-BYTES + constant OUT-OFF
OUT-OFF BUF:HDR-BYTES + constant SEG-OFF
SEG-OFF SEG-CAP + constant ID-OFF
ID-OFF ID-CAP + constant NUM-OFF
NUM-OFF NUM-CAP + constant SPAN-BYTES

\ The slot is the low byte of a handle; everything above it is the generation
\ the slot had when the handle was minted.
8 constant GEN-SHIFT
$FF constant SLOT-MASK

\ Where a slot's access line stands: a zeroed row reads unrequested.
ENUM access unrequested unanswered undelivered delivered handed-off ;ENUM

MAX-WORKERS TYPED-BUFFER SLOT-REGION SPAN:span<u8> SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER SLOT-GEN n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER SLOT-LIVE n SCRATCH-ONLY
variable BODY-LIMIT
DEFAULT-BODY-CAP BODY-LIMIT !
variable BODY-TIMEOUT-MS             \ 0 shares START's deadline with the head

MAX-WORKERS TYPED-BUFFER IN-U n SCRATCH-ONLY \ bytes read into the input buffer
MAX-WORKERS TYPED-BUFFER IN-AT n SCRATCH-ONLY \ bytes of it already consumed
MAX-WORKERS TYPED-BUFFER HEAD-U n SCRATCH-ONLY \ bytes of the current request head
MAX-WORKERS TYPED-BUFFER GIVEN-BACK n SCRATCH-ONLY \ 1: the answer buffers wait for SLOT-RESET
\ What JSON! writes, or the error text; NULL while the slot has none.
MAX-WORKERS TYPED-BUFFER JSON-OUT ^VEC:vector<u8> SCRATCH-ONLY

MAX-WORKERS TYPED-BUFFER METHOD-AT n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER METHOD-N n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER PATH-AT n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER PATH-N n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER QUERY-AT n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER QUERY-N n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER VERSION-AT n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER VERSION-N n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER HEADER-N n SCRATCH-ONLY \ headers parsed into the slot's table
MAX-WORKERS TYPED-BUFFER SEGMENT-N n SCRATCH-ONLY \ segments the matched route bound
MAX-WORKERS TYPED-BUFFER ROUTE-AT n SCRATCH-ONLY \ the matched route, -1 while none
MAX-WORKERS TYPED-BUFFER KEEP-ALIVE n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER STATUS n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER BODY-PTR ^u8 SCRATCH-ONLY \ the response body, owned elsewhere
MAX-WORKERS TYPED-BUFFER BODY-LEN n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER FILE-FD fd SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER FILE-LIVE n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER FILE-BUFFER ^u8 SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER FILE-ALLOCATION NUM:alloc-byte-len SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER FILE-OWNED n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER SEG-U n SCRATCH-ONLY \ bytes of decoded segment text
MAX-WORKERS TYPED-BUFFER ID-U n SCRATCH-ONLY \ bytes of the current request id
MAX-WORKERS TYPED-BUFFER SEQ n SCRATCH-ONLY \ requests this worker has answered
MAX-WORKERS TYPED-BUFFER START-NS n SCRATCH-ONLY \ mono-ns at the current request's id
MAX-WORKERS TYPED-BUFFER ACCESS-STATE access SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER LATE-LEFT n SCRATCH-ONLY \ bytes a request may still read past its deadline

MAX-WORKERS MAX-HEADERS * constant HEADER-ROWS
HEADER-ROWS TYPED-BUFFER HEADER-NAME-AT n SCRATCH-ONLY
HEADER-ROWS TYPED-BUFFER HEADER-NAME-N n SCRATCH-ONLY
HEADER-ROWS TYPED-BUFFER HEADER-VALUE-AT n SCRATCH-ONLY
HEADER-ROWS TYPED-BUFFER HEADER-VALUE-N n SCRATCH-ONLY

MAX-WORKERS MAX-SEGMENTS * constant SEGMENT-ROWS
SEGMENT-ROWS TYPED-BUFFER SEGMENT-AT n SCRATCH-ONLY
SEGMENT-ROWS TYPED-BUFFER SEGMENT-LEN n SCRATCH-ONLY

TASK:+USER MY-SLOT n


: BOUNDED ( n -- n ) {: idx:n :}
   idx 0 < if E-HANDLE throw then
   idx MAX-WORKERS >= if E-HANDLE throw then
   idx ;


\ The whole region of one worker. An unopened slot answers a zero-reach span,
\ so every narrowing below it throws rather than addressing a null base.
: SLOT-SPAN ( n -- SPAN:span<u8> )
   BOUNDED SLOT-REGION @ ;


: HANDLE ( n -- n ) {: idx:n :}
   idx BOUNDED SLOT-GEN @ GEN-SHIFT lshift idx or ;


\ The slot handle h names, and whether h is that slot's current request.
: CURRENT? ( n -- n bool ) {: h:n :}
   h SLOT-MASK and {: idx:n :}
   idx MAX-WORKERS >= if idx false exit then
   idx  idx SLOT-LIVE @ 0<>  h GEN-SHIFT rshift idx SLOT-GEN @ =  and ;


: RESOLVE ( n -- n )
   CURRENT? 0= if E-HANDLE throw then ;


private

: SLOT-OF-REQUEST ( request -- n )
   REQUEST>N RESOLVE ;


: SLOT-OF-RESPONSE ( response -- n )
   RESPONSE>N RESOLVE ;


: REQUEST-OF ( n -- request )
   HANDLE >REQUEST ;


: RESPONSE-OF ( n -- response )
   HANDLE >RESPONSE ;


\ The running task's slot, the only one a handler reaches without a handle.
\ MY-SLOT holds it plus one: a task that never claimed a slot gets E-HANDLE.
: SELF-SLOT ( -- n )
   MY-SLOT @ 1- BOUNDED ;


: SELF-SLOT! ( n -- )
   BOUNDED 1+ MY-SLOT ! ;


: CARVE ( SPAN:span<u8> n -- ) {: region idx:n :}
   region idx SLOT-REGION !
   idx SLOT-GEN @ 1+ idx SLOT-GEN !
   1 idx SLOT-LIVE ! ;


: BUF-AT ( n n -- ^n ) {: idx:n off:n :}
   idx SLOT-SPAN off BUF:HDR-BYTES SPAN:SUB SPAN:$ STR:DATA CELL-VIEW ;


: BODY-HDR ( n -- ^n )    BODY-OFF BUF-AT ;
: HDR-BUF ( n -- ^n )     HDR-OFF BUF-AT ;
: OUT-BUF ( n -- ^n )     OUT-OFF BUF-AT ;


\ The slot's JSON vector deleted, if it has one.
: DROP-JSON ( n -- ) {: idx:n :}
   idx JSON-OUT @ VEC:DELETE
   NULL-PTR idx JSON-OUT ! ;


\ The slot's JSON vector deleted and made again with room for JSON-START bytes.
\ It is held before its room is reserved, so a refused reserve leaks nothing.
: REMAKE-JSON ( n -- ) {: idx:n :}
   idx DROP-JSON
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   v idx JSON-OUT !
   v JSON-START VEC:RESERVE ;


\ One span per worker, taken before any task is live and given back after every
\ task has ended. A slot is live once carved, so ARENA-CLOSE gives back whatever
\ an open that failed part way had taken: a header never opened is zero, which
\ is a disposed one, and a vector never made is NULL.
: ARENA-OPEN ( n -- ) {: workers:n :}
   workers 1 < if E-WORKERS throw then
   workers MAX-WORKERS > if E-WORKERS throw then
   workers 0 ?do
      SPAN-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN i CARVE
      0 i SEQ !
      0 i GIVEN-BACK !
      i HDR-BUF HDR-START BUF:N>BLEN BUF:INIT
      i REMAKE-JSON
      i OUT-BUF OUT-START BUF:N>BLEN BUF:INIT
   loop ;


: CLOSE-FILE ( n -- ) {: idx:n :}
   idx BOUNDED FILE-OWNED @ 0<> if
      0 idx FILE-OWNED !
      idx FILE-BUFFER @ idx FILE-ALLOCATION @ MEM:RELEASE-BYTES
   then
   idx FILE-LIVE @ 0<> if
      0 idx FILE-LIVE !
      idx FILE-FD @ FD>N close
   then ;


: BODY-CLOSE ( n -- )
   BODY-HDR BUF:DISPOSE ;


: ARENA-CLOSE ( -- )
   MAX-WORKERS 0 ?do
      i SLOT-LIVE @ 0 <> if
         i CLOSE-FILE
         i BODY-CLOSE
         i HDR-BUF BUF:DISPOSE
         i DROP-JSON
         i OUT-BUF BUF:DISPOSE
         i SLOT-REGION @ MEM:FREE-SPAN
         0 i SLOT-LIVE !
      then
   loop ;


\ Each fixed one narrows the worker's region to its own offset and capacity, so
\ the value a caller holds cannot reach the buffer next to it.

: IN-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN IN-CAP SPAN:TAKE ;


: BODY-OPEN ( n -- )
   BODY-HDR BODY-START-CAP BUF:N>BLEN BUF:INIT ;


: BODY-APPEND ( str n -- ) {: bytes:str idx:n :}
   bytes STR:DATA bytes STR:LEN BUF:N>BLEN
      idx BODY-HDR BUF:APPEND-SPAN ;


: SEG-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN SEG-OFF SEG-CAP SPAN:SUB ;


: ID-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN ID-OFF ID-CAP SPAN:SUB ;


: NUM-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN NUM-OFF NUM-CAP SPAN:SUB ;


: IN-U@ ( n -- n )        BOUNDED IN-U @ ;
: IN-U! ( n n -- )        BOUNDED IN-U ! ;
: IN-AT@ ( n -- n )       BOUNDED IN-AT @ ;
: IN-AT! ( n n -- )       BOUNDED IN-AT ! ;
: HEAD-U@ ( n -- n )      BOUNDED HEAD-U @ ;
: HEAD-U! ( n n -- )      BOUNDED HEAD-U ! ;
: BODY-U@ ( n -- n )      BODY-HDR BUF:LEN@ BUF:BLEN>N ;
: SLOT-STATUS@ ( n -- n ) BOUNDED STATUS @ ;
: SLOT-STATUS! ( n n -- ) BOUNDED STATUS ! ;
: KEEP-ALIVE@ ( n -- n )  BOUNDED KEEP-ALIVE @ ;
: KEEP-ALIVE! ( n n -- )  BOUNDED KEEP-ALIVE ! ;
: HEADER-N@ ( n -- n )    BOUNDED HEADER-N @ ;
: HEADER-N! ( n n -- )    BOUNDED HEADER-N ! ;
: SEGMENT-N@ ( n -- n )   BOUNDED SEGMENT-N @ ;
: SEGMENT-N! ( n n -- )   BOUNDED SEGMENT-N ! ;
: ROUTE-AT@ ( n -- n )    BOUNDED ROUTE-AT @ ;
: ROUTE-AT! ( n n -- )    BOUNDED ROUTE-AT ! ;
: SEG-U@ ( n -- n )       BOUNDED SEG-U @ ;
: SEG-U! ( n n -- )       BOUNDED SEG-U ! ;
: ID-U@ ( n -- n )        BOUNDED ID-U @ ;
: ID-U! ( n n -- )        BOUNDED ID-U ! ;
: BODY-LEN@ ( n -- n )    BOUNDED BODY-LEN @ ;
: START-NS@ ( n -- n )    BOUNDED START-NS @ ;
: START-NS! ( n n -- )    BOUNDED START-NS ! ;
: ACCESS-STATE@ ( n -- access ) BOUNDED ACCESS-STATE @ ;
: ACCESS-STATE! ( access n -- ) BOUNDED ACCESS-STATE ! ;
: LATE-LEFT@ ( n -- n )   BOUNDED LATE-LEFT @ ;
: LATE-LEFT! ( n n -- )   BOUNDED LATE-LEFT ! ;
: BODY-PTR@ ( n -- ^u8 ) BOUNDED BODY-PTR @ ;


: RESPONSE-BODY! ( str n -- ) {: bytes:str idx:n :}
   bytes STR:LEN 0 < if E-CAPACITY throw then
   idx CLOSE-FILE
   bytes STR:DATA idx BOUNDED BODY-PTR !
   bytes STR:LEN idx BODY-LEN ! ;


\ No body at all, borrowed from nowhere: what a slot answers before a body is
\ set, and what it must answer before the buffer a body borrowed is rewritten,
\ which may move it.
: DROP-BODY ( n -- ) {: idx:n :}
   idx IN-BUF 0 SPAN:TAKE SPAN:$ idx RESPONSE-BODY! ;


: BUF$ ( ^n -- str )  BUF:SPAN$ BUF:BLEN>N >str ;


: NEXT-SEQ ( n -- n ) {: idx:n :}
   idx BOUNDED SEQ @ 1+ {: next:n :}
   next idx SEQ !
   next ;


: METHOD! ( n n n -- ) {: at:n len:n idx:n :}
   at idx BOUNDED METHOD-AT ! len idx METHOD-N ! ;


: PATH! ( n n n -- ) {: at:n len:n idx:n :}
   at idx BOUNDED PATH-AT ! len idx PATH-N ! ;


: QUERY! ( n n n -- ) {: at:n len:n idx:n :}
   at idx BOUNDED QUERY-AT ! len idx QUERY-N ! ;


: VERSION! ( n n n -- ) {: at:n len:n idx:n :}
   at idx BOUNDED VERSION-AT ! len idx VERSION-N ! ;


: SLOT-METHOD$ ( n -- str ) {: idx:n :}
   idx IN-BUF idx BOUNDED METHOD-AT @ idx METHOD-N @ SPAN:SUB SPAN:$ ;


: SLOT-PATH$ ( n -- str ) {: idx:n :}
   idx IN-BUF idx BOUNDED PATH-AT @ idx PATH-N @ SPAN:SUB SPAN:$ ;


: SLOT-QUERY$ ( n -- str ) {: idx:n :}
   idx IN-BUF idx BOUNDED QUERY-AT @ idx QUERY-N @ SPAN:SUB SPAN:$ ;


: SLOT-VERSION$ ( n -- str ) {: idx:n :}
   idx IN-BUF idx BOUNDED VERSION-AT @ idx VERSION-N @ SPAN:SUB SPAN:$ ;


: SLOT-BODY$ ( n -- str ) {: idx:n :}
   idx BODY-HDR BUF:SPAN$ BUF:BLEN>N >str ;


: SLOT-ID$ ( n -- str ) {: idx:n :}
   idx ID-BUF idx ID-U@ SPAN:TAKE SPAN:$ ;


: HEADER-ROW ( n n -- n ) {: idx:n at:n :}
   at 0 < if E-CAPACITY throw then
   at MAX-HEADERS >= if E-CAPACITY throw then
   idx BOUNDED MAX-HEADERS * at + ;


: HEADER+ ( n n n n n -- ) {: name-at:n name-n:n value-at:n value-n:n idx:n :}
   idx HEADER-N@ {: at:n :}
   at MAX-HEADERS >= if E-CAPACITY throw then
   idx at HEADER-ROW {: row:n :}
   name-at row HEADER-NAME-AT !
   name-n row HEADER-NAME-N !
   value-at row HEADER-VALUE-AT !
   value-n row HEADER-VALUE-N !
   at 1+ idx HEADER-N! ;


: HEADER-NAME$ ( n n -- str ) {: idx:n at:n :}
   idx at HEADER-ROW {: row:n :}
   idx IN-BUF row HEADER-NAME-AT @ row HEADER-NAME-N @ SPAN:SUB SPAN:$ ;


: HEADER-VALUE$ ( n n -- str ) {: idx:n at:n :}
   idx at HEADER-ROW {: row:n :}
   idx IN-BUF row HEADER-VALUE-AT @ row HEADER-VALUE-N @ SPAN:SUB SPAN:$ ;


: SEGMENT-ROW ( n n -- n ) {: idx:n at:n :}
   at 0 < if E-CAPACITY throw then
   at MAX-SEGMENTS >= if E-CAPACITY throw then
   idx BOUNDED MAX-SEGMENTS * at + ;


\ A segment's value is decoded text in the slot's segment buffer, not a span of
\ the request line: percent escapes make the two different strings.
: SEGMENT+C ( n n -- ) {: byte:n idx:n :}
   byte idx SEG-BUF idx SEG-U@ SPAN:U8!
   idx SEG-U@ 1+ idx SEG-U! ;


: SEGMENT-OPEN ( n -- n ) {: idx:n :}
   idx SEG-U@ ;


: SEGMENT-CLOSE ( n n -- ) {: start:n idx:n :}
   idx SEGMENT-N@ {: count:n :}
   count MAX-SEGMENTS >= if E-CAPACITY throw then
   idx count SEGMENT-ROW {: row:n :}
   start row SEGMENT-AT !
   idx SEG-U@ start - row SEGMENT-LEN !
   count 1+ idx SEGMENT-N! ;


: SEGMENT$ ( n n -- str ) {: idx:n at:n :}
   idx at SEGMENT-ROW {: row:n :}
   idx SEG-BUF row SEGMENT-AT @ row SEGMENT-LEN @ SPAN:SUB SPAN:$ ;


\ True when the buffer has grown past its first capacity n.
: GROWN? ( ^n n -- bool ) {: buf:^ start:n :}
   buf BUF:CAP@ BUF:BLEN>N start > ;


\ Disposes the buffer, live or not, and makes it again at its first capacity n.
: REMAKE ( ^n n -- ) {: buf:^ start:n :}
   buf BUF:DISPOSE
   buf start BUF:N>BLEN BUF:INIT ;


\ Once any answer buffer has grown, all three are given back. Until SLOT-RESET
\ has made them again any of them may be disposed, so GROWN? does not read them.
: GIVE-BACK ( n -- ) {: idx:n :}
   idx GIVEN-BACK @ 0<> if exit then
   idx HDR-BUF HDR-START GROWN?
   idx JSON-OUT @ VEC:CAP JSON-START > or
   idx OUT-BUF OUT-START GROWN? or 0= if exit then
   1 idx GIVEN-BACK !
   idx HDR-BUF BUF:DISPOSE
   idx DROP-JSON
   idx OUT-BUF BUF:DISPOSE ;


\ A refused allocation leaves GIVEN-BACK set, so the next request tries again.
: MAKE-ANSWER ( n -- ) {: idx:n :}
   idx GIVEN-BACK @ 0= if exit then
   idx HDR-BUF HDR-START REMAKE
   idx REMAKE-JSON
   idx OUT-BUF OUT-START REMAKE
   0 idx GIVEN-BACK ! ;


\ True while the slot has its answer buffers to build a response in.
: ANSWER-MADE? ( n -- bool )  GIVEN-BACK @ 0= ;


\ Everything one request owns, cleared before it is parsed, so a request that
\ the grammar refuses cannot read the spans of the one before it. The input
\ buffer's own cursor survives: bytes of the next request may already be in it.
: SLOT-RESET ( n -- ) {: idx:n :}
   0 idx BOUNDED HEAD-U !
   0 idx METHOD-AT ! 0 idx METHOD-N !
   0 idx PATH-AT ! 0 idx PATH-N !
   0 idx QUERY-AT ! 0 idx QUERY-N !
   0 idx VERSION-AT ! 0 idx VERSION-N !
   idx MAKE-ANSWER
   idx HDR-BUF BUF:CLEAR
   idx OUT-BUF BUF:CLEAR
   0 idx SEG-U !
   0 idx ID-U !
   0 idx HEADER-N !
   0 idx SEGMENT-N !
   -1 idx ROUTE-AT !
   200 idx STATUS !
   0 idx BODY-LEN !
   1 idx KEEP-ALIVE !
   idx DROP-BODY
   idx BODY-OPEN ;


\ A request handle is valid through SEND or takeover release, then retired
\ before its body's block goes away. Disposal also accepts a partial read.
: SLOT-FINISH ( n -- ) {: idx:n :}
   idx BOUNDED SLOT-GEN @ 1+ idx SLOT-GEN !
   idx BODY-CLOSE
   idx GIVE-BACK ;


public

\ The configured incoming body maximum, in bytes; zero accepts empty bodies.
: BODY-CAP ( -- n ) BODY-LIMIT @ ;

\ Bounded by MAX-WORKERS: the index a handler package keys its own per-request
\ storage by. A task that is no request worker gets E-HANDLE.
: WORKER-SLOT ( -- n )
   SELF-SLOT ;


\ True only on the worker answering that request: a retired handle, another
\ worker's request or a task that is no worker answers false, never a throw.
: SERVING? ( request -- bool )
   REQUEST>N CURRENT? swap MY-SLOT @ 1- = and ;

;package
