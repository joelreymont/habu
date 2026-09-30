\ http-arena.f - per-worker storage for the HTTP/1.1 server (lib/net/http.f):
\ one OS-backed span per request worker, carved into the input, body, header,
\ JSON and output buffers one request needs, plus the nominal request and
\ response handles that name a worker's slot. Nothing here touches a socket.
\
\ STORAGE CLASS. PROCESS-WIDE tables indexed by the worker's own slot: every
\ per-request cell below is a row of a typed buffer, never a bare global,
\ because ordinary storage is one copy for every task (docs/threads.md). The
\ running worker's slot itself is a TASK:+USER row.
\
\ A worker's region is one span (lib/span.f): the base and the reach travel
\ together, every buffer below is a narrowing of it, and a narrowing past the
\ region throws E-SPAN-RANGE instead of addressing the next worker's slot.
require lib/errors.f
require lib/prelude.f
require lib/memory.f
require lib/span.f
require lib/type/deftype.f
require lib/task.f
require lib/json-read.f

package HTTP

public
E-HTTP-STATE constant E-STATE
E-HTTP-HANDLE constant E-HANDLE
E-HTTP-WORKERS constant E-WORKERS
E-HTTP-CAPACITY constant E-CAPACITY
E-HTTP-ROUTE constant E-ROUTE
E-HTTP-STATIC constant E-STATIC
E-HTTP-SOCKET constant E-SOCKET
E-HTTP-RESPONSE constant E-RESPONSE

\ A worker slot under two names: the request it is parsing and the response it
\ is building. Both carry the slot's generation, so a handle kept past the
\ request it belonged to is refused instead of addressing the next one.
DEFTYPE REQUEST
DEFTYPE RESPONSE

8 constant MAX-WORKERS
64 constant MAX-HEADERS
8 constant MAX-SEGMENTS

$1C00 constant HEAD-CAP          \ the header block limit answered with 431
$400 constant STREAM-CAP         \ chunk framing and the head of a pipelined request
HEAD-CAP STREAM-CAP + constant IN-CAP
$100000 constant BODY-CAP        \ the body limit answered with 413
$800 constant HDR-CAP            \ response header lines
$100000 constant JSON-CAP        \ one generated body: what JSON! writes, or the default error text
$1000 constant OUT-CAP           \ status line plus headers of one response
$100 constant SEG-CAP            \ the decoded path segments one route binds
$20 constant ID-CAP              \ the request id a fault is reported under
$18 constant NUM-CAP             \ one number on its way into a buffer
JR:STORAGE-BYTES constant READER-CAP   \ one JSON reader over one request body

private

IN-CAP constant BODY-OFF
BODY-OFF BODY-CAP + constant HDR-OFF
HDR-OFF HDR-CAP + constant JSON-OFF
JSON-OFF JSON-CAP + constant OUT-OFF
OUT-OFF OUT-CAP + constant SEG-OFF
SEG-OFF SEG-CAP + constant ID-OFF
ID-OFF ID-CAP + constant NUM-OFF
NUM-OFF NUM-CAP + constant READER-OFF
READER-OFF READER-CAP + constant SPAN-BYTES

\ The slot is the low byte of a handle; everything above it is the generation
\ the slot had when the handle was minted.
8 constant GEN-SHIFT
$FF constant SLOT-MASK

MAX-WORKERS TYPED-BUFFER SLOT-REGION SPAN:span<u8>
MAX-WORKERS TYPED-BUFFER SLOT-GEN n
MAX-WORKERS TYPED-BUFFER SLOT-LIVE n

MAX-WORKERS TYPED-BUFFER IN-U n           \ bytes read into the input buffer
MAX-WORKERS TYPED-BUFFER IN-AT n          \ bytes of it already consumed
MAX-WORKERS TYPED-BUFFER HEAD-U n         \ bytes of the current request head
MAX-WORKERS TYPED-BUFFER BODY-U n
MAX-WORKERS TYPED-BUFFER HDR-U n
MAX-WORKERS TYPED-BUFFER OUT-U n

MAX-WORKERS TYPED-BUFFER METHOD-AT n
MAX-WORKERS TYPED-BUFFER METHOD-N n
MAX-WORKERS TYPED-BUFFER PATH-AT n
MAX-WORKERS TYPED-BUFFER PATH-N n
MAX-WORKERS TYPED-BUFFER QUERY-AT n
MAX-WORKERS TYPED-BUFFER QUERY-N n
MAX-WORKERS TYPED-BUFFER VERSION-AT n
MAX-WORKERS TYPED-BUFFER VERSION-N n
MAX-WORKERS TYPED-BUFFER HEADER-N n       \ headers parsed into the slot's table
MAX-WORKERS TYPED-BUFFER SEGMENT-N n      \ segments the matched route bound
MAX-WORKERS TYPED-BUFFER ROUTE-AT n       \ the matched route, -1 while none
MAX-WORKERS TYPED-BUFFER KEEP-ALIVE n
MAX-WORKERS TYPED-BUFFER STATUS n
MAX-WORKERS TYPED-BUFFER BODY-PTR ptr u8  \ the response body, owned elsewhere
MAX-WORKERS TYPED-BUFFER BODY-LEN n
MAX-WORKERS TYPED-BUFFER FILE-FD fd
MAX-WORKERS TYPED-BUFFER FILE-LIVE n
MAX-WORKERS TYPED-BUFFER FILE-BUFFER ptr u8
MAX-WORKERS TYPED-BUFFER FILE-ALLOCATION NUM:alloc-byte-len
MAX-WORKERS TYPED-BUFFER FILE-OWNED n
MAX-WORKERS TYPED-BUFFER SEG-U n          \ bytes of decoded segment text
MAX-WORKERS TYPED-BUFFER ID-U n           \ bytes of the current request id
MAX-WORKERS TYPED-BUFFER SEQ n            \ requests this worker has answered

MAX-WORKERS MAX-HEADERS * constant HEADER-ROWS
HEADER-ROWS TYPED-BUFFER HEADER-NAME-AT n
HEADER-ROWS TYPED-BUFFER HEADER-NAME-N n
HEADER-ROWS TYPED-BUFFER HEADER-VALUE-AT n
HEADER-ROWS TYPED-BUFFER HEADER-VALUE-N n

MAX-WORKERS MAX-SEGMENTS * constant SEGMENT-ROWS
SEGMENT-ROWS TYPED-BUFFER SEGMENT-AT n
SEGMENT-ROWS TYPED-BUFFER SEGMENT-LEN n

\ The running worker's own slot: task-local, because ordinary storage is not.
TASK:#USER
CELL TASK:+USER MY-SLOT
drop


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


\ A handle names a live slot only while its generation still matches.
: RESOLVE ( n -- n ) {: h:n :}
   h SLOT-MASK and BOUNDED {: idx:n :}
   idx SLOT-LIVE @ 0= if E-HANDLE throw then
   h GEN-SHIFT rshift idx SLOT-GEN @ <> if E-HANDLE throw then
   idx ;


private

: SLOT-OF-REQUEST ( request -- n )
   REQUEST>N RESOLVE ;


: SLOT-OF-RESPONSE ( response -- n )
   RESPONSE>N RESOLVE ;


: REQUEST-OF ( n -- request )
   HANDLE >REQUEST ;


: RESPONSE-OF ( n -- response )
   HANDLE >RESPONSE ;


\ The slot of the task that is running, which is the only slot a handler may
\ reach without being handed a handle.
: SELF-SLOT ( -- n )
   MY-SLOT @ BOUNDED ;


: SELF-SLOT! ( n -- )
   BOUNDED MY-SLOT ! ;


: CARVE ( SPAN:span<u8> n -- ) {: region idx:n :}
   region idx SLOT-REGION !
   idx SLOT-GEN @ 1+ idx SLOT-GEN !
   1 idx SLOT-LIVE ! ;


\ One span per worker, taken before any task is live and given back after every
\ task has ended.
: ARENA-OPEN ( n -- ) {: workers:n :}
   workers 1 < if E-WORKERS throw then
   workers MAX-WORKERS > if E-WORKERS throw then
   workers 0 ?do
      SPAN-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN i CARVE
      0 i SEQ !
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


: ARENA-CLOSE ( -- )
   MAX-WORKERS 0 ?do
      i SLOT-LIVE @ 0 <> if
         i CLOSE-FILE
         i SLOT-REGION @ MEM:FREE-SPAN
         0 i SLOT-LIVE !
      then
   loop ;


\ ---- the slot's buffers ------------------------------------------------------

\ Each one narrows the worker's region to its own offset and capacity, so the
\ value a caller holds cannot reach the buffer next to it.

: IN-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN IN-CAP SPAN:TAKE ;


: BODY-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN BODY-OFF BODY-CAP SPAN:SUB ;


: HDR-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN HDR-OFF HDR-CAP SPAN:SUB ;


: JSON-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN JSON-OFF JSON-CAP SPAN:SUB ;


: OUT-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN OUT-OFF OUT-CAP SPAN:SUB ;


: SEG-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN SEG-OFF SEG-CAP SPAN:SUB ;


: ID-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN ID-OFF ID-CAP SPAN:SUB ;


: NUM-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN NUM-OFF NUM-CAP SPAN:SUB ;


\ Cell-aligned because every capacity before it is a whole number of cells and
\ the region itself is a mapping; lib/json-read.f refuses anything else.
: READER-BUF ( n -- SPAN:span<u8> )
   SLOT-SPAN READER-OFF READER-CAP SPAN:SUB ;


\ The reader's storage as lib/json-read.f takes it: a cell pointer and the
\ reach the span carries. The reader is the only writer there and owns its own
\ bound, so the crossing out of the span happens once, here.
: READER-CELLS ( n -- ptr n n )
   READER-BUF SPAN:$ swap CELL-VIEW swap ;


\ ---- the slot's cells --------------------------------------------------------

: IN-U@ ( n -- n )        BOUNDED IN-U @ ;
: IN-U! ( n n -- )        BOUNDED IN-U ! ;
: IN-AT@ ( n -- n )       BOUNDED IN-AT @ ;
: IN-AT! ( n n -- )       BOUNDED IN-AT ! ;
: HEAD-U@ ( n -- n )      BOUNDED HEAD-U @ ;
: HEAD-U! ( n n -- )      BOUNDED HEAD-U ! ;
: BODY-U@ ( n -- n )      BOUNDED BODY-U @ ;
: BODY-U! ( n n -- )      BOUNDED BODY-U ! ;
: HDR-U@ ( n -- n )       BOUNDED HDR-U @ ;
: HDR-U! ( n n -- )       BOUNDED HDR-U ! ;
: OUT-U@ ( n -- n )       BOUNDED OUT-U @ ;
: OUT-U! ( n n -- )       BOUNDED OUT-U ! ;
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
: BODY-PTR@ ( n -- ptr u8 ) BOUNDED BODY-PTR @ ;


: RESPONSE-BODY! ( ptr u8 n n -- ) {: bytes:ptr len:n idx:n :}
   len 0 < if E-CAPACITY throw then
   idx CLOSE-FILE
   bytes idx BOUNDED BODY-PTR !
   len idx BODY-LEN ! ;


: NEXT-SEQ ( n -- n ) {: idx:n :}
   idx BOUNDED SEQ @ 1+ {: next:n :}
   next idx SEQ !
   next ;


\ ---- the request line's spans ------------------------------------------------

: METHOD! ( n n n -- ) {: at:n len:n idx:n :}
   at idx BOUNDED METHOD-AT ! len idx METHOD-N ! ;


: PATH! ( n n n -- ) {: at:n len:n idx:n :}
   at idx BOUNDED PATH-AT ! len idx PATH-N ! ;


: QUERY! ( n n n -- ) {: at:n len:n idx:n :}
   at idx BOUNDED QUERY-AT ! len idx QUERY-N ! ;


: VERSION! ( n n n -- ) {: at:n len:n idx:n :}
   at idx BOUNDED VERSION-AT ! len idx VERSION-N ! ;


: SLOT-METHOD$ ( n -- ptr u8 n ) {: idx:n :}
   idx IN-BUF idx BOUNDED METHOD-AT @ idx METHOD-N @ SPAN:SUB SPAN:$ ;


: SLOT-PATH$ ( n -- ptr u8 n ) {: idx:n :}
   idx IN-BUF idx BOUNDED PATH-AT @ idx PATH-N @ SPAN:SUB SPAN:$ ;


: SLOT-QUERY$ ( n -- ptr u8 n ) {: idx:n :}
   idx IN-BUF idx BOUNDED QUERY-AT @ idx QUERY-N @ SPAN:SUB SPAN:$ ;


: SLOT-VERSION$ ( n -- ptr u8 n ) {: idx:n :}
   idx IN-BUF idx BOUNDED VERSION-AT @ idx VERSION-N @ SPAN:SUB SPAN:$ ;


: SLOT-BODY$ ( n -- ptr u8 n ) {: idx:n :}
   idx BODY-BUF idx BODY-U@ SPAN:TAKE SPAN:$ ;


: SLOT-ID$ ( n -- ptr u8 n ) {: idx:n :}
   idx ID-BUF idx ID-U@ SPAN:TAKE SPAN:$ ;


\ ---- the slot's header table -------------------------------------------------

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


: HEADER-NAME$ ( n n -- ptr u8 n ) {: idx:n at:n :}
   idx at HEADER-ROW {: row:n :}
   idx IN-BUF row HEADER-NAME-AT @ row HEADER-NAME-N @ SPAN:SUB SPAN:$ ;


: HEADER-VALUE$ ( n n -- ptr u8 n ) {: idx:n at:n :}
   idx at HEADER-ROW {: row:n :}
   idx IN-BUF row HEADER-VALUE-AT @ row HEADER-VALUE-N @ SPAN:SUB SPAN:$ ;


\ ---- the slot's bound path segments ------------------------------------------

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


: SEGMENT$ ( n n -- ptr u8 n ) {: idx:n at:n :}
   idx at SEGMENT-ROW {: row:n :}
   idx SEG-BUF row SEGMENT-AT @ row SEGMENT-LEN @ SPAN:SUB SPAN:$ ;


\ Everything one request owns, cleared before it is parsed, so a request that
\ the grammar refuses cannot read the spans of the one before it. The input
\ buffer's own cursor survives: bytes of the next request may already be in it.
: SLOT-RESET ( n -- ) {: idx:n :}
   0 idx BOUNDED HEAD-U !
   0 idx METHOD-AT ! 0 idx METHOD-N !
   0 idx PATH-AT ! 0 idx PATH-N !
   0 idx QUERY-AT ! 0 idx QUERY-N !
   0 idx VERSION-AT ! 0 idx VERSION-N !
   0 idx BODY-U !
   0 idx HDR-U !
   0 idx OUT-U !
   0 idx SEG-U !
   0 idx ID-U !
   0 idx HEADER-N !
   0 idx SEGMENT-N !
   -1 idx ROUTE-AT !
   200 idx STATUS !
   0 idx BODY-LEN !
   1 idx KEEP-ALIVE !
   idx IN-BUF 0 SPAN:TAKE SPAN:$ idx RESPONSE-BODY! ;


public

\ The slot of the worker that is running, bounded by MAX-WORKERS: the identity
\ a handler package indexes its own per-request storage by, because ordinary
\ storage is one copy for the whole process.
: WORKER-SLOT ( -- n )
   SELF-SLOT ;

;package
