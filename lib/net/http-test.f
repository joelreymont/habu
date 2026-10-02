\ http-test.f - the HTTP/1.1 server answered over a real loopback port.
\
\ Package CURL is the client for the ordinary response classes, and a raw TCP4
\ connection is the client for the requests a well-behaved client cannot send
\ and for the heads CURL does not hand back. Where a raw case pins a whole
\ response head, its status line and headers go into a transcript, written to
\ build/http-transcript.txt, read back and compared whole with the transcript
\ this file expects. The server that answers the transcript runs one worker, so
\ every request id - and with it every error body's length - is the same on
\ every run.
\
\ Every handler is defined before the server starts, because Habu forbids
\ dictionary mutation while a task is live.
\ Run: bin/hb --load lib/net/http-test.f
require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/ffi-abi.f
require lib/task.f
require lib/aio.f
require lib/net/tcp4.f
require lib/net/curl.f
require lib/json-write.f
require lib/json-read.f
require lib/num-types.f
require lib/span.f
require lib/net/http.f

\ Force the real startup semaphore initializer to refuse after the job queue
\ has opened. These helpers exist only in this test image.
package HTTP
public
: TEST-PREOPEN ( -- )
   0 START-READY TASK:SEMAPHORE-INIT ;

: TEST-UNINJECT ( -- )
   START-READY TASK:SEMAPHORE-DESTROY ;
;package

\ White-box helpers: reopens HTTP so the count of connections its listener has
\ taken (TAKEN) and its job queue's room plus one (QUEUED), which no public word
\ answers, are visible here. QUEUED follows JOB-SLOTS, so the full-queue case
\ still leaves the listener holding one more if the queue grows. The
\ definitions land in HTTP-TEST, so HTTP's public surface gains nothing for
\ being tested.
package HTTP
: HTTP-TEST:TAKEN ( -- n )
   ACCEPTED-COUNT atomic@ ;
: HTTP-TEST:QUEUED ( -- n ) JOB-SLOTS 1+ ;
;package

package HTTP-TEST

private

$7F000001 constant LOOPBACK
1 constant ONE-WORKER             \ the transcript's server: one request order
2 constant TWO-WORKERS
$1F4 constant IDLE-MS             \ short, so a keep-alive case ends the test
$4000 constant RES-CAP
$3000 constant REQ-CAP
$2800 constant BIG-HEADER-BYTES   \ past HTTP:HEAD-CAP
$41 constant FILL-BYTE
$80 constant ETAG-CAP             \ the longest ETag a case keeps
$2000 constant SCRIPT-CAP         \ the whole transcript
13 constant CR
10 constant LF
$2F constant SLASH
$2E constant DOT-BYTE
-9990 constant E-BOOM             \ this file's own fixture refusals, outside every lib block
-9991 constant E-HOOK             \ one controlled start hook failure
-9992 constant E-RENDER           \ the error hook that is meant to fail its answer
-9993 constant E-HOOK-OTHER
-9994 constant E-EXIT-HOOK        \ the exit hook that is meant to fail

CAST: BLEN>N ( NUM:byte-len -- n )

create RES-BUF RES-CAP allot
REQ-CAP SPAN-BUFFER: REQ-BUF
FS-PATH-CAP SPAN-BUFFER: ROOT-BUF
create PATH-BUF FS-PATH-CAP allot
ETAG-CAP SPAN-BUFFER: ETAG-BUF
SCRIPT-CAP SPAN-BUFFER: SCRIPT-BUF  \ the transcript this run captured
SCRIPT-CAP SPAN-BUFFER: WANT-BUF    \ the transcript this file expects
SCRIPT-CAP SPAN-BUFFER: BACK-BUF    \ the transcript read back from its file

: TEST-ALIGN8 ( -- )
   here FFI:>CELL 7 and 8 swap - 7 and allot ;

TEST-ALIGN8
variable ROOT-U
variable RES-U
variable REQ-U
variable ETAG-U
variable SCRIPT-U
variable WANT-U
variable BOOM-HITS
variable THROWN                   \ entries into the controlled start hook
variable HOOK-FINISHED
variable HOOK-MODE                \ 0: pass, 1: one fails, 2: both fail, 3: gated
variable START-DONE
variable START-OBSERVED
variable FAILED-PORT
TASK:SEMAPHORE HOOK-READY
TASK:SEMAPHORE HOOK-GATE
TASK:MIN-STACK TASK:TASK HOOK-RELEASER
variable PARKED                   \ set by the handler that never answers


: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ SPAN:TAKE SPAN:$ ;


: RES$ ( -- ptr u8 n )
   RES-BUF RES-U @ ;


: REQ$ ( -- ptr u8 n )
   REQ-BUF REQ-U @ SPAN:TAKE SPAN:$ ;


: ETAG$ ( -- ptr u8 n )
   ETAG-BUF ETAG-U @ SPAN:TAKE SPAN:$ ;


\ ---- the handlers, all defined before any task is live -----------------------

: PING-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" service" s" http-test" JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;


: PING ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   answer 200 HTTP:STATUS!
   answer [: PING-BODY ;] HTTP:JSON! ;


\ The id the pattern bound travels to the body through these cells, which the
\ body quotation reads back: a quotation cannot read a local.
PTR-VARIABLE ITEM-ID-A
variable ITEM-ID-N
PTR-VARIABLE ITEM-QUERY-A
variable ITEM-QUERY-N
PTR-VARIABLE ITEM-HOST-A
variable ITEM-HOST-N


: ITEM-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" id" ITEM-ID-A @ ITEM-ID-N @ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" query" ITEM-QUERY-A @ ITEM-QUERY-N @ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" host" ITEM-HOST-A @ ITEM-HOST-N @ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;


\ Reads the whole request record: the segment the pattern bound, the query
\ string and one named header.
: ITEM ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   asked s" id" HTTP:SEGMENT-OF$ {: ida:ptr idu:n found:bool :}
   found 0= if answer 500 s" no_segment" s" the route bound no id" HTTP:ERROR! exit then
   ida ITEM-ID-A !
   idu ITEM-ID-N !
   asked HTTP:QUERY$ {: qa:ptr qu:n :}
   qa ITEM-QUERY-A !
   qu ITEM-QUERY-N !
   asked s" host" HTTP:HEADER-OF$ {: ha:ptr hu:n seen:bool :}
   ha ITEM-HOST-A !
   seen if hu else 0 then ITEM-HOST-N !
   answer 200 HTTP:STATUS!
   answer [: ITEM-BODY ;] HTTP:JSON! ;


\ The version travels to its body the same way.
PTR-VARIABLE VERSION-A
variable VERSION-N

: VERSION-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" version" VERSION-A @ VERSION-N @ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;


\ The version the request line named, as the client sent it.
: ASKED-VERSION ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked HTTP:VERSION$ {: va:ptr vu:n :}
   va VERSION-A !
   vu VERSION-N !
   answer 200 HTTP:STATUS!
   answer [: VERSION-BODY ;] HTTP:JSON! ;


$40 constant TEXT-CAP
create TEXT-BUF TEXT-CAP allot
variable TEXT-N
variable ECHO-LEN


: ECHO-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" bytes" ECHO-LEN @ JSON-WRITE:FIELD-U
   JSON-WRITE:OBJECT-END ;


: ECHO ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   asked HTTP:BODY$ {: bytes:ptr len:n :}
   len ECHO-LEN !
   answer 200 HTTP:STATUS!
   answer [: ECHO-BODY ;] HTTP:JSON! ;


\ The request body read back through lib/json-read.f, on the reader the server
\ hands out over the slot's own storage. A body that is not JSON at all throws
\ out of the reader, which the worker's catch boundary answers with 500.
: TEXT-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" text" TEXT-BUF TEXT-N @ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;


: READ-TEXT ( HTTP:request -- n ) {: asked:HTTP:request :}
   asked HTTP:BODY-READER
   JR:NEXT JR:T-OBJ <> if JR:CLOSE -1 exit then
   s" text" JR:FIND-KEY 0= if JR:CLOSE -2 exit then
   TEXT-BUF TEXT-CAP JR:STR {: len:n :}
   JR:CLOSE
   len ;


: ANSWER-TEXT ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked READ-TEXT {: len:n :}
   len 0 < if answer 400 s" bad_json"
      s" the body is not an object carrying text" HTTP:ERROR! exit then
   len TEXT-N !
   answer 200 HTTP:STATUS!
   answer [: TEXT-BODY ;] HTTP:JSON! ;


: BOOM ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   1 BOOM-HITS +!
   E-BOOM throw ;


\ Throws with this worker's JSON writer open and half filled: the writer must be
\ closed and reopened empty, or the next answer would carry this one's fragment.
: BOOM-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" partial" s" yes" JSON-WRITE:FIELD-S
   E-BOOM throw ;


: BOOM-JSON ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   1 BOOM-HITS +!
   answer 200 HTTP:STATUS!
   answer [: BOOM-BODY ;] HTTP:JSON! ;


\ One value larger than the slot's whole JSON buffer, which the writer refuses
\ rather than truncating.
HTTP:JSON-CAP $1000 + constant OVER-CAP
create OVER-BUF OVER-CAP allot
variable OVERFLOW-CODE
1 TYPED-BUFFER OVERFLOW-ANSWER HTTP:response


: FILL-OVER ( -- )
   OVER-CAP 0 ?do FILL-BYTE OVER-BUF i + c! loop ;


: OVERFLOW-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" big" OVER-BUF OVER-CAP JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;


\ The quotation cannot read a local, so the response travels through the cell
\ the handler has just filled.
: OVERFLOW-JSON ( -- )
   0 OVERFLOW-ANSWER @ [: OVERFLOW-BODY ;] HTTP:JSON! ;


\ The refusal is this package's own named response failure, and the writer it
\ came out of is closed, so the error body below is written into an empty one.
: OVERFLOW ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   answer 0 OVERFLOW-ANSWER !
   answer 200 HTTP:STATUS!
   [: OVERFLOW-JSON ;] catch OVERFLOW-CODE !
   answer 500 s" too_large" s" the answer does not fit this server's JSON buffer"
   HTTP:ERROR! ;


\ The same body from a handler that catches nothing: the worker's own boundary
\ answers it.
: OVERFLOW-RAW ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   answer 200 HTTP:STATUS!
   answer [: OVERFLOW-BODY ;] HTTP:JSON! ;


\ ---- an installed error renderer ----------------------------------------------

\ What a caller's ON-ERROR hook looks like: a JSON body carrying the code, the
\ message and the request id. They travel to the body quotation through rows of
\ the running worker's own slot, because the quotation cannot read a local.
HTTP:MAX-WORKERS TYPED-BUFFER ERR-ANSWER HTTP:response
HTTP:MAX-WORKERS TYPED-BUFFER ERR-CODE-A ptr u8
HTTP:MAX-WORKERS TYPED-BUFFER ERR-CODE-N n
HTTP:MAX-WORKERS TYPED-BUFFER ERR-MESSAGE-A ptr u8
HTTP:MAX-WORKERS TYPED-BUFFER ERR-MESSAGE-N n


: JSON-ERROR-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   HTTP:WORKER-SLOT {: idx:n :}
   JSON-WRITE:OBJECT-START
   s" code" idx ERR-CODE-A @ idx ERR-CODE-N @ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" message" idx ERR-MESSAGE-A @ idx ERR-MESSAGE-N @ JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA
   s" request_id" idx ERR-ANSWER @ HTTP:REQUEST-ID$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;


: JSON-ERROR ( HTTP:response n ptr u8 n ptr u8 n -- )
   {: answer:HTTP:response status:n code:ptr codelen:n message:ptr messagelen:n :}
   HTTP:WORKER-SLOT {: idx:n :}
   answer idx ERR-ANSWER !
   code idx ERR-CODE-A !
   codelen idx ERR-CODE-N !
   message idx ERR-MESSAGE-A !
   messagelen idx ERR-MESSAGE-N !
   answer status HTTP:STATUS!
   answer [: JSON-ERROR-BODY ;] HTTP:JSON! ;


\ ---- installed static rules ---------------------------------------------------

\ Only a path the build hashes may be cached for a year.
: IMMUTABLE-CACHE ( ptr u8 n -- ptr u8 n ) {: path:ptr len:n :}
   path len s" /_app/immutable/" STARTS-WITH?
   if s" public, max-age=31536000, immutable" exit then
   s" no-cache" ;


: EXTENSION? ( ptr u8 n -- bool ) {: path:ptr len:n :}
   len 0 ?do
      path len 1- i - + c@ {: byte:n :}
      byte SLASH = if false unloop exit then
      byte DOT-BYTE = if true unloop exit then
   loop
   false ;


\ A single-page application's client routes: anything outside /api/ whose last
\ segment names no file extension is answered with the one page.
: CLIENT-ROUTE ( ptr u8 n -- ptr u8 n ) {: path:ptr len:n :}
   path len s" /api/" STARTS-WITH? if path 0 exit then
   path len EXTENSION? if path 0 exit then
   s" /index.html" ;


: INSTALL-POLICY ( -- )
   [: JSON-ERROR ;] HTTP:ON-ERROR
   [: IMMUTABLE-CACHE ;] HTTP:CACHE-RULE!
   [: CLIENT-ROUTE ;] HTTP:FALLBACK-RULE! ;


\ An error hook that starts its answer and throws before finishing it. The
\ worker catches the throw itself: the client is answered in plain text with
\ the status it was owed and nothing of the header this hook added.
: RENDER-THROWS ( HTTP:response n ptr u8 n ptr u8 n -- )
   {: answer:HTTP:response status:n code:ptr codelen:n message:ptr messagelen:n :}
   answer s" X-Hook" s" partial" HTTP:HEADER!
   E-RENDER throw ;


\ ---- the worker lifecycle hooks ----------------------------------------------

\ Every hook and the handler write one mark into the row of the worker they run
\ in, which is the only storage a live task may write, so the order of a
\ worker's own starts, requests and exits is read back from its row.
HTTP:MAX-WORKERS constant SLOTS
$10 constant TRACE-CAP

$61 constant MARK-START-1         \ 'a'
$62 constant MARK-START-2         \ 'b'
$72 constant MARK-REQUEST         \ 'r'
$74 constant MARK-THROWER         \ 't'
$78 constant MARK-EXIT-1          \ 'x'
$79 constant MARK-EXIT-2          \ 'y'
$7A constant MARK-EXIT-THROWS     \ 'z'

create TRACE-ROWS SLOTS TRACE-CAP * allot
SLOTS TYPED-BUFFER TRACE-U n


: TRACE-AT ( n -- ptr u8 )
   TRACE-CAP * TRACE-ROWS + ;


: TRACE$ ( n -- ptr u8 n ) {: slot:n :}
   slot TRACE-AT slot TRACE-U @ ;


: TRACE+C ( n n -- ) {: byte:n slot:n :}
   slot TRACE-U @ 1+ TRACE-CAP > if E-BOOM throw then
   byte slot TRACE-AT slot TRACE-U @ + c!
   slot TRACE-U @ 1+ slot TRACE-U ! ;


: TRACE-RESET ( -- )
   SLOTS 0 ?do 0 i TRACE-U ! loop ;


: MARK ( n -- ) {: byte:n :}
   byte HTTP:WORKER-SLOT TRACE+C ;


: MARK-COUNT ( n n -- n ) {: slot:n byte:n :}
   0
   slot TRACE-U @ 0 ?do
      slot TRACE-AT i + c@ byte = if 1+ then
   loop ;


: START-1 ( -- )   MARK-START-1 MARK ;
: START-2 ( -- )   MARK-START-2 MARK ;
: EXIT-1 ( -- )    MARK-EXIT-1 MARK ;
: EXIT-2 ( -- )    MARK-EXIT-2 MARK ;


\ Fails in every worker's exit, after its mark.
: EXIT-THROWS ( -- )
   MARK-EXIT-THROWS MARK
   E-EXIT-HOOK throw ;


\ The active fixture changes between servers, while registration remains fixed.
: START-THROWS ( -- )
   MARK-THROWER MARK
   1 THROWN atomic-add {: at:n :}
   HOOK-MODE @ case
      1 of at 0= if E-HOOK throw then endof
      2 of at 0= if E-HOOK throw else E-HOOK-OTHER throw then endof
      3 of HOOK-READY TASK:SIGNAL HOOK-GATE TASK:WAIT
           1 HOOK-FINISHED atomic-add drop endof
   endcase ;


$2710 constant PARK-MS            \ far past every stop bound


\ Never answers: the worker that takes this request is still inside the handler
\ when the stop comes, parked in an AIO wait as a worker serving a slow peer
\ is, which is the one thing a cooperative stop cannot wait out. Its kill ends
\ it at the TASK:PAUSE inside AIO:AWAIT.
: PARK ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   1 PARKED !
   PARK-MS >MS AIO:TIMEOUT AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;


: TRACE-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" trace" HTTP:WORKER-SLOT TRACE$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;


\ Answers the marks its own worker has left, this request's included.
: TRACE ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   MARK-REQUEST MARK
   answer 200 HTTP:STATUS!
   answer [: TRACE-BODY ;] HTTP:JSON! ;


: INSTALL-HOOKS ( -- )
   [: START-1 ;] HTTP:ON-WORKER-START
   [: START-2 ;] HTTP:ON-WORKER-START
   [: EXIT-1 ;] HTTP:ON-WORKER-EXIT
   [: EXIT-2 ;] HTTP:ON-WORKER-EXIT ;


\ A quotation cannot open another, so the refusal cases install through words.
: ADD-START-HOOK ( -- )
   [: START-1 ;] HTTP:ON-WORKER-START ;


: ADD-ERROR-HOOK ( -- )
   [: JSON-ERROR ;] HTTP:ON-ERROR ;


: INSTALL-ROUTES ( -- )
   HTTP:ROUTES-RESET
   s" GET" s" /api/trace" [: TRACE ;] HTTP:ROUTE
   s" GET" s" /api/ping" [: PING ;] HTTP:ROUTE
   s" GET" s" /api/items/{id}" [: ITEM ;] HTTP:ROUTE
   s" GET" s" /api/version" [: ASKED-VERSION ;] HTTP:ROUTE
   s" POST" s" /api/echo" [: ECHO ;] HTTP:ROUTE
   s" GET" s" /api/boom" [: BOOM ;] HTTP:ROUTE
   s" GET" s" /api/boom-json" [: BOOM-JSON ;] HTTP:ROUTE
   s" POST" s" /api/answer" [: ANSWER-TEXT ;] HTTP:ROUTE
   s" GET" s" /api/overflow" [: OVERFLOW ;] HTTP:ROUTE
   s" GET" s" /api/overflow-raw" [: OVERFLOW-RAW ;] HTTP:ROUTE
   s" GET" s" /api/park" [: PARK ;] HTTP:ROUTE ;


\ ---- the static fixture ------------------------------------------------------

: UNDER-ROOT ( ptr u8 n -- n ) {: name:ptr len:n :}
   ROOT$ name len PATH-BUF JOIN-PATH ;


: INDEX-TEXT$ ( -- ptr u8 n )
   S\" <!doctype html><title>http-test</title>\n" ;


: ASSET-TEXT$ ( -- ptr u8 n )
   S\" export const build = 1;\n" ;


: MAKE-STATIC ( -- )
   s" http-test" TMPDIR-MKDIR {: a:ptr u:n :}
   a u CLEANUP-TREE+
   a u ROOT-BUF SPAN:COPY
   u ROOT-U !
   s" index.html" UNDER-ROOT {: indexlen:n :}
   PATH-BUF indexlen INDEX-TEXT$ WRITE-ALL
   s" _app" UNDER-ROOT {: applen:n :}
   PATH-BUF applen MAKE-DIR
   s" _app/immutable" UNDER-ROOT {: immutablelen:n :}
   PATH-BUF immutablelen MAKE-DIR
   s" _app/immutable/app.js" UNDER-ROOT {: assetlen:n :}
   PATH-BUF assetlen ASSET-TEXT$ WRITE-ALL ;


\ ---- the CURL client ---------------------------------------------------------

: DROP-CURL-STATUS ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;


: OPEN-HANDLE ( -- CURL:handle )
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop -1 CURL:>HANDLE ENDOF
   ;MATCH ;


: URL-FOR ( ptr u8 n -- ptr u8 n ) {: path:ptr len:n :}
   SB-RESET
   s" http://127.0.0.1:" SB-APPEND
   HTTP:PORT FMT:SB-U
   path len SB-APPEND
   SB$ ;


: TAKE-RESPONSE ( CURL:handle -- n ) {: subject:CURL:handle :}
   subject RES-BUF RES-CAP >LEN CURL:PERFORM
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         got LEN>N RES-U ! status CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop 0 RES-U ! -1 ENDOF
      failed OF drop 0 RES-U ! -2 ENDOF
   ;MATCH ;


: FETCH ( ptr u8 n ptr u8 n -- n ) {: method:ptr methodlen:n path:ptr pathlen:n :}
   OPEN-HANDLE {: subject:CURL:handle :}
   subject path pathlen URL-FOR CURL:URL! DROP-CURL-STATUS
   subject method methodlen CURL:METHOD! DROP-CURL-STATUS
   subject TAKE-RESPONSE {: status:n :}
   subject CURL:CLEANUP
   status ;


: FETCH-IF-NONE-MATCH ( ptr u8 n ptr u8 n -- n )
   {: path:ptr pathlen:n etag:ptr etaglen:n :}
   OPEN-HANDLE {: subject:CURL:handle :}
   subject path pathlen URL-FOR CURL:URL! DROP-CURL-STATUS
   SB-RESET
   s" If-None-Match: " SB-APPEND
   etag etaglen SB-APPEND
   subject SB$ CURL:HEADER+ DROP-CURL-STATUS
   subject TAKE-RESPONSE {: status:n :}
   subject CURL:CLEANUP
   status ;


\ A JSON body posted with the header a browser sends.
: POST-JSON ( ptr u8 n ptr u8 n -- n ) {: body:ptr bodylen:n path:ptr pathlen:n :}
   OPEN-HANDLE {: subject:CURL:handle :}
   subject path pathlen URL-FOR CURL:URL! DROP-CURL-STATUS
   subject s" Content-Type: application/json" CURL:HEADER+ DROP-CURL-STATUS
   subject body bodylen CURL:BODY! DROP-CURL-STATUS
   subject TAKE-RESPONSE {: status:n :}
   subject CURL:CLEANUP
   status ;


\ ---- the raw client ----------------------------------------------------------

: DROP-TCP-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;


: RAW-OPEN ( -- TCP4:connection )
   LOOPBACK TCP4:ADDRESS HTTP:PORT TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF ENDOF
      failed OF drop -1 TCP4:>CONNECTION ENDOF
   ;MATCH ;


\ A refused request may be answered and the connection closed before the whole
\ request has been sent, so a short write is part of the case, not a failure.
: RAW-SEND ( TCP4:connection ptr u8 n -- )
   {: conn:TCP4:connection bytes:ptr len:n :}
   conn bytes len TCP4:TRANSFER-BYTES TCP4:WRITE DROP-TCP-STATUS ;


: RAW-READ-STEP ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   conn RES-BUF RES-U @ + RES-CAP RES-U @ - TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N RES-U @ + RES-U ! true ENDOF
      closed OF drop false ENDOF
      failed OF drop false ENDOF
   ;MATCH ;


\ Reads until the server closes the connection, which the idle deadline
\ guarantees even when the server would have kept it alive.
: RAW-DRAIN ( TCP4:connection -- ) {: conn:TCP4:connection :}
   0 RES-U !
   begin
      RES-U @ RES-CAP >= if exit then
      conn RAW-READ-STEP 0= if exit then
   again ;


: RAW-EXCHANGE ( ptr u8 n -- ) {: bytes:ptr len:n :}
   RAW-OPEN {: conn:TCP4:connection :}
   conn bytes len RAW-SEND
   conn RAW-DRAIN
   conn TCP4:CLOSE DROP-TCP-STATUS ;


$7D0 constant COLLECT-MS          \ a loopback answer that slow is a hang
1000000 constant NS-PER-MS


: RAW-READY? ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   mono-ns COLLECT-MS NS-PER-MS * + {: deadline:n :}
   begin
      conn TCP4:READABLE?
      MATCH TCP4:ready-result
         ready OF true ENDOF
         idle OF false ENDOF
         failed OF drop true ENDOF
      ;MATCH if true exit then
      mono-ns deadline >= if false exit then
      TASK:PAUSE
   again ;


\ Reads until the answer carries the marker, on a connection the server keeps
\ alive, so a worker that never answers fails the case instead of hanging it.
: RAW-COLLECT ( TCP4:connection ptr u8 n -- bool )
   {: conn:TCP4:connection want:ptr len:n :}
   0 RES-U !
   begin
      RES$ want len CONTAINS? if true exit then
      RES-U @ RES-CAP >= if false exit then
      conn RAW-READY? 0= if false exit then
      conn RAW-READ-STEP 0= if RES$ want len CONTAINS? exit then
   again ;


\ One request on its own connection, read until the answer carries the marker,
\ so a server whose worker died fails the case instead of hanging it.
: RAW-ASK ( ptr u8 n ptr u8 n -- bool ) {: bytes:ptr len:n want:ptr wantlen:n :}
   RAW-OPEN {: conn:TCP4:connection :}
   conn bytes len RAW-SEND
   conn want wantlen RAW-COLLECT
   conn TCP4:CLOSE DROP-TCP-STATUS ;


: REQ-RESET ( -- )
   0 REQ-U ! ;


: REQ+ ( ptr u8 n -- ) {: bytes:ptr len:n :}
   bytes len REQ-BUF REQ-U @ SPAN:SKIP SPAN:COPY
   REQ-U @ len + REQ-U ! ;


: REQ-FILL ( n n -- ) {: byte:n count:n :}
   REQ-U @ count + REQ-CAP > if E-BOOM throw then
   count 0 ?do byte REQ-BUF REQ-U @ i + SPAN:U8! loop
   REQ-U @ count + REQ-U ! ;


\ One request on its own connection, answered and closed.
: RAW-ONE ( ptr u8 n -- )
   REQ-RESET REQ+ REQ$ RAW-EXCHANGE ;


\ ---- reading the raw response ------------------------------------------------

: BEGINS? ( ptr u8 n -- bool ) {: want:ptr len:n :}
   RES$ want len STARTS-WITH? ;


: HAS? ( ptr u8 n -- bool ) {: want:ptr len:n :}
   RES$ want len CONTAINS? ;


: RESPONSES ( -- n )
   0
   RES-U @ 0 ?do
      RES-BUF i + RES-U @ i - s" HTTP/1.1 " STARTS-WITH? if 1+ then
   loop ;


\ The ETag of the last raw response, without its header name or its line end.
: TAKE-ETAG ( -- )
   0 ETAG-U !
   RES$ s" ETag: " FIND-SUB
   MATCH option
      some OF {: at:idx :} at IDX>N 6 + ENDOF
      none OF -1 ENDOF
   ;MATCH {: start:n :}
   start 0 < if exit then
   RES-BUF start + RES-U @ start - s\" \r" FIND-SUB
   MATCH option
      some OF {: stop:idx :} stop IDX>N ENDOF
      none OF 0 ENDOF
   ;MATCH {: len:n :}
   RES-BUF start + len ETAG-BUF SPAN:COPY
   len ETAG-U ! ;


\ ---- the transcript -----------------------------------------------------------

: SCRIPT$ ( -- ptr u8 n )
   SCRIPT-BUF SCRIPT-U @ SPAN:TAKE SPAN:$ ;


: SCRIPT+ ( ptr u8 n -- ) {: bytes:ptr len:n :}
   bytes len SCRIPT-BUF SCRIPT-U @ SPAN:SKIP SPAN:COPY
   SCRIPT-U @ len + SCRIPT-U ! ;


: SCRIPT+C ( n -- ) {: byte:n :}
   byte SCRIPT-BUF SCRIPT-U @ SPAN:U8!
   SCRIPT-U @ 1+ SCRIPT-U ! ;


\ The bytes of one head, each line ended by LF alone.
: HEAD+ ( n n -- ) {: from:n to:n :}
   to from ?do
      RES-BUF i + c@ {: byte:n :}
      byte CR <> if byte SCRIPT+C then
   loop ;


: BLANK-LINE$ ( -- ptr u8 n )
   S\" \r\n\r\n" ;


: HEAD-START ( n -- n ) {: at:n :}
   RES-BUF at + RES-U @ at - s" HTTP/1.1 " FIND-SUB
   MATCH option
      some OF {: off:idx :} off IDX>N at + ENDOF
      none OF -1 ENDOF
   ;MATCH ;


: HEAD-END ( n -- n ) {: start:n :}
   RES-BUF start + RES-U @ start - BLANK-LINE$ FIND-SUB
   MATCH option
      some OF {: off:idx :} off IDX>N start + ENDOF
      none OF RES-U @ ENDOF
   ;MATCH ;


\ The case's name, then the status line and headers of every response the last
\ raw exchange read, each followed by a blank line.
: TRANSCRIBE ( ptr u8 n -- ) {: name:ptr len:n :}
   s" == " SCRIPT+ name len SCRIPT+ LF SCRIPT+C
   0
   begin
      {: at:n :}
      at HEAD-START {: start:n :}
      start 0 < if exit then
      start HEAD-END {: stop:n :}
      start stop HEAD+
      LF SCRIPT+C LF SCRIPT+C
      stop
   again ;


: WANT$ ( -- ptr u8 n )
   WANT-BUF WANT-U @ SPAN:TAKE SPAN:$ ;


: W ( ptr u8 n -- ) {: bytes:ptr len:n :}
   bytes len WANT-BUF WANT-U @ SPAN:SKIP SPAN:COPY
   WANT-U @ len + WANT-U !
   LF WANT-BUF WANT-U @ SPAN:U8!
   WANT-U @ 1+ WANT-U ! ;


\ The transcript every run of this file writes: one worker answers it, so the
\ request ids in the error bodies, and the lengths they give, never vary.
: WANT-TRANSCRIPT ( -- )
   0 WANT-U !
   s" == json-route" W
   s" HTTP/1.1 200 OK" W
   s" Content-Type: application/json" W
   s" Connection: close" W
   s" Content-Length: 23" W
   s" " W
   s" == not-found" W
   s" HTTP/1.1 404 Not Found" W
   s" Content-Type: text/plain; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 53" W
   s" " W
   s" == not-allowed" W
   s" HTTP/1.1 405 Method Not Allowed" W
   s" Allow: GET" W
   s" Content-Type: text/plain; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 74" W
   s" " W
   s" == handler-fault" W
   s" HTTP/1.1 500 Internal Server Error" W
   s" Content-Type: text/plain; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 66" W
   s" " W
   s" == static-file" W
   s" HTTP/1.1 200 OK" W
   S\" ETag: \q959b733170daf59207c197e3a71d6fb20063009d923b1206cdf7bcb70075f561\q" W
   s" Cache-Control: no-cache" W
   s" Content-Type: text/html; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 40" W
   s" " W
   s" == static-not-modified" W
   s" HTTP/1.1 304 Not Modified" W
   S\" ETag: \q959b733170daf59207c197e3a71d6fb20063009d923b1206cdf7bcb70075f561\q" W
   s" Cache-Control: no-cache" W
   s" Connection: close" W
   s" " W
   s" == static-asset" W
   s" HTTP/1.1 200 OK" W
   S\" ETag: \qc813d0130320a76737eae33cecb7b0ed2053504fa4139c74e8f9536439fa57a3\q" W
   s" Cache-Control: no-cache" W
   s" Content-Type: text/javascript; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 24" W
   s" " W
   s" == bad-request" W
   s" HTTP/1.1 400 Bad Request" W
   s" Content-Type: text/plain; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 76" W
   s" " W
   s" == headers-too-large" W
   s" HTTP/1.1 431 Request Header Fields Too Large" W
   s" Content-Type: text/plain; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 95" W
   s" " W
   s" == body-too-large" W
   s" HTTP/1.1 413 Content Too Large" W
   s" Content-Type: text/plain; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 84" W
   s" " W
   s" == not-implemented" W
   s" HTTP/1.1 501 Not Implemented" W
   s" Content-Type: text/plain; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 85" W
   s" " W
   s" == chunked" W
   s" HTTP/1.1 200 OK" W
   s" Content-Type: application/json" W
   s" Connection: close" W
   s" Content-Length: 12" W
   s" " W
   s" == keep-alive" W
   s" HTTP/1.1 200 OK" W
   s" Content-Type: application/json" W
   s" Connection: keep-alive" W
   s" Content-Length: 23" W
   s" " W
   s" HTTP/1.1 200 OK" W
   s" Content-Type: application/json" W
   s" Connection: close" W
   s" Content-Length: 32" W
   s" " W
   s" == policy-asset" W
   s" HTTP/1.1 200 OK" W
   S\" ETag: \qc813d0130320a76737eae33cecb7b0ed2053504fa4139c74e8f9536439fa57a3\q" W
   s" Cache-Control: public, max-age=31536000, immutable" W
   s" Content-Type: text/javascript; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 24" W
   s" " W
   s" == policy-client-route" W
   s" HTTP/1.1 200 OK" W
   S\" ETag: \q959b733170daf59207c197e3a71d6fb20063009d923b1206cdf7bcb70075f561\q" W
   s" Cache-Control: no-cache" W
   s" Content-Type: text/html; charset=utf-8" W
   s" Connection: close" W
   s" Content-Length: 40" W
   s" " W
   s" == policy-not-found" W
   s" HTTP/1.1 404 Not Found" W
   s" Content-Type: application/json" W
   s" Connection: close" W
   s" Content-Length: 79" W
   s" " W
   s" == policy-bad-request" W
   s" HTTP/1.1 400 Bad Request" W
   s" Content-Type: application/json" W
   s" Connection: close" W
   s" Content-Length: 101" W
   s" " W
   s" == policy-handler-fault" W
   s" HTTP/1.1 500 Internal Server Error" W
   s" Content-Type: application/json" W
   s" Connection: close" W
   s" Content-Length: 91" W
   s" " W ;


: WRITE-TRANSCRIPT ( -- )
   s" build" MAKE-DIRS
   s" build/http-transcript.txt" SCRIPT$ WRITE-ALL ;


\ The artifact as another reader will find it: read back from its file and
\ compared whole with the transcript this file expects.
: TRANSCRIPT-CASE ( -- )
   WRITE-TRANSCRIPT
   s" build/http-transcript.txt" BACK-BUF SPAN:$ READ-ALL {: got:n :}
   WANT-TRANSCRIPT
   BACK-BUF got SPAN:TAKE SPAN:$ WANT$ T$= ;


\ ---- the operator's stderr ------------------------------------------------------

\ A server reports each fault on fd 2 under the id its client is given. For one
\ server fd 2 is a file, read back after the stop; the process's own stderr
\ waits on a saved descriptor and comes back however that server's case ends.
2 constant STDERR-FD
0 constant F-DUPFD
$A constant SAVE-FD-MIN           \ the saved stderr lands clear of 0..2
$800 constant ERR-CAP

variable ERR-PATH-U
variable ERR-U
variable SAVED-STDERR
create ERR-PATH-BUF FS-PATH-CAP allot
ERR-CAP SPAN-BUFFER: ERR-BUF


: ERR-PATH$ ( -- ptr u8 n )
   ERR-PATH-BUF ERR-PATH-U @ ;


: ERR$ ( -- ptr u8 n )
   ERR-BUF ERR-U @ SPAN:TAKE SPAN:$ ;


: SAVE-STDERR ( -- )
   s" http-stderr" TMPDIR-MKDIR {: dir:ptr dirlen:n :}
   dir dirlen CLEANUP-TREE+
   dir dirlen s" stderr.txt" ERR-PATH-BUF JOIN-PATH ERR-PATH-U !
   STDERR-FD F-DUPFD SAVE-FD-MIN fcntl {: saved:n :}
   saved 0 < if E-BOOM throw then
   saved SAVED-STDERR ! ;


: STDERR-TO-FILE ( -- )
   ERR-PATH$ OPEN-APPEND-FD {: file:n :}
   file STDERR-FD dup2 {: moved:n :}
   file close
   moved 0 < if E-BOOM throw then ;


: RESTORE-STDERR ( -- )
   SAVED-STDERR @ STDERR-FD dup2 {: moved:n :}
   SAVED-STDERR @ close
   moved 0 < if E-BOOM throw then ;


: READ-STDERR ( -- )
   ERR-PATH$ ERR-BUF SPAN:$ READ-ALL ERR-U ! ;


\ ---- the cases ---------------------------------------------------------------

: JSON-CASES ( -- )
   s" GET" s" /api/ping" FETCH 200 T=
   RES$ S\" {\qservice\q:\qhttp-test\q}" T$=
   S\" {\qtext\q:\qa\\\qb\q}" s" /api/answer" POST-JSON 200 T=
   RES$ S\" {\qtext\q:\qa\\\qb\q}" T$=
   S\" {\qother\q:1}" s" /api/answer" POST-JSON 400 T=
   s" bad_json: " HAS? TTRUE
   s" not json at all" s" /api/answer" POST-JSON 500 T=
   s" internal: " HAS? TTRUE
   s" GET" s" /api/items/a%20b?tier=top" FETCH 200 T=
   S\" \qid\q:\qa b\q" HAS? TTRUE
   S\" \qquery\q:\qtier=top\q" HAS? TTRUE
   S\" \qhost\q:\q127.0.0.1:" HAS? TTRUE
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" json-route" TRANSCRIBE ;


\ The default error answer is one line of text naming the code, the message and
\ the request id the fault is reported under.
: REFUSAL-CASES ( -- )
   s" GET" s" /api/nothing" FETCH 404 T=
   s" not_found: no route answers this path (request r0-" BEGINS? TTRUE
   s" DELETE" s" /api/items/7" FETCH 405 T=
   s" method_not_allowed: " BEGINS? TTRUE
   s\" GET /api/nothing HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" not-found" TRANSCRIBE
   s\" DELETE /api/items/7 HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" Allow: GET" HAS? TTRUE
   s" not-allowed" TRANSCRIBE ;


\ A handler that throws is answered 500 and its worker - the only one this
\ server has - answers the next request whole.
: FAULT-CASE ( -- )
   0 BOOM-HITS !
   s" GET" s" /api/boom" FETCH 500 T=
   s" internal: the handler did not finish this request (request r0-" BEGINS? TTRUE
   BOOM-HITS @ 1 T=
   s" GET" s" /api/ping" FETCH 200 T=
   0 BOOM-HITS !
   s" GET" s" /api/boom-json" FETCH 500 T=
   s" internal: " BEGINS? TTRUE
   BOOM-HITS @ 1 T=
   s" GET" s" /api/ping" FETCH 200 T=
   RES$ S\" {\qservice\q:\qhttp-test\q}" T$=
   s\" GET /api/boom HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" handler-fault" TRANSCRIBE ;


\ A body the writer cannot fit is refused by name rather than truncated: the
\ handler reads this package's own E-RESPONSE back, answers its own error into
\ the writer the refusal closed, and the worker serves the next request whole.
\ A handler that lets the refusal through is answered response_too_large by
\ the worker's boundary, not internal, and the worker serves on.
: OVERFLOW-CASE ( -- )
   FILL-OVER
   -1 OVERFLOW-CODE !
   s" GET" s" /api/overflow" FETCH 500 T=
   s" too_large: " BEGINS? TTRUE
   OVERFLOW-CODE @ HTTP:E-RESPONSE T=
   s" GET" s" /api/ping" FETCH 200 T=
   RES$ S\" {\qservice\q:\qhttp-test\q}" T$=
   s" GET" s" /api/overflow-raw" FETCH 500 T=
   s" response_too_large: " BEGINS? TTRUE
   s" GET" s" /api/ping" FETCH 200 T=
   RES$ S\" {\qservice\q:\qhttp-test\q}" T$= ;


\ The ETag a file is served with revalidates it: CURL sends it back and is
\ answered 304 with no body. With no rules installed every file is revalidated
\ and a path that names no file is the router's 404.
: STATIC-CASES ( -- )
   s\" GET /index.html HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" HTTP/1.1 200 OK" BEGINS? TTRUE
   s" Content-Type: text/html; charset=utf-8" HAS? TTRUE
   s" Cache-Control: no-cache" HAS? TTRUE
   TAKE-ETAG
   ETAG-U @ $42 T=
   s" static-file" TRANSCRIBE
   s" /index.html" ETAG$ FETCH-IF-NONE-MATCH 304 T=
   RES-U @ 0 T=
   REQ-RESET
   s\" GET /index.html HTTP/1.1\r\nHost: x\r\nIf-None-Match: " REQ+
   ETAG$ REQ+
   s\" \r\nConnection: close\r\n\r\n" REQ+
   REQ$ RAW-EXCHANGE
   s" static-not-modified" TRANSCRIBE
   s" GET" s" /_app/immutable/app.js" FETCH 200 T=
   RES$ ASSET-TEXT$ T$=
   s\" GET /_app/immutable/app.js HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" static-asset" TRANSCRIBE
   s" GET" s" /items/42" FETCH 404 T=
   s\" GET /_app/../index.html HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" HTTP/1.1 400" BEGINS? TTRUE
   s" bad_path: " HAS? TTRUE ;


: MALFORMED-CASE ( -- )
   s\" GET\r\nHost: x\r\n\r\n" RAW-ONE
   s" HTTP/1.1 400 Bad Request" BEGINS? TTRUE
   s" bad_request: " HAS? TTRUE
   s" bad-request" TRANSCRIBE ;


\ A ping on HTTP/1.1 whose head carries this one header line, which labels the
\ assertion that follows.
: PING-LINE ( ptr u8 n -- )
   {: line:ptr lu:n :}
   REQ-RESET
   s\" GET /api/ping HTTP/1.1\r\n" REQ+
   line lu REQ+
   s\" \r\nConnection: close\r\n\r\n" REQ+
   REQ$ RAW-EXCHANGE
   line lu T-LABEL ;


: HOST-REFUSED ( -- )
   s" HTTP/1.1 400 Bad Request" BEGINS? TTRUE
   s" bad_request: " HAS? TTRUE ;


: HOST-SERVED ( -- )
   s" HTTP/1.1 200 OK" BEGINS? TTRUE ;


\ RFC 9112 3.2: an HTTP/1.1 request without exactly one valid Host is refused
\ before any handler runs, while an HTTP/1.0 one may carry none. A valid Host
\ is uri-host [":" port] (RFC 3986 3.2.2, 3.2.3).
: HOST-CASE ( -- )
   s\" GET /api/ping HTTP/1.1\r\nConnection: close\r\n\r\n" RAW-ONE
   s" no Host" T-LABEL HOST-REFUSED
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" two Hosts" T-LABEL HOST-REFUSED
   s" Host:" PING-LINE HOST-REFUSED
   s" Host: a b" PING-LINE HOST-REFUSED
   s" Host: x:80a" PING-LINE HOST-REFUSED
   s" Host: x%2" PING-LINE HOST-REFUSED
   s" Host: [::1" PING-LINE HOST-REFUSED
   s" Host: [1::2::3]" PING-LINE HOST-REFUSED
   s" Host: [1:2:3:4:5:6:7:8:9]" PING-LINE HOST-REFUSED
   s" Host: [::1.2.3.256]" PING-LINE HOST-REFUSED
   s" Host: [::1.2.3.04]" PING-LINE HOST-REFUSED
   s" Host: [v.a]" PING-LINE HOST-REFUSED
   s\" GET /api/ping HTTP/1.0\r\n\r\n" RAW-ONE
   s" HTTP/1.0 without Host" T-LABEL HOST-SERVED
   s" Host: 127.0.0.1:8080" PING-LINE HOST-SERVED
   s" Host: example.com:8080" PING-LINE HOST-SERVED
   s" Host: %41-._~!$&'()*+,;=:" PING-LINE HOST-SERVED
   s" Host: [::1]:80" PING-LINE HOST-SERVED
   s" Host: [1:2:3:4:5:6:7:8]" PING-LINE HOST-SERVED
   s" Host: [::ffff:1.2.3.255]" PING-LINE HOST-SERVED
   s" Host: [v1.a:b]" PING-LINE HOST-SERVED ;


: BIG-HEADER-CASE ( -- )
   REQ-RESET
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\nX-Big: " REQ+
   FILL-BYTE BIG-HEADER-BYTES REQ-FILL
   s\" \r\n\r\n" REQ+
   REQ$ RAW-EXCHANGE
   s" HTTP/1.1 431" BEGINS? TTRUE
   s" headers_too_large: " HAS? TTRUE
   s" headers-too-large" TRANSCRIBE ;


: BIG-BODY-CASE ( -- )
   s\" POST /api/echo HTTP/1.1\r\nHost: x\r\nContent-Length: 4000000\r\n\r\n" RAW-ONE
   s" HTTP/1.1 413" BEGINS? TTRUE
   s" body_too_large: " HAS? TTRUE
   s" body-too-large" TRANSCRIBE ;


\ One byte over the body limit is still the request reader's own refusal and
\ not a span throw escaping the worker: 413, and the worker takes the next
\ request on a new connection.
: OVER-BODY-CASE ( -- )
   s\" POST /api/echo HTTP/1.1\r\nHost: x\r\nContent-Length: 1048577\r\n\r\n" RAW-ONE
   s" HTTP/1.1 413" BEGINS? TTRUE
   s" body_too_large: " HAS? TTRUE
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" HTTP/1.1 200 OK" BEGINS? TTRUE ;


\ A transfer coding other than chunked is one this server does not implement.
: CODING-CASE ( -- )
   s\" POST /api/echo HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: gzip\r\nConnection: close\r\n\r\n" RAW-ONE
   s" HTTP/1.1 501 Not Implemented" BEGINS? TTRUE
   s" not_implemented: " HAS? TTRUE
   s" not-implemented" TRANSCRIBE ;


: CHUNKED-CASE ( -- )
   REQ-RESET
   s\" POST /api/echo HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n" REQ+
   s\" 5\r\nhello\r\n6\r\n world\r\n0\r\n\r\n" REQ+
   REQ$ RAW-EXCHANGE
   s" HTTP/1.1 200 OK" BEGINS? TTRUE
   S\" {\qbytes\q:11}" HAS? TTRUE
   s" chunked" TRANSCRIBE ;


: KEEP-ALIVE-CASE ( -- )
   REQ-RESET
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\n\r\n" REQ+
   s\" GET /api/items/7 HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" REQ+
   REQ$ RAW-EXCHANGE
   RESPONSES 2 T=
   s" Connection: keep-alive" HAS? TTRUE
   S\" {\qservice\q:\qhttp-test\q}" HAS? TTRUE
   S\" {\qid\q:\q7\q,\qquery\q:\q\q,\qhost\q:\qx\q}" HAS? TTRUE
   s" keep-alive" TRANSCRIBE ;


\ True when the server ends the stream before the collect deadline: the read
\ answers the end of the stream, not data and not a reset.
: CLOSED-WITHIN? ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   conn RAW-READY? 0= if false exit then
   conn RES-BUF RES-CAP TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF drop false ENDOF
      closed OF drop true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;


\ A connection the peer keeps open and says nothing more on is closed by the
\ server's own idle deadline, without the peer asking for it.
: IDLE-CASE ( -- )
   RAW-OPEN {: quiet:TCP4:connection :}
   REQ-RESET
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\n\r\n" REQ+
   quiet REQ$ RAW-SEND
   quiet S\" {\qservice\q:\qhttp-test\q}" RAW-COLLECT TTRUE
   quiet CLOSED-WITHIN? TTRUE
   quiet TCP4:CLOSE DROP-TCP-STATUS ;


\ A HEAD answer carries the head a GET would carry, and no body after it.
: HEAD-CASE ( -- )
   s\" HEAD /index.html HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" HTTP/1.1 200 OK" BEGINS? TTRUE
   s" Content-Type: text/html; charset=utf-8" HAS? TTRUE
   S\" Content-Length: 40\r\n" HAS? TTRUE
   s" <!doctype html>" HAS? TFALSE
   RES$ S\" \r\n\r\n" ENDS-WITH? TTRUE ;


\ Connection: close ends the connection after its own answer, so the request
\ that follows it on the same connection is never read.
: CLOSE-CASE ( -- )
   REQ-RESET
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" REQ+
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\n\r\n" REQ+
   REQ$ RAW-EXCHANGE
   RESPONSES 1 T=
   s" Connection: close" HAS? TTRUE ;


\ A body that throws with its writer open leaves nothing behind: the same
\ worker, on the same connection, answers the next request with its whole body
\ and nothing of the half-written object.
: WRITER-RECOVERY-CASE ( -- )
   REQ-RESET
   s\" GET /api/boom-json HTTP/1.1\r\nHost: x\r\n\r\n" REQ+
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" REQ+
   REQ$ RAW-EXCHANGE
   RESPONSES 2 T=
   s" internal: " HAS? TTRUE
   S\" \r\n\r\n{\qservice\q:\qhttp-test\q}" HAS? TTRUE
   s" partial" HAS? TFALSE ;


\ Read from the answers while the pool is live: both start hooks ran, in the
\ order they were registered, before this worker's first request, however many
\ requests it has answered since. Installing a hook or a rule on a running
\ server is refused by name, because every worker already started would miss it.
: HOOK-LIVE-CASES ( -- )
   s" GET" s" /api/trace" FETCH 200 T=
   S\" {\qtrace\q:\qabr" HAS? TTRUE
   s" GET" s" /api/trace" FETCH 200 T=
   S\" {\qtrace\q:\qabr" HAS? TTRUE
   s" GET" s" /api/trace" FETCH 200 T=
   S\" {\qtrace\q:\qabr" HAS? TTRUE
   [: ADD-START-HOOK ;] HTTP:E-STATE TTHROWSQ
   [: ADD-ERROR-HOOK ;] HTTP:E-STATE TTHROWSQ ;


\ Read after STOP, when every worker has ended: each worker ran both start hooks
\ once in order and both exit hooks once in reverse, with nothing between them
\ but the requests it answered, and the three traced requests are all there.
: HOOK-TRACE-CASES ( -- )
   0
   ONE-WORKER 0 ?do
      i TRACE$ s" ab" STARTS-WITH? TTRUE
      i TRACE$ s" yx" ENDS-WITH? TTRUE
      i MARK-START-1 MARK-COUNT 1 T=
      i MARK-START-2 MARK-COUNT 1 T=
      i MARK-EXIT-1 MARK-COUNT 1 T=
      i MARK-EXIT-2 MARK-COUNT 1 T=
      i MARK-REQUEST MARK-COUNT +
   loop
   3 T= ;


\ The caller's policy, installed between servers: errors rendered as JSON - the
\ router's 404, a refusal and a handler fault alike, the last two rendered
\ outside the handler's boundary - the hashed asset directory cached for a
\ year, and a client route answered with the one page, while /api/ keeps the
\ router's own 404.
: POLICY-CASES ( -- )
   INSTALL-POLICY
   ROOT$ HTTP:STATIC-ROOT
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   s" GET" s" /api/nothing" FETCH 404 T=
   RES$ S\" {\qcode\q:\qnot_found\q,\qmessage\q:\qno route answers this path\q,\qrequest_id\q:\qr0-1\q}" T$=
   s" GET" s" /items/42" FETCH 200 T=
   RES$ INDEX-TEXT$ T$=
   s\" GET /_app/immutable/app.js HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" policy-asset" TRANSCRIBE
   s\" GET /items/42 HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" policy-client-route" TRANSCRIBE
   s\" GET /api/nothing HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   s" policy-not-found" TRANSCRIBE
   s\" GET\r\nHost: x\r\n\r\n" RAW-ONE
   S\" {\qcode\q:\qbad_request\q," HAS? TTRUE
   s" policy-bad-request" TRANSCRIBE
   s\" GET /api/boom HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" RAW-ONE
   S\" {\qcode\q:\qinternal\q," HAS? TTRUE
   s" policy-handler-fault" TRANSCRIBE
   HTTP:STOP
   HTTP:KILLED-TASKS 0 T= ;


$BB8 constant STOP-CEILING-MS     \ over the server's own stop bound, far under a hang


\ True once the parked handler has been reached, which is when its worker can no
\ longer end itself.
: PARK-REACHED? ( -- bool )
   mono-ns COLLECT-MS NS-PER-MS * + {: deadline:n :}
   begin
      PARKED @ 0 <> if true exit then
      mono-ns deadline >= if false exit then
      TASK:PAUSE
   again ;


\ A worker parked inside a handler is the one task a cooperative stop cannot
\ wait out: the stop asks, waits its own bound - under a second for this
\ IDLE-MS, well under the ceiling here - and then kills it, which is what
\ KILLED-TASKS counts. The listener and the other worker end themselves, so
\ ENDED-TASKS is one short of the total. The killed worker's exit closes the
\ connection it was serving, so its peer reads the end of the stream within the
\ collect deadline instead of waiting on it. The server started next serves a
\ request - answering the HTTP/1.0 its request line named, not the HTTP/1.1 of
\ its own status line - and ends every task itself; the trace rows its
\ inherited hooks mark are emptied first. Its own servers, started after the
\ first ones have been stopped and before the controlled start hook is added.
: PARKED-WORKER-CASE ( -- )
   0 PARKED !
   ROOT$ HTTP:STATIC-ROOT
   LOOPBACK 0 TWO-WORKERS IDLE-MS HTTP:START
   RAW-OPEN {: held:TCP4:connection :}
   REQ-RESET
   s\" GET /api/park HTTP/1.1\r\nHost: x\r\n\r\n" REQ+
   held REQ$ RAW-SEND
   PARK-REACHED? TTRUE
   mono-ns {: began:n :}
   HTTP:STOP
   mono-ns began - STOP-CEILING-MS NS-PER-MS * < TTRUE
   HTTP:RUNNING? TFALSE
   HTTP:KILLED-TASKS 1 T=
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL 1- T=
   held CLOSED-WITHIN? TTRUE
   held TCP4:CLOSE DROP-TCP-STATUS
   TRACE-RESET
   LOOPBACK 0 TWO-WORKERS IDLE-MS HTTP:START
   s\" GET /api/version HTTP/1.0\r\nHost: x\r\n\r\n"
   S\" {\qversion\q:\qHTTP/1.0\q}" RAW-ASK TTRUE
   s" HTTP/1.1 200 OK" BEGINS? TTRUE
   HTTP:STOP
   HTTP:KILLED-TASKS 0 T=
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T= ;


HTTP-TEST:QUEUED TYPED-BUFFER QUEUED-CONN TCP4:connection
variable OPENED                   \ the peers whose connect succeeded, first in QUEUED-CONN


\ True once the listener has taken n connections since its server started,
\ within the collect deadline.
: TAKEN-WITHIN? ( n -- bool ) {: want:n :}
   mono-ns COLLECT-MS NS-PER-MS * + {: deadline:n :}
   begin
      HTTP-TEST:TAKEN want >= if true exit then
      mono-ns deadline >= if false exit then
      TASK:PAUSE
   again ;


\ Opens a peer that says nothing, kept only when its connect succeeded.
: PEER-OPEN ( -- )
   LOOPBACK TCP4:ADDRESS HTTP:PORT TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF OPENED @ QUEUED-CONN ! 1 OPENED +! ENDOF
      failed OF drop ENDOF
   ;MATCH ;


\ Opens QUEUED peers, each once the listener has taken the one before, and
\ answers whether it took the last. Opened back to back they would outrun the
\ listener and overflow the listen backlog, and the kernel may reset the peers
\ past it instead of queueing them - macOS does, during the connect or just
\ after it.
: QUEUE-PEERS ( -- bool )
   0 OPENED !
   HTTP-TEST:TAKEN {: before:n :}
   HTTP-TEST:QUEUED 0 ?do
      PEER-OPEN
      before OPENED @ + TAKEN-WITHIN? 0= if false unloop exit then
   loop
   true ;


\ How many of them read the end of their stream.
: CLOSED-PEERS ( -- n )
   0
   OPENED @ 0 ?do i QUEUED-CONN @ CLOSED-WITHIN? if 1+ then loop ;


: DROP-PEERS ( -- )
   OPENED @ 0 ?do i QUEUED-CONN @ TCP4:CLOSE DROP-TCP-STATUS loop ;


\ With its one worker parked inside a handler, the server queues silent peers
\ until its job queue is full and its listener holds one more. The stop still
\ ends within its own bound instead of waiting on that queue, kills the parked
\ worker, and closes every connection no worker took, so each of those peers
\ reads the end of its stream. The trace rows the inherited hooks mark are
\ emptied first.
: FULL-QUEUE-CASE ( -- )
   TRACE-RESET
   0 PARKED !
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   RAW-OPEN {: held:TCP4:connection :}
   REQ-RESET
   s\" GET /api/park HTTP/1.1\r\nHost: x\r\n\r\n" REQ+
   held REQ$ RAW-SEND
   PARK-REACHED? TTRUE
   s" the listener takes every peer, the last one past a full queue" T-LABEL
   QUEUE-PEERS TTRUE
   OPENED @ HTTP-TEST:QUEUED T=
   mono-ns {: began:n :}
   HTTP:STOP
   s" a stop with the job queue full ends within its bound" T-LABEL
   mono-ns began - STOP-CEILING-MS NS-PER-MS * < TTRUE
   HTTP:KILLED-TASKS 1 T=
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL 1- T=
   s" and every queued peer reads the end of its stream" T-LABEL
   CLOSED-PEERS HTTP-TEST:QUEUED T=
   held CLOSED-WITHIN? TTRUE
   held TCP4:CLOSE DROP-TCP-STATUS
   DROP-PEERS ;


$1000 constant CHUNK-BYTES        \ what the peer that keeps sending writes at a time
$1F4 constant SEND-ON-MS          \ how long it sends before the stop: past the linger deadline

create CHUNK-BUF CHUNK-BYTES allot
1 TYPED-BUFFER SENDER-CONN TCP4:connection
TASK:MIN-STACK TASK:TASK SENDER-TASK


\ Writes until a write fails: the reset a close sends a peer whose bytes are
\ still unread.
: KEEP-SENDING ( -- )
   0 SENDER-CONN @ {: conn:TCP4:connection :}
   begin
      conn CHUNK-BUF CHUNK-BYTES TCP4:TRANSFER-BYTES TCP4:WRITE
      MATCH TCP4:status
         ok OF ENDOF
         failed OF drop exit ENDOF
      ;MATCH
   again ;


\ True once the task has ended, false at the collect deadline.
: TASK-ENDED? ( ptr n -- bool ) {: tcb:ptr :}
   mono-ns COLLECT-MS NS-PER-MS * + {: deadline:n :}
   begin
      tcb TASK:DONE? if true exit then
      mono-ns deadline >= if false exit then
      TASK:PAUSE
   again ;


\ A peer that reads its answer to the end of the stream and then keeps sending
\ is read until the linger deadline and no longer: the worker closes however
\ long the peer goes on, which resets it, and is back on its queue well before
\ a stop that then kills nothing. The peer's writes fail on the reset.
: SENDING-PEER-CASE ( -- )
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   RAW-OPEN {: conn:TCP4:connection :}
   conn 0 SENDER-CONN !
   REQ-RESET
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" REQ+
   conn REQ$ RAW-SEND
   conn RAW-DRAIN
   s" HTTP/1.1 200 OK" BEGINS? TTRUE
   [: KEEP-SENDING ;] SENDER-TASK TASK:ACTIVATE
   SEND-ON-MS >MS TASK:SLEEP
   HTTP:STOP
   s" the lingering worker ended itself" T-LABEL
   HTTP:KILLED-TASKS 0 T=
   SENDER-TASK TASK-ENDED? TTRUE
   SENDER-TASK TASK:KILL
   conn TCP4:CLOSE DROP-TCP-STATUS ;


\ Every activated worker runs its exit hooks before the failed START throws.
: FAILED-TRACE ( -- )
   TWO-WORKERS 0 ?do
      i TRACE$ s" abt" STARTS-WITH? TTRUE
      i TRACE$ s" zyx" ENDS-WITH? TTRUE
      i MARK-REQUEST MARK-COUNT 0 T=
      i MARK-EXIT-1 MARK-COUNT 1 T=
      i MARK-EXIT-2 MARK-COUNT 1 T=
   loop ;


: START-TWO ( -- )
   LOOPBACK 0 TWO-WORKERS IDLE-MS HTTP:START ;


: START-SAME-PORT ( -- )
   LOOPBACK FAILED-PORT @ TWO-WORKERS IDLE-MS HTTP:START ;


\ Setup can fail after the queue opens. The failed start must release it and
\ its static tree, leaving the same port usable without a half-open pool.
: SETUP-REFUSAL-CASE ( -- )
   ROOT$ HTTP:STATIC-ROOT
   HTTP:TEST-PREOPEN
   [: START-TWO ;] E-TASK-SEM-STATE TTHROWSQ
   HTTP:RUNNING? TFALSE
   HTTP:STATIC-COUNT 0 T=
   HTTP:PORT FAILED-PORT !
   HTTP:TEST-UNINJECT
   ROOT$ HTTP:STATIC-ROOT
   START-SAME-PORT
   HTTP:RUNNING? TTRUE
   s" GET" s" /api/ping" FETCH 200 T=
   HTTP:STOP
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   HTTP:KILLED-TASKS 0 T= ;


\ One hook fails while its peer succeeds. The returned code and both complete
\ traces prove the caller observes the whole startup outcome before catch ends.
\ An exit hook that throws, run first as the last registered, keeps none of the
\ hooks registered before it from running in either worker.
: THROWING-HOOK-CASE ( -- )
   TRACE-RESET
   0 THROWN !
   1 HOOK-MODE !
   [: START-THROWS ;] HTTP:ON-WORKER-START
   [: EXIT-THROWS ;] HTTP:ON-WORKER-EXIT
   ROOT$ HTTP:STATIC-ROOT
   [: START-TWO ;] E-HOOK TTHROWSQ
   HTTP:RUNNING? TFALSE
   THROWN @ TWO-WORKERS T=
   HTTP:ENDED-TASKS 1 T=
   FAILED-TRACE
   HTTP:PORT FAILED-PORT !
   TRACE-RESET
   0 THROWN !
   2 HOOK-MODE !
   ROOT$ HTTP:STATIC-ROOT
   [: START-SAME-PORT ;] catch {: code:n :}
   code E-HOOK = code E-HOOK-OTHER = or TTRUE
   HTTP:RUNNING? TFALSE
   THROWN @ TWO-WORKERS T=
   FAILED-TRACE
   TRACE-RESET
   0 THROWN !
   0 HOOK-MODE !
   ROOT$ HTTP:STATIC-ROOT
   START-SAME-PORT
   HTTP:RUNNING? TTRUE
   s" GET" s" /api/trace" FETCH 200 T=
   RES$ S\" {\qtrace\q:\qabtr\q}" T$=
   HTTP:STOP
   HTTP:RUNNING? TFALSE
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   HTTP:KILLED-TASKS 0 T= ;


: RELEASE-HOOKS ( -- )
   HOOK-READY TASK:WAIT
   HOOK-READY TASK:WAIT
   50 >MS TASK:SLEEP
   START-DONE atomic@ START-OBSERVED !
   HOOK-GATE TASK:SIGNAL
   HOOK-GATE TASK:SIGNAL ;


: DROP-RELEASER-RESULT ( result<n,n> -- )
   MATCH result
      ok OF drop E-BOOM throw ENDOF
      err OF E-TASK-NO-RESULT <> if E-BOOM throw then ENDOF
   ;MATCH ;


\ The starter cannot return while either worker is held in its start hook.
: GATED-HOOK-CASE ( -- )
   TRACE-RESET
   0 THROWN !
   0 HOOK-FINISHED !
   3 HOOK-MODE !
   0 START-DONE atomic!
   1 START-OBSERVED !
   0 HOOK-READY TASK:SEMAPHORE-INIT
   0 HOOK-GATE TASK:SEMAPHORE-INIT
   ROOT$ HTTP:STATIC-ROOT
   [: RELEASE-HOOKS ;] HOOK-RELEASER TASK:ACTIVATE
   START-TWO
   1 START-DONE atomic!
   HOOK-RELEASER TASK:JOIN DROP-RELEASER-RESULT
   START-OBSERVED @ 0 T=
   HTTP:RUNNING? TTRUE
   THROWN @ TWO-WORKERS T=
   HOOK-FINISHED @ TWO-WORKERS T=
   s" GET" s" /api/trace" FETCH 200 T=
   HTTP:STOP
   HTTP:RUNNING? TFALSE
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   HTTP:KILLED-TASKS 0 T=
   HOOK-READY TASK:SEMAPHORE-DESTROY
   HOOK-GATE TASK:SEMAPHORE-DESTROY ;


: ERROR-HOOK-SERVER ( -- )
   STDERR-TO-FILE
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   s\" GET\r\nHost: x\r\n\r\n"
   s" error_hook: the error hook did not finish this answer (request r0-1)" RAW-ASK TTRUE
   s" HTTP/1.1 400 Bad Request" BEGINS? TTRUE
   s" Content-Type: text/plain; charset=utf-8" HAS? TTRUE
   s" X-Hook" HAS? TFALSE
   s\" GET /api/boom HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n"
   s" error_hook: the error hook did not finish this answer (request r0-2)" RAW-ASK TTRUE
   s" HTTP/1.1 500 Internal Server Error" BEGINS? TTRUE
   s\" GET /api/ping HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n"
   S\" {\qservice\q:\qhttp-test\q}" RAW-ASK TTRUE
   HTTP:STOP ;


\ An error hook that throws, on a refusal and on a handler fault alike, costs
\ its client the rendered answer and nothing more: each is answered with the
\ plain-text fallback and the status it was owed, the one worker serves the
\ next request, every task ends itself, and stderr names each failure under the
\ id its client was given. The trace rows the inherited hooks mark are emptied
\ first.
: ERROR-HOOK-CASE ( -- )
   TRACE-RESET
   [: RENDER-THROWS ;] HTTP:ON-ERROR
   SAVE-STDERR
   [: ERROR-HOOK-SERVER ;] [: RESTORE-STDERR ;] finally
   HTTP:RUNNING? TFALSE
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   HTTP:KILLED-TASKS 0 T=
   READ-STDERR
   ERR$ s" http: request r0-1 failed, throw -9992" CONTAINS? TTRUE
   ERR$ s" http: request r0-2 failed, throw -9990" CONTAINS? TTRUE
   ERR$ s" http: request r0-2 failed, throw -9992" CONTAINS? TTRUE ;


\ Before the loop: a start whose tasks have no loop to ride their readiness
\ waits on is refused by AIO's own code, with nothing of the server taken. The
\ same code thrown inside the listener's own task would be a report nobody
\ reads and a suite hanging on its first request.
: NO-LOOP-CASE ( -- )
   AIO:RUNNING? TFALSE
   [: LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START ;] E-AIO-STATE TTHROWSQ
   HTTP:RUNNING? TFALSE ;


\ ---- the AIO loop ------------------------------------------------------------

$3E8 constant DRAIN-MS            \ how long a stop waits for the ring to drain
10 constant RETRY-MS              \ between tries, so the loop task gets the core


\ A task killed inside a wait leaves its record in flight until the loop drains
\ the kernel's cancel completion, and AIO:STOP is E-AIO-BUSY until it does, so
\ the stop after the last join is retried to a bound rather than asserted on
\ the first try (lib/aio-test.f STOPPED?). Past the bound the suite ends on
\ AIO's own code rather than on a silence.
: AIO-STOP ( -- )
   mono-ns DRAIN-MS NS-PER-MS * + {: deadline:n :}
   begin
      [: AIO:STOP ;] catch {: code:n :}
      code 0= if exit then
      mono-ns deadline > if code throw then
      RETRY-MS >MS TASK:SLEEP
   again ;


\ The loop first: the listener's own wait and this suite's raw READABLE? ride
\ it, and it is stopped once every server has been stopped, which is when every
\ task this suite started has been joined.
: LIFECYCLE ( -- )
   NO-LOOP-CASE
   AIO:START
   MAKE-STATIC
   INSTALL-ROUTES
   INSTALL-HOOKS
   0 SCRIPT-U !
   ROOT$ HTTP:STATIC-ROOT
   HTTP:STATIC-COUNT 2 T=
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   HTTP:PORT 0 > TTRUE
   JSON-CASES
   REFUSAL-CASES
   FAULT-CASE
   OVERFLOW-CASE
   STATIC-CASES
   MALFORMED-CASE
   HOST-CASE
   BIG-HEADER-CASE
   BIG-BODY-CASE
   OVER-BODY-CASE
   CODING-CASE
   CHUNKED-CASE
   KEEP-ALIVE-CASE
   CLOSE-CASE
   HEAD-CASE
   IDLE-CASE
   WRITER-RECOVERY-CASE
   HOOK-LIVE-CASES
   HTTP:STOP
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   HTTP:KILLED-TASKS 0 T=
   HTTP:RUNNING? TFALSE
   HOOK-TRACE-CASES
   POLICY-CASES
   TRANSCRIPT-CASE
   PARKED-WORKER-CASE
   FULL-QUEUE-CASE
   SENDING-PEER-CASE
   ERROR-HOOK-CASE
   SETUP-REFUSAL-CASE
   THROWING-HOOK-CASE
   GATED-HOOK-CASE
   ROOT$ REMOVE-TREE
   AIO-STOP ;


T-RESET
LIFECYCLE
CLEANUP-RUN
T-REPORT
s" http-test: ok" type LF emit   \ CR here is the package constant 13, not `cr`

;package
