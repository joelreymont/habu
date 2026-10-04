\ curl-test.f - focused tests for lib/net/curl.f over loopback HTTP.
\
\ One process holds both peers: a server task binds a TCP4 listener on a free
\ loopback port, publishes the port in a shared cell and accepts one connection
\ at a time, while the main task drives package CURL against it. A second
\ listener is never accepted on: it is the peer that never answers.
\ Run: bin/hb --load lib/net/curl-test.f
\ HABU_NET_TESTS=1 adds one real HTTPS request to https://example.com.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-env.f
require lib/ffi-abi.f
require lib/le.f
require lib/image-lifecycle.f
require lib/task.f
require test/host-threads.f
require lib/aio.f                       \ the loop the multiplexed transfers wait on
require lib/net/tcp4.f
require lib/net/curl.f

package CURL-TEST

private

CAST: BLEN>N ( NUM:byte-len -- n )
CAST: STATE>CELL ( n -- ptr n )

VERSIONED-LIBRARY curl 4
FUNCTION: PRIVATE-GET curl_easy_getinfo ( n n ptr u8 -- i32 )
   2 VARIADIC
   2 $08 WRITES-BYTES
;FUNCTION

$100015 constant INFO-PRIVATE

$7F000001 constant LOOPBACK
$40 constant BACKLOG                    \ the concurrent case connects MANY-N times at once
$2000 constant BODY-CAP
$1000 constant REQ-CAP
$1000 constant RES-CAP
$1000 constant HEADER-CAP
$20 constant NUM-CAP
8 constant DRAIN-TRIES                  \ reads that finish a connection the peer is closing
$1000 constant READY-TRIES              \ TASK:PAUSE turns before the listener is late
100 constant STALL-MS                   \ well under the stall path's forever
$2710 constant REQUEST-MS               \ 10 s: a loopback request that slow is broken
$FE constant FILL-BYTE                  \ the sentinel a refused transfer must not overwrite
$300 constant DRIBBLE-U                 \ the dribble body, served whole but slowly
$40 constant DRIBBLE-STEP               \ bytes per timed write
100 constant DRIBBLE-GAP-MS             \ 12 steps: 1.3 s at about 640 bytes a second
\ The dribble must outlast LOW-SECONDS: a shorter one finishes before any stall
\ test or one-second ceiling could end it, so DRIBBLE-LOW-SPEED would pass
\ whatever LOW-SPEED! set.
DRIBBLE-GAP-MS 3 * constant CEILING-MS  \ several gaps in: a per-read limit never fires, a ceiling does
$61 constant DRIBBLE-BYTE               \ the body's filler, never the sentinel
1000000 constant NS-PER-MS
10 constant LOW-RATE                    \ bytes a second, far under the dribble's own
1 constant LOW-SECONDS                  \ the shortest window libcurl measures
5000 constant STALL-MOST-MS             \ half REQUEST-MS: the stall test ended it, not the ceiling
$80000000 constant OVER-LONG            \ one past the C long ceiling CURL checks

create ROOT-BUF FS-PATH-CAP allot
create FILE-BUF FS-PATH-CAP allot
create JAR-BUF FS-PATH-CAP allot
create BODY-BUF BODY-CAP allot
create REQ-BUF REQ-CAP allot
create RES-BUF RES-CAP allot
create HEADER-BUF HEADER-CAP allot
create HEADER-SAVED HEADER-CAP allot
create TRANSCRIPT-PATH FS-PATH-CAP allot
create TRANSCRIPT-BUF HEADER-CAP allot
create JAR-TEXT-BUF RES-CAP allot
create NUM-BUF NUM-CAP allot
create DRIBBLE-BUF DRIBBLE-U allot

: TEST-ALIGN8 ( -- )
   here FFI:>CELL 7 and 8 swap - 7 and allot ;

TEST-ALIGN8
variable ROOT-U
variable FILE-U
variable JAR-U
variable REQ-U                          \ bytes read from the current request
variable HEAD-U                         \ bytes of its head, past the blank line
variable RES-U
variable NUM-N
variable SEND-COOKIE
variable SERVER-LISTENER
variable SERVER-PORT                    \ the task's mailbox for the bound port
variable SERVER-READY
variable SERVER-STOP
variable SERVER-DONE
variable SERVER-BAD
variable SERVER-ERRNO
variable SERVER-HITS
variable SILENT-LISTENER
variable SILENT-PORT
variable CEILING-HITS                   \ the cut dribble's request, if it got out
variable LAST-STATUS
variable LAST-LEN
variable LAST-CODE
variable LAST-KIND                      \ 0 response, 1 truncated, 2 failed
variable HEADER-LEN
variable HEADER-KIND
variable TRANSCRIPT-U
TYPED-VARIABLE HEADER-HANDLE CURL:handle

0 constant KIND-RESPONSE
1 constant KIND-TRUNCATED
2 constant KIND-FAILED

TASK:MIN-STACK TASK:TASK SERVER-TASK


: ROOT$ ( -- ptr u8 n )       ROOT-BUF ROOT-U @ ;
: FILE$ ( -- ptr u8 n )       FILE-BUF FILE-U @ ;
: JAR$ ( -- ptr u8 n )        JAR-BUF JAR-U @ ;
: HELLO$ ( -- ptr u8 n )      S\" hello\n" ;
: COOKIE-PAIR$ ( -- ptr u8 n ) s" probe=value1" ;
: NO-COOKIE$ ( -- ptr u8 n )  s" none" ;
: PATH-HELLO$ ( -- ptr u8 n ) s" /hello.txt" ;
: PATH-COOKIE$ ( -- ptr u8 n ) s" /cookie" ;
: PATH-STALL$ ( -- ptr u8 n ) s" /stall" ;
: PATH-DRIBBLE$ ( -- ptr u8 n ) s" /dribble" ;
: PATH-HEADERS$ ( -- ptr u8 n ) s" /headers" ;
: PATH-HEADERS-B$ ( -- ptr u8 n ) s" /headers-b" ;
: PATH-REDIRECT$ ( -- ptr u8 n ) s" /redirect" ;
: PATH-INTERIM$ ( -- ptr u8 n ) s" /interim" ;
: JSON$ ( -- ptr u8 n ) s" {}" ;
: HEADERS$ ( -- ptr u8 n )
   S\" HTTP/1.0 200 OK\r\nCache-Control: max-age=60\r\nExpires: Wed, 21 Oct 2030 07:28:00 GMT\r\nAge: 7\r\nRetry-After: 9\r\nX-Duplicate: first\r\nX-Duplicate: second\r\nContent-Length: 2\r\nConnection: close\r\n\r\n" ;
: HEADERS-B$ ( -- ptr u8 n )
   S\" HTTP/1.0 200 OK\r\nX-Generation: two\r\nContent-Length: 2\r\nConnection: close\r\n\r\n" ;
: INTERIM-FINAL$ ( -- ptr u8 n )
   S\" HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nTrailer: Cache-Control\r\nConnection: close\r\n\r\n" ;
: DRIBBLE$ ( -- ptr u8 n ) DRIBBLE-BUF DRIBBLE-U ;


: DRIBBLE-FILL ( -- )
   DRIBBLE-U 0 do DRIBBLE-BYTE DRIBBLE-BUF i + c! loop ;


\ ---- the request the server task reads ---------------------------------------

\ Header names are matched without case because HTTP does not fix it; the path
\ and the method are matched exactly because HTTP does.
: REQ-AT? ( ptr u8 n n -- bool ) {: needle:ptr needleu:n at:n :}
   at needleu + HEAD-U @ > if false exit then
   REQ-BUF at + needleu needle needleu STR= ;


: REQ-AT-CI? ( ptr u8 n n -- bool ) {: needle:ptr needleu:n at:n :}
   at needleu + HEAD-U @ > if false exit then
   REQ-BUF at + needleu needle needleu STR=CI ;


: REQ-HAS? ( ptr u8 n -- bool ) {: needle:ptr needleu:n :}
   HEAD-U @ 0 ?do needle needleu i REQ-AT? if true unloop exit then loop
   false ;


: REQ-HAS-CI? ( ptr u8 n -- bool ) {: needle:ptr needleu:n :}
   HEAD-U @ 0 ?do needle needleu i REQ-AT-CI? if true unloop exit then loop
   false ;


: BLANK-LINE$ ( -- ptr u8 n )
   S\" \r\n\r\n" ;


\ Index just past the blank line that ends the head, or zero while it is absent.
: HEAD-SCAN ( -- n )
   BLANK-LINE$ {: mark:ptr marku:n :}
   REQ-U @ {: u:n :}
   u marku < if 0 exit then
   u marku - 1+ 0 ?do
      REQ-BUF i + marku mark marku STR= if i marku + unloop exit then
   loop 0 ;


: DELIM? ( n -- bool ) {: c:n :}
   c $20 = if true exit then
   c $0D = if true exit then
   c $0A = if true exit then
   false ;


: TOKEN-END ( n -- n ) {: at:n :}
   at begin dup HEAD-U @ < while
      dup REQ-BUF + c@ DELIM? if exit then
      1+
   repeat ;


: METHOD$ ( -- ptr u8 n )
   REQ-BUF 0 TOKEN-END ;


: PATH$ ( -- ptr u8 n )
   0 TOKEN-END 1+ {: at:n :}
   REQ-BUF at + at TOKEN-END at - ;


: METHOD-IS? ( ptr u8 n -- bool ) {: want:ptr wantu:n :}
   METHOD$ want wantu STR= ;


: PATH-IS? ( ptr u8 n -- bool ) {: want:ptr wantu:n :}
   PATH$ want wantu STR= ;


\ ---- the response the server task writes -------------------------------------

: RES-RESET ( -- )
   0 RES-U ! ;


: RES-C+ ( n -- ) {: c:n :}
   RES-U @ RES-CAP >= if E-STR-CAPACITY throw then
   c RES-BUF RES-U @ + c!
   RES-U @ 1+ RES-U ! ;


: RES+ ( ptr u8 n -- ) {: text:ptr u:n :}
   RES-U @ u + RES-CAP > if E-STR-CAPACITY throw then
   text RES-BUF RES-U @ + u BYTE-COPY
   RES-U @ u + RES-U ! ;


: CRLF+ ( -- )
   $0D RES-C+ $0A RES-C+ ;


: NUM-BUILD ( n -- ) {: value:n :}    \ least significant digit first
   0 NUM-N !
   value begin dup 0 > while
      dup 10 mod $30 + NUM-BUF NUM-N @ + c!
      NUM-N @ 1+ NUM-N !
      10 /
   repeat drop ;


: RES-NUM ( n -- ) {: value:n :}
   value 0= if $30 RES-C+ exit then
   value NUM-BUILD
   NUM-N @ 0 ?do NUM-BUF NUM-N @ 1- i - + c@ RES-C+ loop ;


: STATUS-LINE ( ptr u8 n -- ) {: text:ptr u:n :}
   s" HTTP/1.0 " RES+ text u RES+ CRLF+ ;


: COOKIE-HEADER ( -- )
   SEND-COOKIE @ 0= if exit then
   s" Set-Cookie: " RES+ COOKIE-PAIR$ RES+ s" ; path=/" RES+ CRLF+ ;


\ Content-Length plus the close makes the body length unambiguous to the client
\ either way, which is what the exact-bytes assertions rest on.
: RESPOND ( ptr u8 n ptr u8 n -- ) {: text:ptr u:n body:ptr bodyu:n :}
   RES-RESET
   text u STATUS-LINE
   s" Content-Type: text/plain" RES+ CRLF+
   s" Content-Length: " RES+ bodyu RES-NUM CRLF+
   COOKIE-HEADER
   s" Connection: close" RES+ CRLF+ CRLF+
   body bodyu RES+ ;


: RESPOND-EMPTY ( ptr u8 n -- ) {: text:ptr u:n :}
   RES-RESET
   text u STATUS-LINE
   s" Connection: close" RES+ CRLF+ CRLF+ ;


\ ---- the server task ---------------------------------------------------------

: SERVER-FAILED ( TCP4:errno -- )
   TCP4:ERRNO>N SERVER-ERRNO !
   1 SERVER-BAD +! ;


: SERVER-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF SERVER-FAILED ENDOF
   ;MATCH ;


\ Shutting down or closing a connection whose peer has already gone answers its
\ own errno; that is the peer's departure, not a fault of this server.
: PEER-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;


: LISTENER@ ( -- TCP4:listener )
   SERVER-LISTENER @ TCP4:>LISTENER ;


: REQ-CHUNK ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   conn REQ-BUF REQ-U @ + REQ-CAP REQ-U @ - TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N REQ-U @ + REQ-U ! true ENDOF
      closed OF drop false ENDOF
      failed OF SERVER-FAILED false ENDOF
   ;MATCH ;


: REQ-READ ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   0 REQ-U ! 0 HEAD-U !
   begin
      HEAD-SCAN dup 0 > if HEAD-U ! true exit then drop
      REQ-U @ REQ-CAP >= if false exit then
      conn REQ-CHUNK 0= if false exit then
   again ;


: DRAIN-STEP ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   conn REQ-BUF REQ-CAP TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF drop true ENDOF
      closed OF drop false ENDOF
      failed OF drop false ENDOF
   ;MATCH ;


\ Whatever the client already sent is read off before the descriptor goes, so
\ the close is a FIN and never a reset that would lose the response.
: DRAIN ( TCP4:connection -- ) {: conn:TCP4:connection :}
   DRAIN-TRIES 0 do conn DRAIN-STEP 0= if unloop exit then loop ;


: SEND-RESPONSE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn RES-BUF RES-U @ TCP4:TRANSFER-BYTES TCP4:WRITE SERVER-STATUS
   conn TCP4:SENDING TCP4:SHUTDOWN PEER-STATUS
   conn DRAIN
   conn TCP4:CLOSE PEER-STATUS ;


\ The client is answered nothing at all and the connection is held until the
\ client itself gives up: a cancel, a halt or a stall test is what ends it.
: SERVE-STALL ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn DRAIN
   conn TCP4:CLOSE PEER-STATUS ;


\ The head carries the WHOLE body's length, so nothing about this response is
\ chunked or short: only its bytes are late.
: DRIBBLE-HEAD ( -- )
   RES-RESET
   s" 200 OK" STATUS-LINE
   s" Content-Type: text/plain" RES+ CRLF+
   s" Content-Length: " RES+ DRIBBLE-U RES-NUM CRLF+
   s" Connection: close" RES+ CRLF+ CRLF+ ;


\ A client that already gave up answers this write with its own errno, which is
\ its departure and not a fault of this server.
: DRIBBLE-STEP! ( TCP4:connection n -- bool ) {: conn:TCP4:connection at:n :}
   conn DRIBBLE-BUF at + DRIBBLE-STEP TCP4:TRANSFER-BYTES TCP4:WRITE
   MATCH TCP4:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;


\ Steady and slow, never stalled: every gap is far shorter than a low-speed
\ window, so no stall test under this rate ends the transfer and only a limit on
\ the WHOLE transfer can.
: DRIBBLE-BODY ( TCP4:connection -- ) {: conn:TCP4:connection :}
   0 begin dup DRIBBLE-U < while
      DRIBBLE-GAP-MS >MS TASK:SLEEP
      dup conn swap DRIBBLE-STEP! 0= if drop exit then
      DRIBBLE-STEP +
   repeat drop ;


: SERVE-DRIBBLE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   DRIBBLE-HEAD
   conn RES-BUF RES-U @ TCP4:TRANSFER-BYTES TCP4:WRITE SERVER-STATUS
   conn DRIBBLE-BODY
   conn TCP4:SENDING TCP4:SHUTDOWN PEER-STATUS
   conn DRAIN
   conn TCP4:CLOSE PEER-STATUS ;


: SERVE-HELLO ( TCP4:connection -- ) {: conn:TCP4:connection :}
   s" If-Modified-Since:" REQ-HAS-CI? if
      s" 304 Not Modified" RESPOND-EMPTY
   else
      s" 200 OK" HELLO$ RESPOND
   then
   conn SEND-RESPONSE ;


: COOKIE-SENT? ( -- bool )
   s" Cookie:" REQ-HAS-CI? 0= if false exit then
   COOKIE-PAIR$ REQ-HAS? ;


: COOKIE-BODY$ ( -- ptr u8 n )
   COOKIE-SENT? if COOKIE-PAIR$ exit then
   NO-COOKIE$ ;


: SERVE-COOKIE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   1 SEND-COOKIE !
   s" 200 OK" COOKIE-BODY$ RESPOND
   0 SEND-COOKIE !
   conn SEND-RESPONSE ;


: SERVE-MISSING ( TCP4:connection -- ) {: conn:TCP4:connection :}
   s" 404 Not Found" s" not found" RESPOND
   conn SEND-RESPONSE ;


: SERVE-HEADERS ( TCP4:connection -- ) {: conn:TCP4:connection :}
   RES-RESET HEADERS$ RES+ JSON$ RES+
   conn SEND-RESPONSE ;


: SERVE-HEADERS-B ( TCP4:connection -- ) {: conn:TCP4:connection :}
   RES-RESET HEADERS-B$ RES+ JSON$ RES+
   conn SEND-RESPONSE ;


: REDIRECT-RESPONSE ( -- )
   RES-RESET
   S\" HTTP/1.0 302 Found\r\nLocation: /headers\r\nCache-Control: no-store\r\nExpires: Thu, 01 Jan 1970 00:00:00 GMT\r\nX-Oversized: " RES+
   600 0 do $78 RES-C+ loop
   CRLF+
   s" Content-Length: 0" RES+ CRLF+
   s" Connection: close" RES+ CRLF+ CRLF+ ;


: SERVE-REDIRECT ( TCP4:connection -- ) {: conn:TCP4:connection :}
   REDIRECT-RESPONSE conn SEND-RESPONSE ;


: SERVE-INTERIM ( TCP4:connection -- ) {: conn:TCP4:connection :}
   RES-RESET
   S\" HTTP/1.1 100 Continue\r\nX-Interim: one\r\n\r\nHTTP/1.1 103 Early Hints\r\nCache-Control: no-store\r\n\r\n" RES+
   INTERIM-FINAL$ RES+
   S\" 2\r\n{}\r\n0\r\nCache-Control: private\r\nX-Trailer: excluded\r\n\r\n" RES+
   conn SEND-RESPONSE ;


: SERVE-UNIMPLEMENTED ( TCP4:connection -- ) {: conn:TCP4:connection :}
   s" 501 Not Implemented" s" not implemented" RESPOND
   conn SEND-RESPONSE ;


: ROUTE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   s" POST" METHOD-IS? if conn SERVE-UNIMPLEMENTED exit then
   s" DELETE" METHOD-IS? if conn SERVE-UNIMPLEMENTED exit then
   PATH-STALL$ PATH-IS? if conn SERVE-STALL exit then
   PATH-DRIBBLE$ PATH-IS? if conn SERVE-DRIBBLE exit then
   PATH-HEADERS$ PATH-IS? if conn SERVE-HEADERS exit then
   PATH-HEADERS-B$ PATH-IS? if conn SERVE-HEADERS-B exit then
   PATH-REDIRECT$ PATH-IS? if conn SERVE-REDIRECT exit then
   PATH-INTERIM$ PATH-IS? if conn SERVE-INTERIM exit then
   PATH-HELLO$ PATH-IS? if conn SERVE-HELLO exit then
   PATH-COOKIE$ PATH-IS? if conn SERVE-COOKIE exit then
   conn SERVE-MISSING ;


\ The count moves atomically because a case reads it WHILE the server task is
\ serving: the cancel and halt cases wait for their own requests to arrive
\ before ending them.
: SERVE-ONE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn REQ-READ 0= if conn TCP4:CLOSE PEER-STATUS exit then
   1 SERVER-HITS atomic-add drop
   conn ROUTE ;


\ The wakeup connection that ends the accept loop carries no request, so the
\ stop flag is read before anything is parsed.
: SERVE-ACCEPTED ( TCP4:connection TCP4:address TCP4:port -- )
   {: conn:TCP4:connection peer:TCP4:address peer-port:TCP4:port :}
   SERVER-STOP atomic@ 0 <> if conn TCP4:CLOSE PEER-STATUS exit then
   conn SERVE-ONE ;


: SERVE-LOOP ( -- )
   begin
      SERVER-STOP atomic@ 0 <> if exit then
      LISTENER@ TCP4:ACCEPT
      MATCH TCP4:accept-result
         accepted OF SERVE-ACCEPTED ENDOF
         failed OF SERVER-FAILED 1 SERVER-STOP atomic-add drop ENDOF
      ;MATCH
   again ;


: BIND-LOOPBACK ( ptr n -- bool ) {: cell:ptr :}
   LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF TCP4:LISTENER>N cell ! true ENDOF
      failed OF SERVER-FAILED false ENDOF
   ;MATCH ;


: PUBLISH-PORT ( ptr n ptr n -- bool ) {: lcell:ptr pcell:ptr :}
   lcell @ TCP4:>LISTENER TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N pcell ! TCP4:ADDRESS>N drop true ENDOF
      failed OF SERVER-FAILED false ENDOF
   ;MATCH ;


\ A listener on a free loopback port, its handle and then its port stored in the
\ two cells; a refusal is recorded as the server side's fault.
: LISTEN-LOOPBACK ( ptr n ptr n -- bool ) {: lcell:ptr pcell:ptr :}
   lcell BIND-LOOPBACK 0= if false exit then
   lcell @ TCP4:>LISTENER BACKLOG TCP4:LISTEN SERVER-STATUS
   SERVER-BAD @ 0 <> if false exit then
   lcell pcell PUBLISH-PORT ;


: SERVER-START ( -- bool )
   SERVER-LISTENER SERVER-PORT LISTEN-LOOPBACK ;


\ The port is published before readiness is, so a reader that sees ready sees a
\ listening socket and its port together.
: SERVER-WORK ( -- )
   SERVER-START 0= if 1 SERVER-STOP atomic-add drop then
   1 SERVER-READY atomic-add drop
   SERVE-LOOP
   1 SERVER-DONE atomic-add drop ;


\ ---- starting and stopping it from the main task -----------------------------

: WAIT-READY ( -- )
   READY-TRIES 0 do
      SERVER-READY atomic@ 0 > if unloop exit then
      TASK:PAUSE
   loop
   E-PROC-TIMEOUT throw ;


: WAIT-DONE ( -- )
   READY-TRIES 0 do
      SERVER-DONE atomic@ 0 > if unloop exit then
      TASK:PAUSE
   loop
   E-PROC-TIMEOUT throw ;


: WAKE-SERVER ( -- )
   LOOPBACK TCP4:ADDRESS SERVER-PORT @ TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF TCP4:CLOSE PEER-STATUS ENDOF
      failed OF drop ENDOF
   ;MATCH ;


: START-SERVER ( -- )
   ['] SERVER-WORK SERVER-TASK TASK:ACTIVATE
   WAIT-READY ;


: STOP-SERVER ( -- )
   1 SERVER-STOP atomic-add drop
   WAKE-SERVER
   WAIT-DONE
   SERVER-TASK TASK:KILL
   LISTENER@ TCP4:CLOSE-LISTENER SERVER-STATUS ;


\ Nobody accepts on this listener: the kernel completes a client's connection
\ and queues whatever it sends, and nothing ever answers. A transfer that only
\ its own ceiling can end is sent here, because that ceiling can fire before
\ the request is even sent - the scheduler may hold the client that long - and
\ the counting server must see only requests whose arrival the test guarantees.
: OPEN-SILENT ( -- )
   s" a listener that never answers opens on loopback" T-LABEL
   SILENT-LISTENER SILENT-PORT LISTEN-LOOPBACK TTRUE ;


: CLOSE-SILENT ( -- )
   SILENT-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER SERVER-STATUS ;


\ ---- fixtures ----------------------------------------------------------------

: UNDER-ROOT ( ptr u8 n ptr u8 -- n ) {: name:ptr nameu:n dst:ptr :}
   ROOT$ name nameu dst JOIN-PATH ;


: MAKE-ROOT ( -- )
   s" habu-curl" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U !
   s" hello.txt" FILE-BUF UNDER-ROOT FILE-U !
   s" cookies.txt" JAR-BUF UNDER-ROOT JAR-U ! ;


\ The only file on disk is the one the file:// case must NOT be able to read.
: WRITE-FIXTURES ( -- )
   FILE$ HELLO$ WRITE-ALL ;


: FILE-URL$ ( -- ptr u8 n )
   SB-RESET
   s" file://" SB-APPEND
   FILE$ SB-APPEND
   SB$ ;


: URL-AT ( n ptr u8 n -- ptr u8 n ) {: port:n path:ptr pathu:n :}
   SB-RESET
   s" http://127.0.0.1:" SB-APPEND
   port FMT:SB-U
   path pathu SB-APPEND
   SB$ ;


: URL-FOR ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   SERVER-PORT @ path pathu URL-AT ;


\ ---- driving package CURL ----------------------------------------------------

: OPEN-HANDLE ( -- CURL:handle )
   CURL:INIT MATCH CURL:init-result
      ready OF ENDOF
      failed OF CURL:CODE>N drop CURL:E-STATE throw ENDOF
   ;MATCH ;


: EXPECT-OK ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF CURL:CODE>N 0 T= ENDOF
   ;MATCH ;


: RECORD-RESPONSE ( CURL:http-status len -- )
   LEN>N LAST-LEN !
   CURL:HTTP-STATUS>N LAST-STATUS !
   KIND-RESPONSE LAST-KIND ! ;


: RECORD-TRUNCATED ( CURL:http-status len -- )
   LEN>N LAST-LEN !
   CURL:HTTP-STATUS>N LAST-STATUS !
   KIND-TRUNCATED LAST-KIND ! ;


: RECORD-FAILED ( CURL:code -- )
   CURL:CODE>N LAST-CODE !
   KIND-FAILED LAST-KIND ! ;


: BODY-FILL ( -- )
   BODY-CAP 0 do FILL-BYTE BODY-BUF i + c! loop ;


: RESULT-RESET ( -- )
   0 LAST-STATUS ! 0 LAST-LEN ! 0 LAST-CODE ! KIND-FAILED LAST-KIND !
   BODY-FILL ;


: FETCH ( CURL:handle len -- ) {: subject:CURL:handle capacity:len :}
   RESULT-RESET
   subject BODY-BUF capacity CURL:PERFORM
   MATCH CURL:fetch-result
      response OF RECORD-RESPONSE ENDOF
      truncated OF RECORD-TRUNCATED ENDOF
      failed OF RECORD-FAILED ENDOF
   ;MATCH ;


: BODY$ ( -- ptr u8 n )
   BODY-BUF LAST-LEN @ ;


: UNTOUCHED? ( -- bool )
   BODY-CAP 0 do BODY-BUF i + c@ FILL-BYTE <> if false unloop exit then loop
   true ;


: HEADER-FILL ( -- )
   HEADER-CAP 0 do FILL-BYTE HEADER-BUF i + c! loop ;


: HEADER-UNTOUCHED? ( -- bool )
   HEADER-CAP 0 do HEADER-BUF i + c@ FILL-BYTE <> if false unloop exit then loop
   true ;


: HEADER-COPY ( CURL:handle n -- ) {: subject:CURL:handle capacity:n :}
   HEADER-FILL
   subject HEADER-BUF capacity >LEN CURL:HEADERS
   MATCH CURL:header-result
      complete OF LEN>N HEADER-LEN ! 0 HEADER-KIND ! ENDOF
      truncated OF LEN>N HEADER-LEN ! 1 HEADER-KIND ! ENDOF
   ;MATCH ;


: HEADER-EXACT ( CURL:handle ptr u8 n -- )
   {: subject:CURL:handle want:ptr u:n :}
   subject u HEADER-COPY
   HEADER-KIND @ 0 T=
   HEADER-LEN @ u T=
   HEADER-BUF u want u T$=
   HEADER-BUF u + c@ FILL-BYTE T= ;


: HEADER-REFUSED ( CURL:handle -- ) {: subject:CURL:handle :}
   subject HEADER-HANDLE !
   HEADER-FILL
   s" unavailable response headers leave the destination untouched" T-LABEL
   [: HEADER-HANDLE @ HEADER-BUF HEADER-CAP >LEN CURL:HEADERS drop ;]
      CURL:E-STATE TTHROWSQ
   HEADER-UNTOUCHED? TTRUE ;


: HEADER-BAD-CAP ( CURL:handle -- ) {: subject:CURL:handle :}
   subject HEADER-HANDLE !
   HEADER-FILL
   s" a negative header capacity is refused before the destination is touched" T-LABEL
   [: HEADER-HANDLE @ HEADER-BUF -1 >LEN CURL:HEADERS drop ;]
      CURL:E-OPERAND TTHROWSQ
   HEADER-UNTOUCHED? TTRUE ;


: HEADER-TRANSCRIPT ( -- )
   s" HB_TMP" GETENV {: root:ptr rootu:n :}
   rootu 0= if exit then
   root rootu s" curl-headers-transcript.txt" TRANSCRIPT-PATH JOIN-PATH
      {: pathu:n :}
   SB-RESET
   S\" result=response status=200 body-len=2\nheaders:\n" SB-APPEND
   HEADER-BUF HEADER-LEN @ SB-APPEND
   S\" body:{}\n" SB-APPEND
   SB$ {: transcript:ptr u:n :}
   transcript TRANSCRIPT-BUF u BYTE-COPY
   u TRANSCRIPT-U !
   TRANSCRIPT-PATH pathu TRANSCRIPT-BUF TRANSCRIPT-U @ WRITE-ALL
   TRANSCRIPT-PATH pathu TRANSCRIPT-BUF HEADER-CAP READ-ALL
      TRANSCRIPT-U @ T=
   TRANSCRIPT-BUF TRANSCRIPT-U @ SB$ T$= ;


: TEXT-AT? ( ptr u8 n ptr u8 n n -- bool )
   {: text:ptr u:n needle:ptr needleu:n at:n :}
   at needleu + u > if false exit then
   text at + needleu needle needleu STR= ;


\ The jar is libcurl's Netscape format, where the name and the value are two
\ tab-separated fields, so each is looked for on its own. The jar is read into
\ its own buffer: REQ-BUF belongs to the server task.
: JAR-HAS? ( ptr u8 n -- bool ) {: needle:ptr needleu:n :}
   JAR$ JAR-TEXT-BUF RES-CAP READ-ALL {: u:n :}
   u 0 ?do JAR-TEXT-BUF u needle needleu i TEXT-AT? if true unloop exit then loop
   false ;


: GET-READY ( ptr u8 n -- CURL:handle ) {: path:ptr pathu:n :}
   OPEN-HANDLE {: subject:CURL:handle :}
   subject path pathu URL-FOR CURL:URL! EXPECT-OK
   subject REQUEST-MS >MS CURL:TIMEOUT! EXPECT-OK
   subject ;


\ A transfer to the peer that never answers, which only its ceiling ends.
: STALL-READY ( -- CURL:handle )
   OPEN-HANDLE {: subject:CURL:handle :}
   subject SILENT-PORT @ PATH-STALL$ URL-AT CURL:URL! EXPECT-OK
   subject STALL-MS >MS CURL:TIMEOUT! EXPECT-OK
   subject ;


\ ---- cases -------------------------------------------------------------------

: TEST-GET ( -- )
   PATH-HELLO$ GET-READY {: subject:CURL:handle :}
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" GET answers 200 with the fixture's exact bytes" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 200 T=
   BODY$ HELLO$ T$= ;


: TEST-MISSING ( -- )
   s" /nothing-here.txt" GET-READY {: subject:CURL:handle :}
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" an unknown path answers 404, not a transfer failure" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 404 T= ;


\ The server answers 304 only when it parsed the header out of the request, so
\ the status is the proof that HEADER+ reached the wire.
: TEST-HEADER ( -- )
   PATH-HELLO$ GET-READY {: subject:CURL:handle :}
   subject s" If-Modified-Since: Sat, 01 Jan 2050 00:00:00 GMT" CURL:HEADER+ EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" a request header reaches the server and changes its answer" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 304 T=
   LAST-LEN @ 0 T= ;


: TEST-POST ( -- )
   PATH-HELLO$ GET-READY {: subject:CURL:handle :}
   subject s" X-Habu-Probe: yes" CURL:HEADER+ EXPECT-OK
   subject s" Content-Type: text/plain" CURL:HEADER+ EXPECT-OK
   subject s" name=habu&kind=post" CURL:BODY! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" POST with headers and a body answers 501" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 501 T= ;


: TEST-METHOD ( -- )
   PATH-HELLO$ GET-READY {: subject:CURL:handle :}
   subject s" DELETE" CURL:METHOD! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" a custom method reaches the server" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 501 T= ;


\ First request: no cookie is sent, the server sets one, and CLEANUP writes it
\ to the jar.
: COOKIE-FIRST ( -- )
   PATH-COOKIE$ GET-READY {: subject:CURL:handle :}
   subject s" " CURL:COOKIE-FILE! EXPECT-OK
   subject JAR$ CURL:COOKIE-JAR! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" the first request carries no cookie and the server sets one" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 200 T=
   BODY$ NO-COOKIE$ T$= ;


\ Second request: the jar is read back and the server sees the cookie return.
: COOKIE-SECOND ( -- )
   PATH-COOKIE$ GET-READY {: subject:CURL:handle :}
   subject JAR$ CURL:COOKIE-FILE! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" the cookie comes back on the second request through the jar" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   BODY$ COOKIE-PAIR$ T$= ;


: TEST-COOKIES ( -- )
   COOKIE-FIRST
   JAR$ FILE? TTRUE
   s" probe" JAR-HAS? TTRUE
   s" value1" JAR-HAS? TTRUE
   COOKIE-SECOND ;


: TEST-TRUNCATED ( -- )
   PATH-HELLO$ GET-READY {: subject:CURL:handle :}
   subject 3 >LEN FETCH
   subject CURL:CLEANUP
   s" a body past the caller's span is truncated, never silently short" T-LABEL
   LAST-KIND @ KIND-TRUNCATED T=
   LAST-STATUS @ 200 T=
   LAST-LEN @ 6 T=
   BODY-BUF 3 s" hel" T$= ;


: TEST-RESPONSE-HEADERS ( -- )
   PATH-HEADERS$ GET-READY {: subject:CURL:handle :}
   subject HEADER-REFUSED
   subject BODY-CAP >LEN FETCH
   s" PERFORM retains the exact status-headed block and duplicate fields" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   BODY$ JSON$ T$=
   subject HEADER-BAD-CAP
   subject HEADERS$ HEADER-EXACT
   HEADER-TRANSCRIPT
   subject 0 HEADER-COPY
   HEADER-KIND @ 1 T=
   HEADER-LEN @ HEADERS$ nip T=
   HEADER-UNTOUCHED? TTRUE
   subject HEADER-LEN @ 1- HEADER-COPY
   HEADER-KIND @ 1 T=
   HEADER-LEN @ HEADERS$ nip T=
   HEADER-BUF HEADER-LEN @ 1- + c@ FILL-BYTE T=
   subject HEADERS$ HEADER-EXACT
   HEADER-BUF HEADER-SAVED HEADER-LEN @ BYTE-COPY
   subject FILE-URL$ CURL:URL! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   LAST-KIND @ KIND-FAILED T=
   LAST-CODE @ 1 T=
   subject HEADER-REFUSED
   subject CURL:CLEANUP
   HEADER-SAVED HEADER-LEN @ HEADERS$ T$= ;


: TEST-HEADER-TRUNCATIONS ( -- )
   PATH-HEADERS$ GET-READY {: subject:CURL:handle :}
   subject 1 >LEN FETCH
   s" body and header truncation report independent whole lengths" T-LABEL
   LAST-KIND @ KIND-TRUNCATED T=
   LAST-LEN @ 2 T=
   subject 1 HEADER-COPY
   HEADER-KIND @ 1 T=
   HEADER-LEN @ HEADERS$ nip T=
   HEADER-BUF c@ $48 T=
   HEADER-BUF 1+ c@ FILL-BYTE T=
   subject HEADERS$ HEADER-EXACT
   subject CURL:CLEANUP
   PATH-HEADERS$ GET-READY {: second:CURL:handle :}
   second 1 >LEN FETCH
   LAST-KIND @ KIND-TRUNCATED T=
   second HEADERS$ HEADER-EXACT
   second CURL:CLEANUP ;


: TEST-HEADER-REDIRECT ( -- )
   PATH-REDIRECT$ GET-READY {: subject:CURL:handle :}
   subject true CURL:FOLLOW! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   s" FOLLOW selects only the final block past an oversized redirect" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   BODY$ JSON$ T$=
   subject HEADERS$ HEADER-EXACT
   subject CURL:CLEANUP
   PATH-REDIRECT$ GET-READY {: direct:CURL:handle :}
   direct BODY-CAP >LEN FETCH
   LAST-STATUS @ 302 T=
   direct 0 HEADER-COPY
   HEADER-LEN @ 600 > TTRUE
   direct HEADER-CAP HEADER-COPY
   HEADER-KIND @ 0 T=
   HEADER-BUF HEADER-LEN @ S\" HTTP/1.0 302 Found\r\nLocation: /headers\r\nCache-Control: no-store\r\n" 0 TEXT-AT? TTRUE
   direct CURL:CLEANUP ;


: TEST-HEADER-INTERIM ( -- )
   PATH-INTERIM$ GET-READY {: subject:CURL:handle :}
   subject BODY-CAP >LEN FETCH
   s" 100 and 103 are superseded; chunked trailers do not extend the block" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   BODY$ JSON$ T$=
   subject INTERIM-FINAL$ HEADER-EXACT
   subject CURL:CLEANUP ;


: TEST-TIMEOUT ( -- )
   STALL-READY {: subject:CURL:handle :}
   subject BODY-CAP >LEN FETCH
   subject HEADER-REFUSED
   subject CURL:CLEANUP
   s" a server that never answers ends as CURLE_OPERATION_TIMEDOUT" T-LABEL
   LAST-KIND @ KIND-FAILED T=
   LAST-CODE @ 28 T= ;


\ A client its ceiling ended can return while the serial server still drains
\ and closes that connection, so the next timed case waits for this answer: the
\ server takes connections one at a time in the order they were made, and it
\ answers a request made now only once it is done with every earlier one -
\ whether that one carried a request or its ceiling ended it before one left.
: SERVER-BARRIER ( -- )
   PATH-HELLO$ GET-READY {: subject:CURL:handle :}
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" the serial server answers after a transfer its ceiling ended" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 200 T= ;


\ The ceiling TIMEOUT! sets ends a transfer that is slow but perfectly alive:
\ exactly the loss a low-speed limit exists to avoid. That ceiling can also fire
\ before the request is sent, when the scheduler holds the client that long, so
\ whether the request arrived is found out after the barrier, not assumed; a run
\ whose request never arrived shows no more than TEST-TIMEOUT does.
: DRIBBLE-CEILING ( -- )
   SERVER-HITS atomic@ {: before:n :}
   PATH-DRIBBLE$ GET-READY {: subject:CURL:handle :}
   subject CEILING-MS >MS CURL:TIMEOUT! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" a whole-transfer ceiling ends the dribble though it never stalled" T-LABEL
   LAST-KIND @ KIND-FAILED T=
   LAST-CODE @ 28 T=
   SERVER-BARRIER
   SERVER-HITS atomic@ before - 1- {: arrived:n :}
   s" the dribble a ceiling ended reached the server at most once" T-LABEL
   arrived 1 <= TTRUE
   arrived CEILING-HITS ! ;


\ The same dribble under a low-speed limit far below its rate finishes whole:
\ the stall test tolerates slow, and only slow.
: DRIBBLE-LOW-SPEED ( -- )
   PATH-DRIBBLE$ GET-READY {: subject:CURL:handle :}
   subject LOW-RATE LOW-SECONDS CURL:LOW-SPEED! EXPECT-OK
   mono-ns {: started:n :}
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   LAST-KIND @ KIND-FAILED = if
      s" curl low-speed code: " type LAST-CODE @ .
      s" elapsed ms: " type mono-ns started - NS-PER-MS / . cr
   then
   s" a low-speed limit under the dribble's rate lets it finish whole" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 200 T=
   LAST-LEN @ DRIBBLE-U T=
   BODY$ DRIBBLE$ T$= ;


: ELAPSED-MS ( n -- n ) {: started:n :}
   mono-ns started - NS-PER-MS / ;


\ A transfer that never moves a byte is what the stall test must end, and the
\ elapsed time is what says it did: the handle's own ceiling is REQUEST-MS away.
: STALL-LOW-SPEED ( -- )
   PATH-STALL$ GET-READY {: subject:CURL:handle :}
   subject 1 LOW-SECONDS CURL:LOW-SPEED! EXPECT-OK
   mono-ns {: started:n :}
   subject BODY-CAP >LEN FETCH
   started ELAPSED-MS {: elapsed:n :}
   subject CURL:CLEANUP
   s" a stalled transfer ends under the low-speed limit alone" T-LABEL
   LAST-KIND @ KIND-FAILED T=
   LAST-CODE @ 28 T=
   elapsed STALL-MOST-MS < TTRUE ;


: TEST-LOW-SPEED ( -- )
   DRIBBLE-CEILING
   DRIBBLE-LOW-SPEED
   STALL-LOW-SPEED ;


: TEST-NO-URL ( -- )
   OPEN-HANDLE {: subject:CURL:handle :}
   subject BODY-CAP >LEN FETCH
   subject HEADER-REFUSED
   subject CURL:CLEANUP
   s" a handle with no URL fails and carries the curl code" T-LABEL
   LAST-KIND @ KIND-FAILED T=
   LAST-CODE @ 0 T<> ;


\ INIT restricts the schemes, so a readable local file is refused by libcurl
\ before anything is opened and none of its bytes reach the caller's span.
: TEST-FILE-SCHEME ( -- )
   OPEN-HANDLE {: subject:CURL:handle :}
   subject FILE-URL$ CURL:URL! EXPECT-OK
   subject REQUEST-MS >MS CURL:TIMEOUT! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" a file:// URL is refused and its bytes never reach the buffer" T-LABEL
   FILE$ FILE? TTRUE
   LAST-KIND @ KIND-FAILED T=
   LAST-CODE @ 1 T=
   UNTOUCHED? TTRUE ;


: NULL-HANDLE ( -- )
   0 CURL:>HANDLE CURL:CLEANUP ;


: EMBEDDED-NUL ( -- )
   OPEN-HANDLE {: subject:CURL:handle :}
   subject S\" http://127.0.0.1\x00/evil" CURL:URL! EXPECT-OK
   subject CURL:CLEANUP ;


: LOW-SPEED-RATE ( -- )
   OPEN-HANDLE {: subject:CURL:handle :}
   subject -1 LOW-SECONDS CURL:LOW-SPEED! EXPECT-OK
   subject CURL:CLEANUP ;


: LOW-SPEED-WINDOW ( -- )
   OPEN-HANDLE {: subject:CURL:handle :}
   subject LOW-RATE OVER-LONG CURL:LOW-SPEED! EXPECT-OK
   subject CURL:CLEANUP ;


\ Zero in both values is libcurl's own spelling for "no stall test at all".
: LOW-SPEED-PAIRS ( -- )
   OPEN-HANDLE {: subject:CURL:handle :}
   subject 0 0 CURL:LOW-SPEED! EXPECT-OK
   subject LOW-RATE 30 CURL:LOW-SPEED! EXPECT-OK
   subject CURL:CLEANUP ;


: TEST-REFUSALS ( -- )
   s" a handle that was never opened is refused" T-LABEL
   [: NULL-HANDLE ;] CURL:E-STATE TTHROWSQ
   s" a NUL inside a URL is refused, never silently cut" T-LABEL
   [: EMBEDDED-NUL ;] CURL:E-OPERAND TTHROWSQ
   s" a low-speed rate below zero is refused" T-LABEL
   [: LOW-SPEED-RATE ;] CURL:E-OPERAND TTHROWSQ
   s" a low-speed window past the C long ceiling is refused" T-LABEL
   [: LOW-SPEED-WINDOW ;] CURL:E-OPERAND TTHROWSQ
   s" a handle takes the disabling pair and a real one" T-LABEL
   LOW-SPEED-PAIRS ;


\ ---- many transfers on one task ----------------------------------------------
\ Every wait below is bounded, so a transfer the loop never answers is a FAIL
\ and not a hung suite.

2000 constant WAIT-MS                   \ the bound on every wait in these cases
$20 constant MANY-N                     \ the concurrent transfers of case one
$40 constant MANY-CAP                   \ one span each, far past the six-byte body
4 constant STALL-GROUP                  \ one stalled transfer and three healthy ones

create MANY-BUF MANY-N MANY-CAP * allot

TEST-ALIGN8
variable DURING-THREADS
variable HALT-PARKED
variable HALT-RC
variable DONE-PARKED
variable DONE-COLLECTED
variable DONE-RC
variable OWNER-RC

8 BUFFER: DONE-STATE-CELL

MANY-N TYPED-BUFFER MANY-HANDLES CURL:handle
TYPED-VARIABLE COLD-HANDLE CURL:handle
TYPED-VARIABLE EXTRA-HANDLE CURL:handle
TYPED-VARIABLE OWNER-HANDLE CURL:handle
TYPED-VARIABLE HALT-HANDLE CURL:handle
TYPED-VARIABLE DONE-HANDLE CURL:handle

TASK:MIN-STACK TASK:TASK OWNER-TASK
TASK:MIN-STACK TASK:TASK HALT-TASK
TASK:MIN-STACK TASK:TASK DONE-TASK


: REACHED? ( ptr n n -- bool ) {: cell:ptr want:n :}
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      cell atomic@ want >= if true exit then
      mono-ns deadline > if false exit then
      TASK:PAUSE
   again ;


: ENDED? ( ptr n -- bool ) {: tcb:ptr :}
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      tcb TASK:DONE? if true exit then
      mono-ns deadline > if false exit then
      TASK:PAUSE
   again ;


\ An abandoned record is given back by the loop and not by the task that left
\ it, so the stop is retried to a bound rather than asserted on the first try.
: STOPPED? ( -- bool )
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      [: CURL:LOOP-STOP ;] catch 0= if true exit then
      mono-ns deadline > if false exit then
      TASK:PAUSE
   again ;


: WAIT-HIT ( n -- ) {: before:n :}
   SERVER-HITS before 1+ REACHED? 0= if E-PROC-TIMEOUT throw then ;


\ The loop's answer for one handle, recorded exactly as FETCH records PERFORM's,
\ so the two paths are compared field by field and not by their spelling.
: AWAIT-ONE ( CURL:handle -- ) {: subject:CURL:handle :}
   subject CURL:AWAIT
   MATCH CURL:fetch-result
      response OF RECORD-RESPONSE ENDOF
      truncated OF RECORD-TRUNCATED ENDOF
      failed OF RECORD-FAILED ENDOF
   ;MATCH ;


: DROP-FETCH ( CURL:fetch-result -- )
   MATCH CURL:fetch-result
      response OF 2drop ENDOF
      truncated OF 2drop ENDOF
      failed OF drop ENDOF
   ;MATCH ;


: MULTI-FETCH ( CURL:handle len -- ) {: subject:CURL:handle capacity:len :}
   RESULT-RESET
   subject BODY-BUF capacity CURL:START EXPECT-OK
   subject AWAIT-ONE ;


\ One request through PERFORM and the same one through the loop: the answers
\ must agree, status, length and bytes.
: PARITY-WHOLE ( -- )
   PATH-HELLO$ GET-READY {: one:CURL:handle :}
   one BODY-CAP >LEN FETCH
   one CURL:CLEANUP
   s" PERFORM answers 200 with the fixture's bytes" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   BODY$ HELLO$ T$=
   LAST-STATUS @ {: status:n :}
   LAST-LEN @ {: u:n :}
   PATH-HELLO$ GET-READY {: two:CURL:handle :}
   two BODY-CAP >LEN MULTI-FETCH
   two CURL:CLEANUP
   s" START and AWAIT answer the same fetch-result PERFORM did" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ status T=
   LAST-LEN @ u T=
   BODY$ HELLO$ T$= ;


\ A span too short truncates on both paths, with the WHOLE body's length.
: PARITY-TRUNCATED ( -- )
   PATH-HELLO$ GET-READY {: one:CURL:handle :}
   one 3 >LEN FETCH
   one CURL:CLEANUP
   LAST-KIND @ {: kind:n :}
   LAST-LEN @ {: u:n :}
   PATH-HELLO$ GET-READY {: two:CURL:handle :}
   two 3 >LEN MULTI-FETCH
   two CURL:CLEANUP
   s" a short span truncates through the loop as it does through PERFORM" T-LABEL
   kind KIND-TRUNCATED T=
   LAST-KIND @ KIND-TRUNCATED T=
   LAST-LEN @ u T=
   BODY-BUF 3 s" hel" T$= ;


: CASE-HEADER-REUSE ( -- )
   PATH-HEADERS$ GET-READY {: subject:CURL:handle :}
   subject BODY-CAP >LEN FETCH
   subject HEADERS$ HEADER-EXACT
   HEADER-BUF HEADER-SAVED HEADER-LEN @ BYTE-COPY
   subject PATH-HEADERS-B$ URL-FOR CURL:URL! EXPECT-OK
   subject BODY-BUF BODY-CAP >LEN CURL:START EXPECT-OK
   subject HEADER-REFUSED
   subject AWAIT-ONE
   s" AWAIT publishes replacement headers only after collection" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   BODY$ JSON$ T$=
   subject HEADERS-B$ HEADER-EXACT
   subject PATH-HEADERS$ URL-FOR CURL:URL! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject HEADERS$ HEADER-EXACT
   subject CURL:CLEANUP
   HEADER-SAVED HEADER-LEN @ HEADERS$ T$= ;


: CASE-HEADER-INDEPENDENT ( -- )
   PATH-HEADERS$ GET-READY {: one:CURL:handle :}
   PATH-HEADERS-B$ GET-READY {: two:CURL:handle :}
   one BODY-BUF BODY-CAP >LEN CURL:START EXPECT-OK
   two BODY-BUF BODY-CAP >LEN CURL:START EXPECT-OK
   two AWAIT-ONE
   two HEADERS-B$ HEADER-EXACT
   one HEADER-REFUSED
   one AWAIT-ONE
   one HEADERS$ HEADER-EXACT
   two HEADERS-B$ HEADER-EXACT
   one CURL:CLEANUP
   two CURL:CLEANUP ;


: MANY-SPAN ( n -- ptr u8 ) {: idx:n :}
   MANY-BUF idx MANY-CAP * + ;


: MANY-START ( n -- ) {: idx:n :}
   PATH-HELLO$ GET-READY {: subject:CURL:handle :}
   subject idx MANY-HANDLES !
   subject idx MANY-SPAN MANY-CAP >LEN CURL:START EXPECT-OK ;


: MANY-CHECK ( n -- ) {: idx:n :}
   idx MANY-HANDLES @ {: subject:CURL:handle :}
   subject AWAIT-ONE
   subject CURL:CLEANUP
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 200 T=
   idx MANY-SPAN LAST-LEN @ HELLO$ T$= ;


: EXTRA-START ( -- )
   EXTRA-HANDLE @ BODY-BUF BODY-CAP >LEN CURL:START EXPECT-OK ;


\ Thirty-two transfers in flight at once, awaited in order. The two new OS
\ thread identities are the CURL loop and the AIO loop.
: CASE-MANY ( -- )
   MANY-N 0 ?do i MANY-START loop
   TEST-HOST:NEW-THREADS DURING-THREADS !
   PATH-HELLO$ GET-READY EXTRA-HANDLE !
   s" a START with every transfer record claimed is refused" T-LABEL
   [: EXTRA-START ;] CURL:E-CAPACITY TTHROWSQ
   EXTRA-HANDLE @ CURL:CLEANUP
   MANY-N 0 ?do i MANY-CHECK loop
   s" thirty-two transfers cost the CURL loop and the AIO loop alone" T-LABEL
   DURING-THREADS @ 2 T= ;


\ One transfer nothing ever answers, ended by its own ceiling, while the
\ others on the same loop answer 200: a transfer's failure is its own.
: CASE-STALLED ( -- )
   STALL-READY {: bad:CURL:handle :}
   RESULT-RESET
   bad BODY-BUF BODY-CAP >LEN CURL:START EXPECT-OK
   STALL-GROUP 1- 0 ?do i MANY-START loop
   bad AWAIT-ONE
   bad HEADER-REFUSED
   bad CURL:CLEANUP
   s" a stalled transfer ends as CURLE_OPERATION_TIMEDOUT and alone" T-LABEL
   LAST-KIND @ KIND-FAILED T=
   LAST-CODE @ 28 T=
   STALL-GROUP 1- 0 ?do i MANY-CHECK loop ;


\ The server holds this path open and answers nothing, so the transfer is really
\ in flight when it is cancelled - the hit count is what says its request
\ arrived - and nothing but the cancel can end it inside the ten-second ceiling.
: CASE-CANCEL ( -- )
   PATH-HEADERS$ GET-READY {: subject:CURL:handle :}
   subject BODY-CAP >LEN FETCH
   subject HEADERS$ HEADER-EXACT
   subject PATH-STALL$ URL-FOR CURL:URL! EXPECT-OK
   SERVER-HITS atomic@ {: before:n :}
   RESULT-RESET
   subject BODY-BUF BODY-CAP >LEN CURL:START EXPECT-OK
   before WAIT-HIT
   subject CURL:CANCEL
   subject AWAIT-ONE
   subject HEADER-REFUSED
   subject CURL:CLEANUP
   s" a cancelled transfer in flight ends as CURLE_ABORTED_BY_CALLBACK" T-LABEL
   LAST-KIND @ KIND-FAILED T=
   LAST-CODE @ 42 T= ;


: OWNER-WORK ( -- )
   [: OWNER-HANDLE @ CURL:AWAIT DROP-FETCH ;] catch OWNER-RC ! ;


\ A transfer belongs to the task that started it, the loop will not stop while
\ one is in flight, and the record is gone once its answer has been taken.
: CASE-OWNER ( -- )
   0 OWNER-RC !
   PATH-HELLO$ GET-READY OWNER-HANDLE !
   RESULT-RESET
   OWNER-HANDLE @ BODY-BUF BODY-CAP >LEN CURL:START EXPECT-OK
   s" the loop refuses to stop while a transfer is in flight" T-LABEL
   [: CURL:LOOP-STOP ;] CURL:E-STATE TTHROWSQ
   ['] OWNER-WORK OWNER-TASK TASK:ACTIVATE
   OWNER-TASK ENDED? TTRUE
   OWNER-TASK TASK:KILL
   s" a transfer is awaited by the task that started it, and once" T-LABEL
   OWNER-RC @ CURL:E-STATE T=
   OWNER-HANDLE @ AWAIT-ONE
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 200 T=
   [: OWNER-HANDLE @ CURL:AWAIT DROP-FETCH ;] CURL:E-STATE TTHROWSQ
   OWNER-HANDLE @ CURL:CLEANUP ;


: HALT-WORK ( -- )
   HALT-HANDLE @ BODY-BUF BODY-CAP >LEN CURL:START
   MATCH CURL:status
      ok OF ENDOF
      failed OF CURL:CODE>N HALT-RC ! ENDOF
   ;MATCH
   1 HALT-PARKED atomic!
   HALT-HANDLE @ CURL:AWAIT DROP-FETCH ;


\ A task halted while it waits ends at its PAUSE and abandons its record; the
\ loop gives that record back, which is what lets the loop stop afterwards. The
\ halt waits for the request to arrive, so the transfer is really in flight.
: CASE-HALTED ( -- )
   0 HALT-PARKED !
   0 HALT-RC !
   PATH-HEADERS$ GET-READY HALT-HANDLE !
   HALT-HANDLE @ BODY-CAP >LEN FETCH
   HALT-HANDLE @ HEADERS$ HEADER-EXACT
   HALT-HANDLE @ PATH-STALL$ URL-FOR CURL:URL! EXPECT-OK
   SERVER-HITS atomic@ {: before:n :}
   ['] HALT-WORK HALT-TASK TASK:ACTIVATE
   HALT-PARKED 1 REACHED? TTRUE
   before WAIT-HIT
   HALT-TASK TASK:HALT
   HALT-TASK ENDED? TTRUE
   HALT-TASK TASK:KILL
   s" a submitter halted in AWAIT ends and the loop stops after it" T-LABEL
   HALT-RC @ 0 T=
   STOPPED? TTRUE
   HALT-HANDLE @ HEADER-REFUSED
   HALT-HANDLE @ CURL:CLEANUP ;


\ Read the handle's existing libcurl private slot before START transfers it to
\ the loop. Its response-valid cell becomes nonzero only when FINISH publishes
\ the completed peer response; reading it does not call libcurl on a busy handle.
: DONE-STATE ( CURL:handle -- ptr n ) {: subject:CURL:handle :}
   subject CURL:HANDLE>N INFO-PRIVATE DONE-STATE-CELL PRIVATE-GET 0 T=
   DONE-STATE-CELL LE:U64@ STATE>CELL ;


: DONE-PUBLISHED? ( ptr n -- bool ) $18 + atomic@ 0<> ;


: WAIT-PUBLISHED ( ptr n -- ) {: state:ptr :}
   mono-ns WAIT-MS NS-PER-MS * + {: deadline:n :}
   begin
      state DONE-PUBLISHED? if exit then
      mono-ns deadline > if E-PROC-TIMEOUT throw then
   again ;


: DONE-WORK ( -- )
   DONE-HANDLE @ BODY-BUF BODY-CAP >LEN CURL:START
   MATCH CURL:status
      ok OF ENDOF
      failed OF CURL:CODE>N DONE-RC ! ENDOF
   ;MATCH
   1 DONE-PARKED atomic!
   DONE-HANDLE @ CURL:AWAIT DROP-FETCH
   1 DONE-COLLECTED atomic! ;


\ The response cell witnesses FINISH publishing the completed peer response.
\ ABANDON takes the loop's facility after FINISH settles the record, even if
\ HALT arrives during that publication. The waiter must not have collected it.
: CASE-HALTED-DONE ( -- )
   0 DONE-PARKED ! 0 DONE-COLLECTED ! 0 DONE-RC !
   PATH-HEADERS$ GET-READY DONE-HANDLE !
   DONE-HANDLE @ DONE-STATE {: state:ptr :}
   ['] DONE-WORK DONE-TASK TASK:ACTIVATE
   DONE-PARKED 1 REACHED? TTRUE
   state WAIT-PUBLISHED
   DONE-TASK TASK:HALT
   DONE-TASK ENDED? TTRUE
   DONE-TASK TASK:KILL
   s" a completed transfer abandoned by its waiter has no headers" T-LABEL
   DONE-RC @ 0 T=
   DONE-COLLECTED @ 0 T=
   STOPPED? TTRUE
   DONE-HANDLE @ HEADER-REFUSED
   DONE-HANDLE @ CURL:CLEANUP ;


\ The loop opens again on an empty table and carries a transfer as before.
: CASE-RESTART ( -- )
   CURL:LOOP-START EXPECT-OK
   PATH-HELLO$ GET-READY {: subject:CURL:handle :}
   subject BODY-CAP >LEN MULTI-FETCH
   subject CURL:CLEANUP
   s" the loop starts again after a stop and carries a transfer" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 200 T=
   BODY$ HELLO$ T$=
   CURL:LOOP-STOP ;


: START-COLD ( -- )
   COLD-HANDLE @ BODY-BUF BODY-CAP >LEN CURL:START EXPECT-OK ;


: TEST-MULTI ( -- )
   PATH-HELLO$ GET-READY COLD-HANDLE !
   s" a transfer started before the loop is running is refused" T-LABEL
   [: START-COLD ;] CURL:E-STATE TTHROWSQ
   COLD-HANDLE @ CURL:CLEANUP
   CURL:LOOP-START EXPECT-OK
   s" a second LOOP-START is refused" T-LABEL
   [: CURL:LOOP-START EXPECT-OK ;] CURL:E-STATE TTHROWSQ
   PARITY-WHOLE
   PARITY-TRUNCATED
   CASE-HEADER-REUSE
   CASE-HEADER-INDEPENDENT
   CASE-MANY
   CASE-STALLED
   CASE-CANCEL
   CASE-OWNER
   CASE-HALTED
   CURL:LOOP-START EXPECT-OK
   CASE-HALTED-DONE
   CASE-RESTART ;


\ Every request whose arrival the test guarantees arrived exactly once, and the
\ dribble its ceiling ended arrived as DRIBBLE-CEILING found it.
: TEST-SERVER ( -- )
   s" the server task served every request and reported no fault" T-LABEL
   SERVER-BAD @ 0 T=
   SERVER-ERRNO @ 0 T=
   SERVER-HITS atomic@ 69 CEILING-HITS @ + T= ;


\ Opt-in: the one case that leaves the machine. It proves the system CA bundle
\ and TLS work through the same package, which loopback HTTP cannot show.
: NET-TESTS? ( -- bool )
   s" HABU_NET_TESTS" GETENV s" 1" STR= ;


: TEST-HTTPS ( -- )
   OPEN-HANDLE {: subject:CURL:handle :}
   subject s" https://example.com/" CURL:URL! EXPECT-OK
   subject REQUEST-MS >MS CURL:TIMEOUT! EXPECT-OK
   subject true CURL:FOLLOW! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" HTTPS to a public endpoint succeeds over the system CA bundle" T-LABEL
   LAST-KIND @ KIND-RESPONSE T=
   LAST-STATUS @ 200 T= ;


: OPT-IN ( -- )
   NET-TESTS? 0= if exit then
   TEST-HTTPS ;


: PREPARE ( -- )
   MAKE-ROOT
   WRITE-FIXTURES
   DRIBBLE-FILL
   START-SERVER
   OPEN-SILENT ;


: TEARDOWN ( -- )
   STOP-SERVER
   CLOSE-SILENT
   ROOT$ REMOVE-TREE ;

\ No request or live task is needed: each fresh initialization must arm the
\ reset that makes the next captured process initialize libcurl for itself.
: TEST-RECAPTURE ( -- )
   IMAGE-LIFECYCLE:PREPARE
   IMAGE-LIFECYCLE:COUNT {: base:n :}
   2 0 do
      OPEN-HANDLE CURL:CLEANUP
      IMAGE-LIFECYCLE:COUNT base 1+ T=
      IMAGE-LIFECYCLE:PREPARE
      IMAGE-LIFECYCLE:COUNT base T=
   loop ;

public

: RUN ( -- )
   T-RESET
   TEST-RECAPTURE
   PREPARE
   TEST-GET
   TEST-MISSING
   TEST-HEADER
   TEST-POST
   TEST-METHOD
   TEST-COOKIES
   TEST-TRUNCATED
   TEST-RESPONSE-HEADERS
   TEST-HEADER-TRUNCATIONS
   TEST-HEADER-REDIRECT
   TEST-HEADER-INTERIM
   TEST-TIMEOUT
   TEST-LOW-SPEED
   TEST-NO-URL
   TEST-FILE-SCHEME
   TEST-REFUSALS
   TEST-HOST:SNAPSHOT
   AIO:START
   TEST-MULTI
   AIO:STOP
   OPT-IN
   TEARDOWN
   TEST-SERVER
   T-REPORT
   s" curl-test: ok" type cr ;

;package

CURL-TEST:RUN
