\ curl-test.f - focused tests for lib/net/curl.f over loopback HTTP.
\
\ One process holds both peers: a server task binds a TCP4 listener on a free
\ loopback port, publishes the port in a shared cell and accepts one connection
\ at a time, while the main task drives package CURL against it.
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
require lib/task.f
require lib/net/tcp4.f
require lib/net/curl.f

package CURL-TEST

private

CAST: BLEN>N ( NUM:byte-len -- n )

$7F000001 constant LOOPBACK
8 constant BACKLOG
$2000 constant BODY-CAP
$1000 constant REQ-CAP
$400 constant RES-CAP
$20 constant NUM-CAP
8 constant DRAIN-TRIES                  \ reads that finish a connection the peer is closing
$1000 constant READY-TRIES              \ TASK:PAUSE turns before the listener is late
500 constant STALL-MS                   \ well under the stall path's forever
$2710 constant REQUEST-MS               \ 10 s: a loopback request that slow is broken
$FE constant FILL-BYTE                  \ the sentinel a refused transfer must not overwrite

create ROOT-BUF FS-PATH-CAP allot
create FILE-BUF FS-PATH-CAP allot
create JAR-BUF FS-PATH-CAP allot
create BODY-BUF BODY-CAP allot
create REQ-BUF REQ-CAP allot
create RES-BUF RES-CAP allot
create JAR-TEXT-BUF RES-CAP allot
create NUM-BUF NUM-CAP allot

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
variable LAST-STATUS
variable LAST-LEN
variable LAST-CODE
variable LAST-KIND                      \ 0 response, 1 truncated, 2 failed

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
\ client itself gives up, which is the only thing TIMEOUT! can end.
: SERVE-STALL ( TCP4:connection -- ) {: conn:TCP4:connection :}
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


: SERVE-UNIMPLEMENTED ( TCP4:connection -- ) {: conn:TCP4:connection :}
   s" 501 Not Implemented" s" not implemented" RESPOND
   conn SEND-RESPONSE ;


: ROUTE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   s" POST" METHOD-IS? if conn SERVE-UNIMPLEMENTED exit then
   s" DELETE" METHOD-IS? if conn SERVE-UNIMPLEMENTED exit then
   PATH-STALL$ PATH-IS? if conn SERVE-STALL exit then
   PATH-HELLO$ PATH-IS? if conn SERVE-HELLO exit then
   PATH-COOKIE$ PATH-IS? if conn SERVE-COOKIE exit then
   conn SERVE-MISSING ;


: SERVE-ONE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn REQ-READ 0= if conn TCP4:CLOSE PEER-STATUS exit then
   1 SERVER-HITS +!
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


: BIND-SERVER ( -- bool )
   LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF TCP4:LISTENER>N SERVER-LISTENER ! true ENDOF
      failed OF SERVER-FAILED false ENDOF
   ;MATCH ;


: PUBLISH-PORT ( -- bool )
   LISTENER@ TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N SERVER-PORT ! TCP4:ADDRESS>N drop true ENDOF
      failed OF SERVER-FAILED false ENDOF
   ;MATCH ;


: SERVER-START ( -- bool )
   BIND-SERVER 0= if false exit then
   LISTENER@ BACKLOG TCP4:LISTEN SERVER-STATUS
   SERVER-BAD @ 0 <> if false exit then
   PUBLISH-PORT ;


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


\ ---- fixtures ----------------------------------------------------------------

: UNDER-ROOT ( ptr u8 n ptr u8 -- n ) {: name:ptr nameu:n dst:ptr :}
   ROOT$ name nameu dst JOIN-PATH ;


: MAKE-ROOT ( -- )
   s" habu-curl" TMPDIR-MKDIR {: a:ptr u:n :}
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


: URL-FOR ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   SB-RESET
   s" http://127.0.0.1:" SB-APPEND
   SERVER-PORT @ FMT:SB-U
   path pathu SB-APPEND
   SB$ ;


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


: TEST-TIMEOUT ( -- )
   PATH-STALL$ GET-READY {: subject:CURL:handle :}
   subject STALL-MS >MS CURL:TIMEOUT! EXPECT-OK
   subject BODY-CAP >LEN FETCH
   subject CURL:CLEANUP
   s" a server that never answers ends as CURLE_OPERATION_TIMEDOUT" T-LABEL
   LAST-KIND @ KIND-FAILED T=
   LAST-CODE @ 28 T= ;


: TEST-NO-URL ( -- )
   OPEN-HANDLE {: subject:CURL:handle :}
   subject BODY-CAP >LEN FETCH
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


: TEST-REFUSALS ( -- )
   s" a handle that was never opened is refused" T-LABEL
   [: NULL-HANDLE ;] CURL:E-STATE TTHROWSQ
   s" a NUL inside a URL is refused, never silently cut" T-LABEL
   [: EMBEDDED-NUL ;] CURL:E-OPERAND TTHROWSQ ;


: TEST-SERVER ( -- )
   s" the server task served every request and reported no fault" T-LABEL
   SERVER-BAD @ 0 T=
   SERVER-ERRNO @ 0 T=
   SERVER-HITS @ 9 T= ;


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
   START-SERVER ;


: TEARDOWN ( -- )
   STOP-SERVER
   ROOT$ REMOVE-TREE ;

public

: RUN ( -- )
   T-RESET
   PREPARE
   TEST-GET
   TEST-MISSING
   TEST-HEADER
   TEST-POST
   TEST-METHOD
   TEST-COOKIES
   TEST-TRUNCATED
   TEST-TIMEOUT
   TEST-NO-URL
   TEST-FILE-SCHEME
   TEST-REFUSALS
   OPT-IN
   TEARDOWN
   TEST-SERVER
   T-REPORT
   s" curl-test: ok" type cr ;

;package

CURL-TEST:RUN
