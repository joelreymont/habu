split-str
\ httpc-test.f - focused tests for src/lib/net/httpc.f over loopback HTTP/1.1.
\ One process holds both peers: a server task accepts on a loopback TCP4
\ listener bound to port 0 and answers each request with the bytes its case
\ scripts, counting accepts, requests per connection and request bytes and
\ keeping the last request head, while the main task drives HTTPC. Every
\ exchange has deadlines, so a server that fails to answer ends its case
\ `timeout` and not in a hang. A scripted close is a half-close: the server
\ sends its FIN and still counts any request that arrives after it.
\ The https cases speak to TEST-TLS-PEER's `openssl s_server` and its
\ certificates. An https origin at the plain server, which answers nothing,
\ fails its handshake `timeout` at the stall deadline. With -www, a trusted
\ GET / reads its answer to its close_notify, and an unrelated CA or an
\ origin naming another host fails the greeting with `trust`. Without it,
\ the server sends in one record what the test writes to its stdin: an
\ answer whose record carries bytes past its body leaves them in the TLS
\ session, and the next request dials anew. A relay task between HTTPC and
\ an s_server restarted on TLS 1.2 closes HTTPC's socket in place of the
\ server's close_notify, so an answer read to the close fails `cut`.
\ TEST-TABLE compares TEST-FSM:TABLE-MODULE's text for the LIFECYCLE table
\ with spec/HttpcTable.tla whole, so the table spec/HttpcFollows.tla ties to
\ the model spec/Httpc.tla is the code's.
\ Run: bin/hb --load src/lib/net/httpc-test.f </dev/null
HB-TARGET-LINUX-KERNEL? ?TARGET
require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/test.f
require src/lib/test/wait.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/net/tcp4.f
require src/lib/net/httpc.f
require src/lib/arena.f
require src/lib/test-fsm.f
require src/lib/net/test-tls-peer.f

\ White-box: the conn's socket, so a case waits for the server's FIN to reach
\ it before it sends again.
package HTTPC
: HTTPC-TEST:FD ( ^conn -- n ) CONN-FD @ ;
;package

package HTTPC-TEST

private

CAST: BLEN>N ( NUM:byte-len -- n )

$1000 constant REQ-CAP
$10000 constant ROOM-CAP
64 constant SMALL-ROOM                  \ holds a request head, not BIG-HEAD$
$4000 constant GOT-CAP                  \ s_server's status page fits
$40 constant ORIGIN-CAP
5000 constant TOTAL-MS
5000 constant STALL-MS
5000 constant FIN-MS
100 constant SHORT-STALL-MS             \ the stall the timeout case expects
8 constant CONN-CAP                     \ connections one case may make
10 constant SIZED-SPAN                  \ shorter than the sized body
4 constant CHUNKED-SPAN                 \ crosses every chunk boundary
$40 constant WIDE-SPAN
$01 constant POLLIN

create REQ-BUF REQ-CAP allot
create SEEN-BUF REQ-CAP allot
create ROOM-BUF ROOM-CAP allot
create SPAN-BUF WIDE-SPAN allot
create GOT-BUF GOT-CAP allot
create ORIGIN-BUF ORIGIN-CAP allot
create FIN-PFD 8 allot

variable LISTENER-CELL
variable PORT-CELL
variable READY
variable STOP
variable DONE
variable ACCEPTS
variable REQUESTS
variable REQ-BYTES
variable REQ-U
variable SEEN-U                         \ the last request head answered
variable ORIGIN-U
variable GOT-U
variable WIDEST                         \ the largest piece one NEXT answered
variable LAST-STATUS
variable FAULTS
TYPED-VARIABLE LAST-FAULT HTTPC:fault
CONN-CAP TYPED-BUFFER PER-CONN n

\ What the server answers in the running case.
ENUM script sized chunked closing interim no-content reuse idle-close
   unread cut-head cut-body silent bad-status bad-length bad-chunk big-head
   trailing ;ENUM
TYPED-VARIABLE SCRIPT script
TYPED-VARIABLE LINK HTTPC:conn
TYPED-VARIABLE SPARE HTTPC:conn          \ a record OPEN finds full of bytes

CAST: CONN>BYTES ( ^HTTPC:conn -- ^u8 )

TASK:MIN-STACK TASK:TASK SERVER-TASK


: ALPHABET$ ( -- str ) s" abcdefghijklmnopqrstuvwxyz" ;
: SIZED$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 26\r\n\r\nabcdefghijklmnopqrstuvwxyz" ;
: SIZED-HEAD$ ( -- str ) S\" HTTP/1.1 200 OK\r\nContent-Length: 26\r\n\r\n" ;
: CHUNKED$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n7;x=1\r\n, world\r\n0\r\nX-Trailer: 1\r\n\r\n" ;
: CLOSING$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nuntil the close" ;
: INTERIM$ ( -- str )
   S\" HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" ;
: FINAL-HEAD$ ( -- str ) S\" HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n" ;
: NO-CONTENT$ ( -- str ) S\" HTTP/1.1 204 No Content\r\n\r\n" ;
: FIRST$ ( -- str ) S\" HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nfirst" ;
: SECOND$ ( -- str ) S\" HTTP/1.1 200 OK\r\nContent-Length: 6\r\n\r\nsecond" ;

: CUT-BODY$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 26\r\n\r\nabc" ;
: BAD-STATUS$ ( -- str ) S\" HTTP/1.1 2x0 OK\r\n\r\n" ;
: BAD-LENGTH$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: abc\r\n\r\nok" ;
: BAD-CHUNK$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\nok\r\n0\r\n\r\n" ;
: BIG-HEAD$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nX-Pad: abcdefghijklmnopqrstuvwxyz\r\nContent-Length: 0\r\n\r\n" ;
\ FIRST$ and two bytes past its body, sent in one write.
: TRAILING$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nfirstXX" ;

: TWO$ ( n -- str ) 0= if FIRST$ else SECOND$ then ;

\ The answer to the case's request K, and whether the server closes after it.
: REPLY ( n -- ^u8 n bool ) {: k:n :}
   SCRIPT @ MATCH script
      sized OF SIZED$ false ENDOF
      chunked OF CHUNKED$ false ENDOF
      closing OF CLOSING$ true ENDOF
      interim OF INTERIM$ false ENDOF
      no-content OF NO-CONTENT$ false ENDOF
      reuse OF k TWO$ false ENDOF
      idle-close OF k TWO$ k 0= ENDOF
      unread OF k TWO$ false ENDOF
      cut-head OF s" " true ENDOF
      cut-body OF CUT-BODY$ true ENDOF
      silent OF s" " false ENDOF
      bad-status OF BAD-STATUS$ false ENDOF
      bad-length OF BAD-LENGTH$ false ENDOF
      bad-chunk OF BAD-CHUNK$ false ENDOF
      big-head OF BIG-HEAD$ false ENDOF
      trailing OF k 0= if TRAILING$ else SECOND$ then false ENDOF
   ;MATCH ;


\ ---- The server task

\ A peer that has gone answers its own errno; that is its departure.
: PEER-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;

: REQ-CHUNK ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   REQ-U @ REQ-CAP >= if false exit then
   conn REQ-BUF REQ-U @ + REQ-CAP REQ-U @ - TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N REQ-U +! true ENDOF
      closed OF drop false ENDOF
      failed OF drop false ENDOF
   ;MATCH ;

\ The request head's length through its blank line; 0 while incomplete.
: HEAD-U ( -- n )
   REQ-BUF REQ-U @ S\" \r\n\r\n" FIND-SUB MATCH option
      none OF 0 ENDOF
      some OF IDX>N 4 + ENDOF
   ;MATCH ;

\ The body length a request head of H bytes declares; 0 for none.
: BODY-U ( n -- n ) {: h:n :}
   REQ-BUF h s" Content-Length: " FIND-SUB MATCH option
      none OF 0 ENDOF
      some OF IDX>N 16 + {: at:n :}
         REQ-BUF at + h at - 13 INDEX-OF MATCH option
            none OF 0 ENDOF
            some OF IDX>N REQ-BUF at + swap STR-PARSE-POS
               MATCH option none OF 0 ENDOF some OF ENDOF ;MATCH
            ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

\ One whole request, its bytes counted; false at the client's close.
: READ-REQUEST ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   0 REQ-U !
   begin HEAD-U dup 0= while
      drop conn REQ-CHUNK 0= if false exit then
   repeat
   dup BODY-U + {: whole:n :}
   begin REQ-U @ whole < while
      conn REQ-CHUNK 0= if false exit then
   repeat
   REQ-U @ REQ-BYTES atomic-add drop
   REQ-BUF SEEN-BUF HEAD-U BYTE-COPY  HEAD-U SEEN-U !
   true ;

\ Answers each request until a scripted close; true after that close.
: ANSWER-ALL ( TCP4:connection n -- bool ) {: conn:TCP4:connection idx:n :}
   begin conn READ-REQUEST while
      1 idx PER-CONN atomic-add drop
      1 REQUESTS atomic-add REPLY {: a:^ u:n shut:bool :}
      conn a u TCP4:TRANSFER-BYTES TCP4:WRITE PEER-STATUS
      shut if conn TCP4:SENDING TCP4:SHUTDOWN PEER-STATUS true exit then
   repeat false ;

\ After its FIN the server still counts every request that arrives.
: COUNT-LATE ( TCP4:connection n -- ) {: conn:TCP4:connection idx:n :}
   begin conn READ-REQUEST while
      1 idx PER-CONN atomic-add drop
   repeat ;

: SERVE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   1 ACCEPTS atomic-add CONN-CAP 1- min {: idx:n :}
   conn idx ANSWER-ALL if conn idx COUNT-LATE then
   conn TCP4:CLOSE PEER-STATUS ;

: LISTENER@ ( -- TCP4:listener ) LISTENER-CELL @ TCP4:>LISTENER ;

: SERVE-LOOP ( -- )
   begin
      STOP atomic@ 0 <> if exit then
      LISTENER@ TCP4:ACCEPT
      MATCH TCP4:accept-result
         accepted OF 2drop
            STOP atomic@ 0 <> if TCP4:CLOSE PEER-STATUS exit then
            SERVE
         ENDOF
         failed OF drop exit ENDOF
      ;MATCH
   again ;

\ The port is published before readiness is.
: SERVER-WORK ( -- )
   LISTENER-CELL PORT-CELL TEST-TLS-PEER:LISTEN
   0= if 1 STOP atomic-add drop then
   1 READY atomic-add drop
   SERVE-LOOP
   1 DONE atomic-add drop ;

: START-SERVER ( -- )
   ['] SERVER-WORK SERVER-TASK TASK:ACTIVATE
   READY 1 SERVER-TASK TEST-WAIT:FOR-CELL ;

: WAKE-SERVER ( -- )
   TEST-TLS-PEER:LOOPBACK TCP4:ADDRESS PORT-CELL @ TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF TCP4:CLOSE PEER-STATUS ENDOF
      failed OF drop ENDOF
   ;MATCH ;

: END-SERVER ( -- )
   1 STOP atomic-add drop
   WAKE-SERVER
   DONE 1 SERVER-TASK TEST-WAIT:FOR-CELL
   SERVER-TASK TASK:KILL
   LISTENER@ TCP4:CLOSE-LISTENER PEER-STATUS ;


\ ---- The client

: ORIGIN$ ( -- str ) ORIGIN-BUF ORIGIN-U @ ;

: ORIGIN! ( n -- ) {: port:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND port FMT:SB-U
   SB$ {: a:^ u:n :}
   a ORIGIN-BUF u BYTE-COPY  u ORIGIN-U ! ;

\ The server's script for a fresh case, its counts zeroed.
: SCRIPT-IS ( script -- )
   SCRIPT !
   0 ACCEPTS ! 0 REQUESTS ! 0 REQ-BYTES ! 0 SEEN-U !
   CONN-CAP 0 do 0 i PER-CONN ! loop ;

\ A fresh case: the script, zeroed counts and a conn holding nothing, with a
\ room of ROOM bytes.
: BEGIN-IN ( script n -- ) {: room:n :}
   SCRIPT-IS
   LINK ORIGIN$ s" " ROOM-BUF room HTTPC:OPEN
   s" OPEN takes the loopback origin" T-LABEL TTRUE ;

: BEGIN-CASE ( script -- ) ROOM-CAP BEGIN-IN ;

: END-CASE ( -- ) LINK HTTPC:CLOSE ;

: PREPARE ( ^u8 n ^u8 n -- ) {: m:^ mu:n t:^ tu:n :}
   LINK m mu t tu HTTPC:REQUEST
   LINK TOTAL-MS STALL-MS HTTPC:DEADLINES! ;

: ASK ( -- ) s" GET" s" /case" PREPARE  LINK s" " HTTPC:SEND ;

: GOT$ ( -- str ) GOT-BUF GOT-U @ ;

: GOT+ ( n -- ) {: k:n :}
   GOT-U @ k + GOT-CAP > if
      s" the answer outgrows GOT-BUF" T-FAIL-AS exit
   then
   SPAN-BUF GOT-BUF GOT-U @ + k BYTE-COPY
   k GOT-U +!  WIDEST @ k max WIDEST ! ;

: FAILED ( HTTPC:fault -- ) LAST-FAULT !  1 FAULTS +! ;

: ANSWERED? ( HTTPC:chunk-result -- bool )
   MATCH HTTPC:chunk-result
      chunk OF LEN>N GOT+ LAST-STATUS ! false ENDOF
      response OF LAST-STATUS ! true ENDOF
      failed OF FAILED true ENDOF
   ;MATCH ;

: ANSWER-RESET ( -- ) 0 GOT-U ! 0 WIDEST ! -1 LAST-STATUS ! 0 FAULTS ! ;

\ Reads the answer to its end in NEXT spans of SPAN bytes.
: READ-ALL ( n -- ) {: span:n :}
   ANSWER-RESET
   begin LINK SPAN-BUF span >LEN HTTPC:NEXT ANSWERED? until ;

\ One NEXT of SPAN bytes, which answers a piece of the body.
: ONE-PIECE ( n -- ) {: span:n :}
   ANSWER-RESET
   LINK SPAN-BUF span >LEN HTTPC:NEXT ANSWERED? TFALSE ;

: ANSWER-IS ( n ^u8 n -- ) {: status:n a:^ u:n :}
   FAULTS @ 0<> if
      s" httpc-test: NEXT failed: " type LAST-FAULT @ HTTPC:FAULT$ type cr
   then
   FAULTS @ 0 T=  LAST-STATUS @ status T=  GOT$ a u T$= ;

\ The answer ended in the fault, which LOST? answers LOST for. LAST-FAULT
\ is the answer's own only when one fault was recorded.
: FAULT-IS ( HTTPC:fault bool -- ) {: f:HTTPC:fault lost:bool :}
   FAULTS @ 1 T=
   FAULTS @ 1 = if
      LAST-FAULT @ HTTPC:FAULT$ f HTTPC:FAULT$ T$=
      LAST-FAULT @ HTTPC:LOST? lost if TTRUE else TFALSE then
   then ;

\ Whether the last request head the server answered names a Content-Length.
: SEEN-LENGTH? ( -- bool )
   SEEN-BUF SEEN-U @ s" Content-Length" FIND-SUB MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ A NEXT after CLOSE fails `send` and leaves no socket held.
: CLOSED-NEXT ( -- )
   LINK HTTPC:CLOSE  ANSWER-RESET
   LINK SPAN-BUF WIDE-SPAN >LEN HTTPC:NEXT ANSWERED? TTRUE
   HTTPC-FAULT:SEND true FAULT-IS
   LINK HTTPC-TEST:FD -1 T= ;

\ Blocks until the server's FIN reaches the conn's socket.
: AWAIT-FIN ( -- bool )
   LINK HTTPC-TEST:FD FIN-PFD LE:U32!
   POLLIN FIN-PFD 4 + LE:U16!  0 FIN-PFD 6 + LE:U16!
   FIN-PFD 1 FIN-MS poll 1 = ;


\ ---- Cases

: TEST-SIZED ( -- )
   construct script sized BEGIN-CASE
   ASK SIZED-SPAN READ-ALL
   s" a Content-Length body arrives whole across short spans" T-LABEL
   200 ALPHABET$ ANSWER-IS
   WIDEST @ SIZED-SPAN <= TTRUE
   s" HEAD$ holds the answer's head" T-LABEL
   LINK HTTPC:HEAD$ SIZED-HEAD$ T$=
   END-CASE ;

: TEST-CHUNKED ( -- )
   construct script chunked BEGIN-CASE
   ASK CHUNKED-SPAN READ-ALL
   s" a chunked body decodes across unaligned spans" T-LABEL
   200 s" hello, world" ANSWER-IS
   END-CASE ;

: TEST-CLOSING ( -- )
   construct script closing BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" a close-delimited body ends at the close" T-LABEL
   200 s" until the close" ANSWER-IS
   ASK WIDE-SPAN READ-ALL
   s" the request after a close-delimited body dials again" T-LABEL
   200 s" until the close" ANSWER-IS
   ACCEPTS @ 2 T=
   END-CASE ;

: TEST-INTERIM ( -- )
   construct script interim BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" a 1xx head is discarded for the answer after it" T-LABEL
   200 s" ok" ANSWER-IS
   LINK HTTPC:HEAD$ FINAL-HEAD$ T$=
   END-CASE ;

\ The server keeps the connection open, so waiting for a close would end in
\ `timeout`.
: TEST-NO-CONTENT ( -- )
   construct script no-content BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" a 204 ends at its head" T-LABEL
   204 s" " ANSWER-IS
   ASK WIDE-SPAN READ-ALL
   s" the request after a 204 reuses the connection" T-LABEL
   204 s" " ANSWER-IS
   ACCEPTS @ 1 T=
   END-CASE ;

: TEST-REUSE ( -- )
   construct script reuse BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" the first of two requests" T-LABEL
   200 s" first" ANSWER-IS
   ASK WIDE-SPAN READ-ALL
   s" the second request reuses the connection" T-LABEL
   200 s" second" ANSWER-IS
   ACCEPTS @ 1 T=
   END-CASE ;

: TEST-IDLE-CLOSE ( -- )
   construct script idle-close BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" the answer before the server closes the kept connection" T-LABEL
   200 s" first" ANSWER-IS
   REQ-BYTES @ {: one:n :}
   s" the server's FIN reaches the idle connection" T-LABEL
   AWAIT-FIN TTRUE
   ASK WIDE-SPAN READ-ALL
   s" the next request dials anew and is written once" T-LABEL
   200 s" second" ANSWER-IS
   ACCEPTS @ 2 T=
   0 PER-CONN @ 1 T=
   1 PER-CONN @ 1 T=
   REQ-BYTES @ one 2 * T=
   END-CASE ;

: TEST-UNREAD ( -- )
   construct script unread BEGIN-CASE
   ASK ASK WIDE-SPAN READ-ALL
   s" a SEND over an unread answer drops that connection" T-LABEL
   200 s" second" ANSWER-IS
   ACCEPTS @ 2 T=
   0 PER-CONN @ 1 T=
   1 PER-CONN @ 1 T=
   END-CASE ;

: TEST-TRAILING ( -- )
   construct script trailing BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" an answer with bytes past its body" T-LABEL
   200 s" first" ANSWER-IS
   ASK WIDE-SPAN READ-ALL
   s" the request after bytes past an answer dials anew" T-LABEL
   200 s" second" ANSWER-IS
   ACCEPTS @ 2 T=
   0 PER-CONN @ 1 T=
   END-CASE ;

: TEST-LENGTH-SENT ( -- )
   construct script reuse BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" a GET with no body sends no Content-Length" T-LABEL
   200 s" first" ANSWER-IS
   SEEN-LENGTH? TFALSE
   s" POST" s" /case" PREPARE  LINK s" " HTTPC:SEND  WIDE-SPAN READ-ALL
   s" an empty POST sends Content-Length: 0" T-LABEL
   200 s" second" ANSWER-IS
   SEEN-BUF SEEN-U @ S\" \r\nContent-Length: 0\r\n" FIND-SUB MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH TTRUE
   END-CASE ;

\ A loopback port bound but not listening refuses every connection.
: TEST-CONNECT ( -- )
   TEST-TLS-PEER:LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF ENDOF
      failed OF drop s" a loopback port binds" T-LABEL false TTRUE exit ENDOF
   ;MATCH {: shut:TCP4:listener :}
   shut TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N ORIGIN! drop ENDOF
      failed OF drop 0 ORIGIN! ENDOF
   ;MATCH
   construct script sized BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" a port nothing listens on fails `connect`" T-LABEL
   HTTPC-FAULT:CONNECT true FAULT-IS
   END-CASE
   shut TCP4:CLOSE-LISTENER PEER-STATUS
   PORT-CELL @ ORIGIN! ;

: TEST-CUT ( -- )
   construct script cut-head BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" a close before the head fails `cut`" T-LABEL
   HTTPC-FAULT:CUT true FAULT-IS
   END-CASE
   construct script cut-body BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" a close inside a Content-Length body fails `cut`" T-LABEL
   HTTPC-FAULT:CUT true FAULT-IS
   GOT$ s" abc" T$=
   END-CASE ;

: TEST-TIMEOUT ( -- )
   construct script silent BEGIN-CASE
   LINK s" GET" s" /case" HTTPC:REQUEST
   LINK TOTAL-MS SHORT-STALL-MS HTTPC:DEADLINES!
   LINK s" " HTTPC:SEND  WIDE-SPAN READ-ALL
   s" a server that answers nothing fails `timeout` at the stall" T-LABEL
   HTTPC-FAULT:TIMEOUT true FAULT-IS
   END-CASE ;

: MALFORMED-BY ( script ^u8 n -- ) {: sc:script a:^ u:n :}
   sc BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   a u T-LABEL
   HTTPC-FAULT:MALFORMED false FAULT-IS
   END-CASE ;

: TEST-MALFORMED ( -- )
   construct script bad-status s" a bad status line fails `malformed`"
   MALFORMED-BY
   construct script bad-length
   s" a non-numeric Content-Length fails `malformed`" MALFORMED-BY
   construct script bad-chunk s" a bad chunk size fails `malformed`"
   MALFORMED-BY ;

: TEST-OVERSIZE ( -- )
   construct script big-head SMALL-ROOM BEGIN-IN
   ASK WIDE-SPAN READ-ALL
   s" an answer head larger than the room fails `oversize`" T-LABEL
   HTTPC-FAULT:OVERSIZE false FAULT-IS
   END-CASE ;

\ A marked request's first NEXT answers its fault.
: REFUSED ( HTTPC:fault -- ) {: f:HTTPC:fault :}
   LINK s" " HTTPC:SEND  WIDE-SPAN READ-ALL
   f false FAULT-IS ;

\ The good request after the refused ones accepts every connection the
\ refused ones would have made, so ACCEPTS counts them.
: TEST-REFUSED ( -- )
   construct script reuse SMALL-ROOM BEGIN-IN
   s" GET" s" /case" PREPARE  LINK ALPHABET$ HTTPC:HEADER+
   s" a request head larger than the room fails `oversize`" T-LABEL
   HTTPC-FAULT:OVERSIZE REFUSED
   s" GET" s" /case" PREPARE  LINK S\" X-Bad: a\rb" HTTPC:HEADER+
   s" a CR in a header line fails `malformed`" T-LABEL
   HTTPC-FAULT:MALFORMED REFUSED
   s" GET" s" /a b" PREPARE
   s" a SP in the target fails `malformed`" T-LABEL
   HTTPC-FAULT:MALFORMED REFUSED
   ASK WIDE-SPAN READ-ALL
   s" refused requests dial nothing" T-LABEL
   200 s" first" ANSWER-IS
   ACCEPTS @ 1 T=
   s" GET" s" /a b" PREPARE
   s" a refused request over a held connection" T-LABEL
   HTTPC-FAULT:MALFORMED REFUSED
   ASK WIDE-SPAN READ-ALL
   s" a refused request drops the held connection" T-LABEL
   200 s" second" ANSWER-IS
   ACCEPTS @ 2 T=
   0 PER-CONN @ 1 T=
   1 PER-CONN @ 1 T=
   END-CASE ;


\ The body is longer than the span, so CLOSE comes partway through it.
: TEST-CLOSE ( -- )
   construct script sized BEGIN-CASE
   ASK
   s" the first NEXT of a sized answer answers a piece" T-LABEL
   SIZED-SPAN ONE-PIECE
   s" a NEXT after CLOSE partway fails `send`, holding no socket" T-LABEL
   CLOSED-NEXT
   END-CASE ;


\ ---- An https origin

$200 constant TRUST-ROOM
TRUST-ROOM BUFFER: TRUST-BUF            \ the trust file's path OPEN borrows
variable TRUST-U

\ The origin `https://` HOST `:` PORT.
: HTTPS-ORIGIN! ( str n -- ) {: h:^ hu:n port:n :}
   SB-RESET s" https://" SB-APPEND h hu SB-APPEND s" :" SB-APPEND
   port FMT:SB-U SB$ {: a:^ u:n :}
   a ORIGIN-BUF u BYTE-COPY  u ORIGIN-U ! ;

: TRUST! ( str -- )
   TEST-TLS-PEER:IN-DIR {: a:^ u:n :}
   a TRUST-BUF u BYTE-COPY  u TRUST-U ! ;

\ A conn opened on the https origin for HOST at PORT, trusting the scratch
\ file, with a room of ROOM bytes.
: BEGIN-HTTPS-AT ( str str n n -- ) {: h:^ hu:n f:^ fu:n port:n room:n :}
   f fu TRUST!  h hu port HTTPS-ORIGIN!
   LINK ORIGIN$ TRUST-BUF TRUST-U @ ROOM-BUF room HTTPC:OPEN
   s" OPEN takes the https origin" T-LABEL TTRUE ;

: BEGIN-HTTPS ( str str -- )
   TEST-TLS-PEER:PORT ROOM-CAP BEGIN-HTTPS-AT ;

: ASK-ROOT ( -- )
   LINK s" GET" s" /" HTTPC:REQUEST
   LINK TOTAL-MS STALL-MS HTTPC:DEADLINES!
   LINK s" " HTTPC:SEND ;

: TRUST-FAULT ( HTTPC:fault -- ) {: f:HTTPC:fault :}
   f HTTPC:FAULT$ HTTPC-FAULT:TRUST HTTPC:FAULT$ T$=
   f HTTPC:LOST? TFALSE ;

\ The first NEXT fails with `trust`, its CAUSE carrying the verify flag.
: DISTRUSTED ( n -- ) {: flag:n :}
   ASK-ROOT
   LINK SPAN-BUF WIDE-SPAN >LEN HTTPC:NEXT MATCH HTTPC:chunk-result
      chunk OF 2drop s" an answer arrived" T-FAIL-AS ENDOF
      response OF drop s" an answer arrived" T-FAIL-AS ENDOF
      failed OF TRUST-FAULT ENDOF
   ;MATCH
   LINK HTTPC:CAUSE flag and flag T= ;

: TEST-HTTPS-TRUSTED ( -- )
   s" localhost" s" ca.pem" BEGIN-HTTPS
   ASK-ROOT WIDE-SPAN READ-ALL
   s" a trusted https GET / answers 200 and its body to close_notify" T-LABEL
   FAULTS @ 0 T=  LAST-STATUS @ 200 T=
   GOT$ s" <HTML>" STARTS-WITH? TTRUE
   LINK HTTPC:HEAD$ s" HTTP/1.0 200" STARTS-WITH? TTRUE
   END-CASE ;

: TEST-HTTPS-UNRELATED ( -- )
   s" localhost" s" other.pem" BEGIN-HTTPS
   s" a trust file of an unrelated CA fails with trust, not lost" T-LABEL
   TEST-TLS-PEER:NOT-TRUSTED DISTRUSTED
   END-CASE ;

: TEST-HTTPS-OTHER-HOST ( -- )
   s" 127.0.0.1" s" ca.pem" BEGIN-HTTPS
   s" an origin naming another host fails with trust" T-LABEL
   TEST-TLS-PEER:CN-MISMATCH DISTRUSTED
   END-CASE ;

\ The plain server answers nothing to the ClientHello, so the handshake waits
\ until the stall deadline.
: TEST-HTTPS-SILENT ( -- )
   construct script silent SCRIPT-IS
   s" localhost" s" ca.pem" PORT-CELL @ ROOM-CAP BEGIN-HTTPS-AT
   LINK s" GET" s" /" HTTPC:REQUEST
   LINK TOTAL-MS SHORT-STALL-MS HTTPC:DEADLINES!
   LINK s" " HTTPC:SEND  WIDE-SPAN READ-ALL
   s" a handshake the peer never answers fails `timeout` at the stall"
   T-LABEL
   HTTPC-FAULT:TIMEOUT true FAULT-IS
   END-CASE
   s" the plain server took the connection" T-LABEL
   ACCEPTS 1 SERVER-TASK TEST-WAIT:FOR-CELL ;

\ An answer head padded to hold the request head, which shares the room.
: HELD-HEAD$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 5\r\nX-Pad: abcdefghijklmnopq\r\n\r\n" ;
: STALE$ ( -- str ) S\" HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nstale" ;

\ The head fills a room of its own size, so the head's read ends at it and the
\ body's at the body: STALE$, in the same record, stays in the TLS session. A
\ reused connection would answer the second request with it.
: TEST-HTTPS-HELD ( -- )
   s" localhost" s" ca.pem" TEST-TLS-PEER:PORT HELD-HEAD$ nip
   BEGIN-HTTPS-AT
   ASK-ROOT
   SB-RESET HELD-HEAD$ SB-APPEND s" first" SB-APPEND STALE$ SB-APPEND
   SB$ TEST-TLS-PEER:SCRIPTED
   WIDE-SPAN READ-ALL
   s" an https answer whose record holds bytes past its body" T-LABEL
   200 s" first" ANSWER-IS
   ASK-ROOT  SECOND$ TEST-TLS-PEER:SCRIPTED  WIDE-SPAN READ-ALL
   s" the request after bytes the TLS session holds dials anew" T-LABEL
   200 s" second" ANSWER-IS
   END-CASE ;

\ The first NEXT reads the 5-byte body whole, so a NEXT after CLOSE would
\ settle the answer over the session CLOSE stopped.
: TEST-HTTPS-CLOSE ( -- )
   s" localhost" s" ca.pem" BEGIN-HTTPS
   ASK-ROOT  FIRST$ TEST-TLS-PEER:SCRIPTED
   s" an https NEXT answers the 5-byte body" T-LABEL
   WIDE-SPAN ONE-PIECE  GOT$ s" first" T$=
   s" an https NEXT after CLOSE fails `send`, holding no socket" T-LABEL
   CLOSED-NEXT
   END-CASE ;

\ ---- A relay that withholds the close_notify

\ The relay task accepts HTTPC's connection and dials s_server. It forwards
\ the client's bytes as they come and the server's by whole records, each
\ framed by its 5-byte header. At the server's first alert record, the
\ close_notify, it closes the client's socket with that record unsent.
5 constant RECORD-HEAD
21 constant ALERT-TYPE                  \ a TLS 1.2 record's content type
$10000 constant RECORDS-CAP             \ a part record, 5 + 2^14 + 2048
                                        \ bytes at most, and a read
create RECORDS RECORDS-CAP allot        \ the server's bytes not forwarded
variable RECORDS-U
create UPWARD REQ-CAP allot             \ the client's bytes on their way
variable WITHHELD                       \ the client was closed at an alert
variable RELAY-LISTENER
variable RELAY-PORT
variable RELAY-DONE
TASK:MIN-STACK TASK:TASK RELAY-TASK

\ The length of the whole record at AT, header included; 0 while it is not
\ whole yet.
: RECORD-LEN ( n -- n ) {: at:n :}
   at RECORD-HEAD + RECORDS-U @ > if 0 exit then
   RECORDS at + 3 + c@ 8 lshift  RECORDS at + 4 + c@ or RECORD-HEAD +
   {: len:n :}
   at len + RECORDS-U @ > if 0 exit then
   len ;

\ Where the whole records before any alert end, and whether an alert
\ record follows them.
: WHOLE ( -- n bool )
   0 begin {: at:n :}
      at RECORD-LEN {: len:n :}
      len 0= if at false exit then
      RECORDS at + c@ ALERT-TYPE = if at true exit then
      at len +
   again ;

\ The whole records before any alert to the client; what is not sent moves
\ to the buffer's start. False at an alert, which WITHHELD records.
: PASS ( TCP4:connection -- bool ) {: client:TCP4:connection :}
   WHOLE {: sent:n alert:bool :}
   client RECORDS sent TCP4:TRANSFER-BYTES TCP4:WRITE PEER-STATUS
   RECORDS sent + RECORDS RECORDS-U @ sent - BYTE-COPY
   sent negate RECORDS-U +!
   alert if true WITHHELD ! false exit then
   true ;

: UP ( TCP4:connection n -- bool ) {: server:TCP4:connection k:n :}
   server UPWARD k TCP4:TRANSFER-BYTES TCP4:WRITE PEER-STATUS true ;

\ The client's bytes to the server; false at the client's end.
: FROM-CLIENT ( TCP4:connection TCP4:connection -- bool )
   {: client:TCP4:connection server:TCP4:connection :}
   client UPWARD REQ-CAP TCP4:TRANSFER-BYTES TCP4:TRY-READ
   MATCH TCP4:try-read-result
      data OF BLEN>N server swap UP ENDOF
      closed OF drop false ENDOF
      empty OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;

\ The server's bytes into RECORDS, its whole records passed on; false at
\ its alert or its end.
: FROM-SERVER ( TCP4:connection TCP4:connection -- bool )
   {: client:TCP4:connection server:TCP4:connection :}
   server RECORDS RECORDS-U @ + RECORDS-CAP RECORDS-U @ - TCP4:TRANSFER-BYTES
   TCP4:TRY-READ MATCH TCP4:try-read-result
      data OF BLEN>N RECORDS-U +! client PASS ENDOF
      closed OF drop false ENDOF
      empty OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;

\ Forwards both ways until either end ends or the server's alert.
: SHUTTLE ( TCP4:connection TCP4:connection -- )
   {: client:TCP4:connection server:TCP4:connection :}
   begin
      client TCP4:CONNECTION-FD FD:POLLIN 0 >IDX PROCESS:PFD-AT!
      server TCP4:CONNECTION-FD FD:POLLIN 1 >IDX PROCESS:PFD-AT!
      2 -1 >MS PROCESS:POLL drop
      0 >IDX PROCESS:PFD-REVENTS 0<> if
         client server FROM-CLIENT 0= if exit then
      then
      1 >IDX PROCESS:PFD-REVENTS 0<> if
         client server FROM-SERVER 0= if exit then
      then
   again ;

: RELAY-SERVER ( TCP4:connection -- ) {: client:TCP4:connection :}
   TEST-TLS-PEER:LOOPBACK TCP4:ADDRESS TEST-TLS-PEER:PORT TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF client over SHUTTLE TCP4:CLOSE PEER-STATUS ENDOF
      failed OF drop ENDOF
   ;MATCH
   client TCP4:CLOSE PEER-STATUS ;

: RELAY-WORK ( -- )
   RELAY-LISTENER @ TCP4:>LISTENER TCP4:ACCEPT
   MATCH TCP4:accept-result
      accepted OF 2drop RELAY-SERVER ENDOF
      failed OF drop ENDOF
   ;MATCH
   1 RELAY-DONE atomic-add drop ;

: START-RELAY ( -- )
   0 RECORDS-U !  false WITHHELD !
   0 RELAY-DONE !
   s" the relay listens on loopback" T-LABEL
   RELAY-LISTENER RELAY-PORT TEST-TLS-PEER:LISTEN TTRUE
   ['] RELAY-WORK RELAY-TASK TASK:ACTIVATE ;

: END-RELAY ( -- )
   RELAY-DONE 1 RELAY-TASK TEST-WAIT:FOR-CELL
   RELAY-TASK TASK:KILL
   RELAY-LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER PEER-STATUS ;

\ s_server serves on TLS 1.2 from here on.
: TEST-HTTPS-CUT ( -- )
   TEST-TLS-PEER:UNSERVE true TEST-TLS-PEER:WWW-SERVE
   START-RELAY
   s" localhost" s" ca.pem" RELAY-PORT @ ROOM-CAP BEGIN-HTTPS-AT
   ASK-ROOT WIDE-SPAN READ-ALL
   END-CASE
   END-RELAY
   s" the relay closed the client in place of the close_notify" T-LABEL
   WITHHELD @ TTRUE
   s" an https answer read to the close fails `cut` without close_notify"
   T-LABEL
   HTTPC-FAULT:CUT true FAULT-IS
   LAST-STATUS @ 200 T=
   GOT$ s" <HTML>" STARTS-WITH? TTRUE ;

: HTTPS-CASES ( -- )
   TEST-TLS-PEER:CERTS
   TEST-HTTPS-SILENT
   false TEST-TLS-PEER:WWW-SERVE
   TEST-HTTPS-TRUSTED TEST-HTTPS-UNRELATED TEST-HTTPS-OTHER-HOST
   TEST-TLS-PEER:UNSERVE TEST-TLS-PEER:SCRIPT-SERVE
   TEST-HTTPS-HELD
   TEST-HTTPS-CLOSE
   TEST-HTTPS-CUT ;

: TEST-HTTPS ( -- )
   s" habu-httpc" TEST-TLS-PEER:SCRATCH
   [: HTTPS-CASES ;] [: TEST-TLS-PEER:CLEAN ;] finally ;


\ ---- The table against its model

\ The file at the path, whole, in the arena.
: FILE-IN ( str ^ARENA:arena -- str ) {: p:^ pu:n ar :}
   p pu FS:FILE-SIZE {: u:n :}
   ar u ARENA:ALLOC drop {: b:^ :}
   b  p pu b u FS:READ-ALL ;

: TABLE-IS-MODEL ( ^ARENA:arena -- ) {: ar :}
   s" src/lib/net/httpc.f" s" LIFECYCLE" s" Httpc" ar TEST-FSM:TABLE-MODULE
   s" spec/HttpcTable.tla" ar FILE-IN T$= ;

: TEST-TABLE ( -- )
   s" LIFECYCLE is the table spec/HttpcTable.tla holds" T-LABEL
   [: TABLE-IS-MODEL ;] ARENA:WITH ;

public

\ OPEN overwrites whatever the record held: a NEXT with no SEND fails `send`
\ with no cause.
: TEST-OPEN-NEXT ( -- )
   [type] HTTPC:conn width>n 0 do $A5 SPARE CONN>BYTES i + c! loop
   s" OPEN takes a record full of stale bytes" T-LABEL
   SPARE s" http://127.0.0.1" s" " ROOM-BUF ROOM-CAP HTTPC:OPEN TTRUE
   ANSWER-RESET
   s" a NEXT before any SEND fails `send` with no cause" T-LABEL
   SPARE SPAN-BUF WIDE-SPAN >LEN HTTPC:NEXT ANSWERED? TTRUE
   HTTPC-FAULT:SEND true FAULT-IS
   SPARE HTTPC:CAUSE 0 T= ;

: RUN ( -- )
   T-RESET
   TEST-TABLE
   TEST-OPEN-NEXT
   START-SERVER
   s" the server listens on loopback" T-LABEL
   STOP @ 0 T=
   PORT-CELL @ ORIGIN!
   TEST-SIZED
   TEST-CHUNKED
   TEST-CLOSING
   TEST-INTERIM
   TEST-NO-CONTENT
   TEST-REUSE
   TEST-IDLE-CLOSE
   TEST-UNREAD
   TEST-TRAILING
   TEST-LENGTH-SENT
   TEST-CONNECT
   TEST-CUT
   TEST-TIMEOUT
   TEST-MALFORMED
   TEST-OVERSIZE
   TEST-REFUSED
   TEST-CLOSE
   TEST-HTTPS
   END-SERVER ;

;package

HTTPC-TEST:RUN
T-REPORT
