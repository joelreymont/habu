\ ws-test.f - RFC 6455 connections over a real loopback port: a Habu client on
\ TCP4 shakes hands with src/lib/net/ws.f inside an HTTP server and speaks frames
\ to it, built and read with the codec's own words (src/lib/net/ws-frame.f).
\
\ Every frame the client sends or reads one at a time goes into a transcript as
\ one line - direction, opcode (`+` when more fragments follow), length, and the
\ payload in hex or, past 24 bytes, as the sum of its bytes. Pushes log no
\ frame. The transcript is written to build/ws-transcript.txt, read back and
\ compared whole with the one this file expects.
\
\ Every socket is served by one handler, NOTE, on WS's task: text and binary
\ come back as they came, the text `throw` makes it throw, and each `closed`
\ is logged with its socket. Every handler is defined before the server
\ starts, because Habu forbids dictionary mutation while a task is live.
\ Run: bin/hb --load src/lib/net/ws-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/num-types.f
require src/lib/net/tcp4.f
require src/lib/net/http.f
require src/lib/net/ws.f

package WS-TEST

$7F000001 constant LOOPBACK
2 constant WORKERS                \ fewer than the sockets a case holds open at once
$1F4 constant IDLE-MS
10 constant POLL-MS               \ how often a wait looks again
$7D0 constant ANSWER-MS           \ a loopback answer that slow is a hang
$3E8 constant GRACE-MS            \ what a bounded wait may run past its bound on a loaded box
$190 constant STOP-BOUND-MS       \ WS's GOODBYE-MS and STOP-MARGIN-MS: how long WS:STOP may take
$400000 constant QUEUE-MAX        \ WS's: the unwritten bytes behind which a frame loses its socket
$10000 constant PUSH-BYTES        \ one pushed frame's payload
$200000 constant STALL-PUSH       \ what the stalled case queues: half of QUEUE-MAX, far past its peer's buffers
$1000 constant PEER-RCVBUF       \ the stalled peer's receive buffer, fixed before it connects; macOS lo0 rounds it up
$10 constant SOCKADDR-N          \ sockaddr_in
$2000000 constant FLOOD-PUSH      \ what the overflow case may push: QUEUE-MAX and every kernel buffer, many times over
$3F constant SOCKETS             \ WS:START's limit
$100 constant LOG-CAP             \ the sockets one run may close
$12000 constant BUF-CAP           \ the longest frame a case sends or is sent, with its header
$400 constant HEAD-CAP
$10000 constant STREAM-CAP        \ what one read takes of the server's stream
$3000 constant SCRIPT-CAP
$40 constant RAW-CAP
$18 constant HEX-MAX              \ the longest payload a transcript line spells out
$37FA213D constant MASK-KEY       \ the masking key of RFC 6455 section 5.7's examples
1006 constant LOST                \ the status of a socket that ended with no close frame
1000000 constant NS-PER-MS
13 constant CR
10 constant LF
32 constant SP
-9980 constant E-SILENT           \ this file's own fixture refusals, outside every lib block
-9981 constant E-HANDLER          \ the handler that is meant to throw
-9982 constant E-DIAL
-9983 constant E-PLATFORM          \ a target this file has no socket option values for

CAST: BLEN>N ( NUM:byte-len -- n )
CAST: SOCK>N ( WS:socket -- n )

: SOL-SOCKET ( -- n )
   HB-TARGET-LINUX-KERNEL? if 1 exit then
   HB-TARGET-MACOS? if $FFFF exit then
   E-PLATFORM throw ;
: SO-RCVBUF ( -- n )
   HB-TARGET-LINUX-KERNEL? if 8 exit then
   HB-TARGET-MACOS? if $1002 exit then
   E-PLATFORM throw ;

\ The one socket control this fixture needs: Linux and macOS grow a loopback
\ socket's buffers as traffic goes, and grown ones take a whole push into the
\ kernel, where no byte waits in WS's queue.
PROCESS-SYMBOLS
FUNCTION: SET-OPT-CALL setsockopt ( n n n ptr u8 n -- i32 ) ;FUNCTION

BUF-CAP SPAN-BUFFER: PATTERN
BUF-CAP SPAN-BUFFER: CLIENT-TX
BUF-CAP SPAN-BUFFER: CLIENT-RX
WS:HEADER-MAX SPAN-BUFFER: RX-HEAD
HEAD-CAP SPAN-BUFFER: HEAD-BUF
STREAM-CAP SPAN-BUFFER: STREAM-BUF  \ what the client has read and not yet taken
HEAD-CAP SPAN-BUFFER: REQ-BUF
RAW-CAP SPAN-BUFFER: RAW-BUF
2 SPAN-BUFFER: STATUS-BUF
2 SPAN-BUFFER: HEX-PAIR
4 SPAN-BUFFER: OPT-VAL
SOCKADDR-N SPAN-BUFFER: PEER-SA  \ the server's address, for a connect the loop makes
SCRIPT-CAP SPAN-BUFFER: SCRIPT-BUF  \ the transcript this run captured
SCRIPT-CAP SPAN-BUFFER: WANT-BUF    \ the transcript this file expects
SCRIPT-CAP SPAN-BUFFER: BACK-BUF    \ the transcript read back from its file
variable SCRIPT-U
variable WANT-U
variable HEAD-U
variable STREAM-AT
variable STREAM-U
variable REQ-U
variable RX-FIN
variable RX-LEN
variable RX-EOF
TYPED-VARIABLE RX-OP WS:opcode
variable REFUSED-WITH             \ the status the route was told its handshake was refused with
variable LAST-CLOSE               \ the status the last `closed` carried; 0 once a case took it
LOG-CAP TYPED-BUFFER CLOSED-SOCK WS:socket  \ each socket that closed, in order
LOG-CAP TYPED-BUFFER CLOSED-CODE n          \ and the status it closed under
variable CLOSED-N                 \ entries logged; written last, by WS's task alone
1 TYPED-BUFFER HELD WS:socket     \ the socket the route accepted last
1 TYPED-BUFFER WATCH WS:socket    \ the socket a case awaits the close of
1 TYPED-BUFFER PUSHED-TO WS:socket  \ the socket this task pushes to
variable PEER-HELD                \ what the stalled peer held unread when last looked at
variable PEER-MOVED               \ when that last changed
variable CLOSE-GOAL               \ the count of logged closes a case awaits
SOCKETS 1+ TYPED-BUFFER CONNS TCP4:connection  \ the client connections a case holds open at once
TASK:SEMAPHORE ACCEPTED           \ the route has kept its outcome


\ ---- waits -------------------------------------------------------------------

\ Returns once the probe answers other than 0, looking every POLL-MS. A probe
\ still 0 once that many ms have passed is a deadline: the wait names itself
\ and throws E-PROC-TIMEOUT.
: AWAITED ( [ -- n ] n -- n )
   {: probe limit:n :}
   mono-ns limit NS-PER-MS * + {: deadline:n :}
   begin
      probe execute {: got:n :}
      got 0 <> if got exit then
      mono-ns deadline > if s" ws-test AWAITED" TEST-WAIT:EXPIRED then
      POLL-MS >MS TASK:SLEEP
   again ;

: LAST-CLOSED ( -- n )
   LAST-CLOSE @ ;

\ The last `closed` NOTE heard carried this status.
: CLOSED-LAST ( n -- ) {: code:n :}
   [: LAST-CLOSED ;] ANSWER-MS AWAITED code T=
   0 LAST-CLOSE ! ;

\ ---- the transcript ----------------------------------------------------------

: SCRIPT$ ( -- ptr u8 n )
   SCRIPT-BUF SCRIPT-U @ SPAN:TAKE SPAN:$ ;

: LOG ( ptr u8 n -- ) {: a u:n :}
   a u SCRIPT-BUF SCRIPT-U @ SPAN:SKIP SPAN:COPY
   u SCRIPT-U +! ;

: LOG-C ( n -- ) {: c:n :}
   c SCRIPT-BUF SCRIPT-U @ SPAN:U8!
   1 SCRIPT-U +! ;

: LOG-LINE ( ptr u8 n -- )
   LOG LF LOG-C ;

: LOG-N ( n -- ) {: v:n :}
   v 10 >= if v 10 / RECURSE then
   v 10 mod [char] 0 + LOG-C ;

: LOG-HEX ( ptr u8 n -- ) {: a u:n :}
   u 0 ?do
      a i + c@ HEX-PAIR SPAN:$ drop BYTE>HEX
      HEX-PAIR SPAN:$ LOG
   loop ;

: BYTE-SUM ( ptr u8 n -- n ) {: a u:n :}
   0 u 0 ?do a i + c@ + loop ;

: LOG-PAYLOAD ( ptr u8 n -- ) {: a u:n :}
   u 0= if exit then
   SP LOG-C
   u HEX-MAX <= if a u LOG-HEX exit then
   s" sum " LOG a u BYTE-SUM LOG-N ;

: OP$ ( WS:opcode -- ptr u8 n )
   MATCH WS:opcode
      continuation OF s" continuation" ENDOF
      text OF s" text" ENDOF
      binary OF s" binary" ENDOF
      close OF s" close" ENDOF
      ping OF s" ping" ENDOF
      pong OF s" pong" ENDOF
   ;MATCH ;

: LOG-FRAME ( ptr u8 n bool WS:opcode ptr u8 n -- ) {: dir du:n fin:bool op a u:n :}
   dir du LOG op OP$ LOG
   fin 0= if [char] + LOG-C then
   SP LOG-C u LOG-N a u LOG-PAYLOAD LF LOG-C ;


\ ---- the client --------------------------------------------------------------

: DROP-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;

: DIAL ( -- TCP4:connection )
   0 STREAM-AT ! 0 STREAM-U !
   LOOPBACK TCP4:ADDRESS HTTP:PORT TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF ENDOF
      failed OF drop E-DIAL throw ENDOF
   ;MATCH ;

\ The server's address as sockaddr_in lays it out; macOS leads with its length.
: PEER-SA! ( -- )
   SOCKADDR-N 0 ?do 0 PEER-SA i SPAN:U8! loop
   HB-TARGET-MACOS? if SOCKADDR-N PEER-SA 0 SPAN:U8! 2 PEER-SA 1 SPAN:U8!
   else 2 PEER-SA 0 SPAN:U8! then
   HTTP:PORT {: port:n :}
   port 8 rshift $FF and PEER-SA 2 SPAN:U8!
   port $FF and PEER-SA 3 SPAN:U8!
   4 0 ?do LOOPBACK 3 i - 8 * rshift $FF and PEER-SA i 4 + SPAN:U8! loop ;

\ A connection whose receive buffer is PEER-RCVBUF from its first byte: the
\ option is set before the loop connects it, and holds the buffer at that size.
: DIAL-SMALL ( -- TCP4:connection )
   0 STREAM-AT ! 0 STREAM-U !
   TCP4:SOCKET
   MATCH TCP4:socket-result
      opened OF ENDOF
      failed OF drop E-DIAL throw ENDOF
   ;MATCH {: conn:TCP4:connection :}
   4 0 ?do PEER-RCVBUF i 8 * rshift $FF and OPT-VAL i SPAN:U8! loop
   conn TCP4:CONNECTION-FD FD>N SOL-SOCKET SO-RCVBUF OPT-VAL SPAN:$ SET-OPT-CALL
   0 <> if conn TCP4:CLOSE DROP-STATUS E-DIAL throw then
   PEER-SA!
   conn TCP4:CONNECTION-FD PEER-SA SPAN:$ AIO:CONNECT AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF conn TCP4:CLOSE DROP-STATUS E-DIAL throw ENDOF
      cancelled OF conn TCP4:CLOSE DROP-STATUS E-DIAL throw ENDOF
      refused OF drop conn TCP4:CLOSE DROP-STATUS E-DIAL throw ENDOF
   ;MATCH
   conn ;

: SEND-RAW ( TCP4:connection ptr u8 n -- ) {: conn:TCP4:connection a u:n :}
   conn a u TCP4:TRANSFER-BYTES TCP4:WRITE DROP-STATUS ;

\ A server that says nothing for ANSWER-MS is a deadline that names itself,
\ instead of a hung suite.
: AWAIT ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn ANSWER-MS >MS TCP4:READABLE-WITHIN?
   MATCH TCP4:ready-result
      ready OF ENDOF
      idle OF s" ws-test AWAIT" TEST-WAIT:EXPIRED ENDOF
      failed OF drop ENDOF
   ;MATCH ;

\ One read of whatever the server has sent; nothing at the end of its stream.
: REFILL ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn AWAIT
   conn STREAM-BUF SPAN:$ TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N ENDOF
      closed OF drop 0 ENDOF
      failed OF drop 0 ENDOF
   ;MATCH
   STREAM-U !
   0 STREAM-AT ! ;

\ Bytes into the span from what the client holds, read afresh when it holds
\ none: their count, 0 at the end of the stream.
: READ-SOME ( TCP4:connection SPAN:span<u8> -- n ) {: conn:TCP4:connection dst :}
   STREAM-AT @ STREAM-U @ >= if conn REFILL then
   STREAM-U @ STREAM-AT @ - dst SPAN:LEN min {: take:n :}
   STREAM-BUF STREAM-AT @ take SPAN:SUB SPAN:$ dst SPAN:COPY
   take STREAM-AT +!
   take ;

\ The whole span, or false when the stream ended first.
: READ-N ( TCP4:connection SPAN:span<u8> -- bool ) {: conn:TCP4:connection dst :}
   dst SPAN:LEN 0= if true exit then
   conn dst READ-SOME {: got:n :}
   got 0= if false exit then
   conn dst got SPAN:SKIP RECURSE ;

: HEAD$ ( -- ptr u8 n )
   HEAD-BUF HEAD-U @ SPAN:TAKE SPAN:$ ;

\ The response head and not a byte past it: frames may follow at once.
: READ-HEAD ( TCP4:connection -- ) {: conn:TCP4:connection :}
   0 HEAD-U !
   begin
      HEAD$ S\" \r\n\r\n" ENDS-WITH? if exit then
      conn HEAD-BUF HEAD-U @ 1 SPAN:SUB READ-N 0= if exit then
      1 HEAD-U +!
   again ;

: REQ$ ( -- ptr u8 n )
   REQ-BUF REQ-U @ SPAN:TAKE SPAN:$ ;

: REQ+ ( ptr u8 n -- ) {: a u:n :}
   a u REQ-BUF REQ-U @ SPAN:SKIP SPAN:COPY
   u REQ-U +! ;

: LINE+ ( ptr u8 n ptr u8 n -- ) {: name nu:n value vu:n :}
   name nu REQ+ s" : " REQ+ value vu REQ+ S\" \r\n" REQ+ ;

\ A request line naming that version, with no header behind it.
: REQ-BARE ( ptr u8 n ptr u8 n ptr u8 n -- ) {: method mu:n path pu:n version vu:n :}
   0 REQ-U !
   method mu REQ+ s"  " REQ+ path pu REQ+ s"  " REQ+ version vu REQ+ S\" \r\n" REQ+ ;

: HOST+ ( -- )
   s" Host" s" ws-test" LINE+ ;

\ An HTTP/1.1 request line and its Host.
: REQ-START ( ptr u8 n ptr u8 n -- ) {: method mu:n path pu:n :}
   method mu path pu s" HTTP/1.1" REQ-BARE
   HOST+ ;

: KEY$ ( -- ptr u8 n )
   s" dGhlIHNhbXBsZSBub25jZQ==" ;

\ The four lines of a handshake with nothing wrong with it.
: GOOD-LINES ( -- )
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+ ;

\ The request as built goes out with whatever a case put behind its head, and
\ the answer's head is read.
: REQ-OVER ( TCP4:connection -- TCP4:connection ) {: conn:TCP4:connection :}
   conn REQ$ SEND-RAW
   conn READ-HEAD
   conn ;

: REQ-SEND ( -- TCP4:connection )
   DIAL REQ-OVER ;

: UPGRADE-OVER ( TCP4:connection ptr u8 n -- TCP4:connection ) {: conn:TCP4:connection path pu:n :}
   s" GET" path pu REQ-START GOOD-LINES S\" \r\n" REQ+
   conn REQ-OVER
   HEAD$ s" HTTP/1.1 101 " STARTS-WITH? TTRUE ;

: UPGRADE ( ptr u8 n -- TCP4:connection )
   DIAL -rot UPGRADE-OVER ;

\ One masked frame as a client sends it, in the client's own buffer.
: FRAME$ ( bool WS:opcode ptr u8 n -- ptr u8 n ) {: fin:bool op a u:n :}
   fin op u MASK-KEY WS-HEADER:MAKE WS-SENDER:client CLIENT-TX WS:ENCODE-HEADER {: size:n :}
   a u CLIENT-TX size SPAN:SKIP SPAN:COPY
   MASK-KEY CLIENT-TX size u SPAN:SUB WS:MASK
   CLIENT-TX size u + SPAN:TAKE SPAN:$ ;

: SAY ( TCP4:connection bool WS:opcode ptr u8 n -- )
   {: conn:TCP4:connection fin:bool op a u:n :}
   s" > " fin op a u LOG-FRAME
   conn fin op a u FRAME$ SEND-RAW ;

: NIBBLE ( n -- n ) {: c:n :}
   c [char] a >= if c [char] a - 10 + exit then
   c [char] 0 - ;

\ Bytes written in hex, two lowercase digits each.
: BYTES ( ptr u8 n -- ptr u8 n ) {: a u:n :}
   u 2 / {: k:n :}
   k 0 ?do
      a i 2 * + c@ NIBBLE 4 lshift a i 2 * + 1+ c@ NIBBLE or $FF and
      RAW-BUF i SPAN:U8!
   loop
   RAW-BUF k SPAN:TAKE SPAN:$ ;

\ Bytes no well-behaved client frames, as they are written.
: SAY-RAW ( TCP4:connection ptr u8 n -- ) {: conn:TCP4:connection a u:n :}
   s" > raw " LOG a u LOG-LINE
   conn a u BYTES SEND-RAW ;

: STATUS$ ( n -- ptr u8 n ) {: code:n :}
   code 8 rshift $FF and STATUS-BUF 0 SPAN:U8!
   code $FF and STATUS-BUF 1 SPAN:U8!
   STATUS-BUF SPAN:$ ;

: KEEP-HEADER ( WS:header n -- bool ) {: h size:n :}
   h WS-HEADER:UNMAKE {: fin:bool op len:n key:n :}
   fin if 1 else 0 then RX-FIN !
   op RX-OP !
   len RX-LEN !
   true ;

\ True once the bytes held are a whole header; until then the count it needs.
: HEADER? ( n -- n bool ) {: held:n :}
   RX-HEAD SPAN:$ drop held WS-SENDER:server BUF-CAP WS:DECODE-HEADER
   MATCH WS:decoded
      need OF false ENDOF
      frame OF KEEP-HEADER held swap ENDOF
   ;MATCH ;

: READ-HEADER ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   conn RX-HEAD 2 SPAN:TAKE READ-N 0= if false exit then
   2 HEADER? if drop true exit then {: size:n :}
   conn RX-HEAD 2 size 2 - SPAN:SUB READ-N 0= if false exit then
   size HEADER? nip ;

\ The next frame the server sent, or the end of its stream.
: NEXT-FRAME ( TCP4:connection -- ) {: conn:TCP4:connection :}
   0 RX-EOF ! 0 RX-LEN !
   conn READ-HEADER 0= if 1 RX-EOF ! exit then
   conn CLIENT-RX RX-LEN @ SPAN:TAKE READ-N 0= if 1 RX-EOF ! then ;

: RX$ ( -- ptr u8 n )
   CLIENT-RX RX-LEN @ SPAN:TAKE SPAN:$ ;

: HEAR ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn NEXT-FRAME
   RX-EOF @ 0 <> if s" < end of stream" LOG-LINE exit then
   s" < " RX-FIN @ 0 <> RX-OP @ RX$ LOG-FRAME ;

\ The frame in hand is final, of this opcode, with this payload.
: GOT ( WS:opcode ptr u8 n -- ) {: op want wu:n :}
   RX-EOF @ 0 T=
   RX-FIN @ 1 T=
   RX-OP @ OP$ op OP$ T$=
   RX$ want wu T$= ;

: GOT-CLOSE ( n -- ) {: code:n :}
   WS-OPCODE:close code STATUS$ GOT ;

: GOT-END ( -- )
   RX-EOF @ 1 T= ;

\ The server ends the stream, which it does once the handler has returned.
: ENDS ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn HEAR GOT-END
   conn TCP4:CLOSE DROP-STATUS ;

: EXCHANGE ( TCP4:connection WS:opcode ptr u8 n -- ) {: conn:TCP4:connection op a u:n :}
   conn true op a u SAY
   conn HEAR op a u GOT ;

\ The close handshake a client starts, then the end of the stream.
: FAREWELL ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn true WS-OPCODE:close 1000 STATUS$ SAY
   conn HEAR 1000 GOT-CLOSE
   conn ENDS ;

\ The server fails the connection with this status and ends the stream.
: FAILED ( TCP4:connection n -- ) {: conn:TCP4:connection code:n :}
   conn HEAR code GOT-CLOSE
   conn ENDS
   code CLOSED-LAST ;

: SECTION ( ptr u8 n -- ) {: a u:n :}
   a u T-LABEL
   s" == " LOG a u LOG-LINE ;




\ ---- the handler, the routes, and what they kept ------------------------------

\ The status the socket closed under, as NOTE logged it; 0 while it has not.
: CLOSED-OF ( WS:socket -- n ) {: sock:WS:socket :}
   CLOSED-N atomic@ 0 ?do
      i CLOSED-SOCK @ SOCK>N sock SOCK>N = if i CLOSED-CODE @ unloop exit then
   loop
   0 ;

: WATCHED ( -- n )
   0 WATCH @ CLOSED-OF ;

: CLOSED-IN ( n WS:socket -- ) {: code:n sock:WS:socket :}
   CLOSED-N @ {: k:n :}
   k LOG-CAP >= if E-SILENT throw then
   sock k CLOSED-SOCK !
   code k CLOSED-CODE !
   k 1+ CLOSED-N atomic!
   code LAST-CLOSE ! ;

: TEXT-IN ( ptr u8 n WS:socket -- ) {: a u:n sock:WS:socket :}
   a u s" throw" STR= if E-HANDLER throw then
   sock a u WS:SEND-TEXT ;

: BINARY-IN ( ptr u8 n WS:socket -- ) {: a u:n sock:WS:socket :}
   sock a u WS:SEND-BINARY ;

\ Every socket's handler, on WS's task.
: NOTE ( WS:socket WS:message -- ) {: sock:WS:socket msg :}
   msg
   MATCH WS:message
      text OF sock TEXT-IN ENDOF
      binary OF sock BINARY-IN ENDOF
      closed OF sock CLOSED-IN ENDOF
   ;MATCH ;

\ The route keeps what ACCEPT answered and says so.
: OPEN ( HTTP:request HTTP:response -- )
   [: NOTE ;] WS:ACCEPT
   MATCH WS:accepted
      open OF 0 HELD ! ENDOF
      refused OF REFUSED-WITH ! ENDOF
   ;MATCH
   ACCEPTED TASK:SIGNAL ;

: PLAIN ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   answer 200 HTTP:STATUS!
   answer s" plain" HTTP:BODY! ;

: INSTALL-ROUTES ( -- )
   HTTP:ROUTES-RESET
   s" GET" s" /ws" [: OPEN ;] HTTP:ROUTE
   s" GET" s" /plain" [: PLAIN ;] HTTP:ROUTE ;


\ ---- the cases ---------------------------------------------------------------

\ The socket's handler is told `closed` with this status within that many ms.
: CLOSES-WITHIN ( WS:socket n n -- ) {: sock:WS:socket code:n limit:n :}
   sock 0 WATCH !
   [: WATCHED ;] limit AWAITED code T=
   0 LAST-CLOSE ! ;

\ A handshake on /ws that passed: the client's connection and its socket.
: OPENS-OVER ( TCP4:connection -- TCP4:connection WS:socket )
   s" /ws" UPGRADE-OVER
   ACCEPTED TASK:WAIT
   0 HELD @ ;

: OPENS ( -- TCP4:connection WS:socket )
   DIAL OPENS-OVER ;

\ The body read behind the head is dropped with its connection.
: PLAIN-ONCE ( -- )
   s" GET" s" /plain" REQ-START
   s" Connection" s" close" LINE+
   S\" \r\n" REQ+
   REQ-SEND {: conn:TCP4:connection :}
   HEAD$ s" HTTP/1.1 200 OK" STARTS-WITH? TTRUE
   conn TCP4:CLOSE DROP-STATUS
   0 STREAM-AT ! 0 STREAM-U ! ;

\ Text and binary reach the handler as the messages they are, and its replies
\ come back as such.
: ECHO-CASE ( -- )
   s" text and binary reach the handler" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn WS-OPCODE:text s" hello" EXCHANGE
   conn WS-OPCODE:binary PATTERN 5 SPAN:TAKE SPAN:$ EXCHANGE
   conn FAREWELL
   sock 1000 ANSWER-MS CLOSES-WITHIN ;

\ The peer's close reaches the handler as `closed` with the status it carried.
: PEER-CLOSE-CASE ( -- )
   s" the peer's close reaches the handler" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:close 4001 STATUS$ SAY
   conn HEAR 4001 GOT-CLOSE
   conn ENDS
   sock 4001 ANSWER-MS CLOSES-WITHIN ;

\ A handler that throws closes its socket under 1011.
: THROW-CASE ( -- )
   s" a throwing handler" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:text s" throw" SAY
   conn 1011 FAILED ;

\ More sockets than the server has workers stay open, and plain requests are
\ still answered: no socket holds a worker.
: MANY-CASE ( -- )
   s" more sockets than workers" SECTION
   HTTP:MAX-WORKERS 1+ {: n:n :}
   n 0 ?do OPENS drop i CONNS ! loop
   PLAIN-ONCE PLAIN-ONCE
   n 0 ?do i CONNS @ WS-OPCODE:text s" each" EXCHANGE loop
   n 0 ?do i CONNS @ FAREWELL loop ;

\ A task that is no HTTP worker sends through a socket it was handed.
: PUSH-CASE ( -- )
   s" a push from another task" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   sock s" pushed" WS:SEND-TEXT
   conn HEAR WS-OPCODE:text s" pushed" GOT
   conn FAREWELL ;

: PUSH-ONE ( ptr u8 n -- ptr u8 n ) {: a u:n :}
   0 PUSHED-TO @ a u WS:SEND-BINARY
   a u ;

\ That many PUSH-BYTES frames from this task to the socket; the code the first
\ refused push threw, or 0 when all of them were queued.
: PUSHES ( WS:socket n -- n ) {: sock:WS:socket n:n :}
   sock 0 PUSHED-TO !
   n 0 ?do
      PATTERN PUSH-BYTES SPAN:TAKE SPAN:$ [: PUSH-ONE ;] catch {: code:n :} drop drop
      code 0 <> if code unloop exit then
   loop
   0 ;

\ What the peer's end holds and its reader has not taken.
: UNREAD-N ( TCP4:connection -- n )
   TCP4:UNREAD
   MATCH TCP4:queue-result
      queued OF BLEN>N ENDOF
      failed OF drop E-SILENT throw ENDOF
   ;MATCH ;

\ The socket's handler is told `closed` 1006 within WS:STALL-MS of the last
\ byte the peer's end took, as WS's own clock runs. That moment is the
\ kernel's: a zero-window probe, sent first at BSD's 5 s persist minimum, can
\ fill the part of a segment still free in the peer's buffer just as STALL-MS
\ runs out, and WS rightly starts its clock again.
: STALL-CLOSES ( TCP4:connection WS:socket -- ) {: peer:TCP4:connection sock:WS:socket :}
   sock 0 WATCH !
   peer UNREAD-N PEER-HELD !
   mono-ns PEER-MOVED !
   begin
      WATCHED {: code:n :}
      code 0 <> if code LOST T= 0 LAST-CLOSE ! exit then
      peer UNREAD-N {: held:n :}
      held PEER-HELD @ <> if held PEER-HELD ! mono-ns PEER-MOVED ! then
      mono-ns PEER-MOVED @ - WS:STALL-MS GRACE-MS + NS-PER-MS * >
      if s" ws-test STALL-CLOSES" TEST-WAIT:EXPIRED then
      POLL-MS >MS TASK:SLEEP
   again ;

\ A peer that reads nothing of a queue for WS:STALL-MS is closed under 1006,
\ and another socket's messages go back and forth meanwhile.
: STALL-CASE ( -- )
   s" a stalled peer" SECTION
   DIAL-SMALL OPENS-OVER {: still:TCP4:connection stuck:WS:socket :}
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   stuck STALL-PUSH PUSH-BYTES / PUSHES 0 T=
   conn WS-OPCODE:text s" meanwhile" EXCHANGE
   stuck 0 WATCH !
   WATCHED 0 T=
   still stuck STALL-CLOSES
   conn WS-OPCODE:text s" after" EXCHANGE
   conn FAREWELL
   still TCP4:CLOSE DROP-STATUS ;

\ A peer that reads nothing while a task pushes past QUEUE-MAX loses its socket
\ under 1006 at once, the push is E-WS-CLOSED, and another socket's messages
\ are not held up.
: OVERFLOW-CASE ( -- )
   s" a queue past its bound" SECTION
   OPENS {: still:TCP4:connection stuck:WS:socket :}
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   stuck FLOOD-PUSH PUSH-BYTES / PUSHES E-WS-CLOSED T=
   stuck LOST ANSWER-MS CLOSES-WITHIN
   conn WS-OPCODE:text s" unhindered" EXCHANGE
   conn FAREWELL
   still TCP4:CLOSE DROP-STATUS ;

: ALL-CLOSED ( -- n )
   CLOSED-N atomic@ CLOSE-GOAL @ >= if 1 else 0 then ;

\ The socket past WS's table is refused 503, and no socket exists for it.
: CAPACITY-CASE ( -- )
   s" the socket past capacity" SECTION
   CLOSED-N @ {: base:n :}
   SOCKETS 0 ?do OPENS drop i CONNS ! loop
   s" GET" s" /ws" REQ-START GOOD-LINES S\" \r\n" REQ+
   REQ-SEND {: conn:TCP4:connection :}
   ACCEPTED TASK:WAIT
   HEAD$ s" HTTP/1.1 503 " STARTS-WITH? TTRUE
   REFUSED-WITH @ 503 T=
   conn TCP4:CLOSE DROP-STATUS
   base SOCKETS + CLOSE-GOAL !
   SOCKETS 0 ?do i CONNS @ TCP4:CLOSE DROP-STATUS loop
   [: ALL-CLOSED ;] ANSWER-MS AWAITED drop
   0 LAST-CLOSE ! ;

\ WS:STOP sends 1001 to an open socket, tells its handler, and returns within
\ its bound.
: STOP-CASE ( -- )
   s" a stop" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   mono-ns {: began:n :}
   WS:STOP
   mono-ns began - STOP-BOUND-MS GRACE-MS + NS-PER-MS * < TTRUE
   conn HEAR 1001 GOT-CLOSE
   conn ENDS
   sock CLOSED-OF 1001 T= ;


\ ---- the transcript this file expects ----------------------------------------

: WANT$ ( -- ptr u8 n )
   WANT-BUF WANT-U @ SPAN:TAKE SPAN:$ ;

: W ( ptr u8 n -- ) {: a u:n :}
   a u WANT-BUF WANT-U @ SPAN:SKIP SPAN:COPY
   u WANT-U +!
   LF WANT-BUF WANT-U @ SPAN:U8!
   1 WANT-U +! ;

\ A close frame and the close that answers it, then the end of the stream.
: W-FAREWELL ( -- )
   s" > close 2 03e8" W
   s" < close 2 03e8" W
   s" < end of stream" W ;

: W-FAILED ( ptr u8 n -- )
   W
   s" < end of stream" W ;

: WANT-TRANSCRIPT ( -- )
   0 WANT-U !
   s" == the socket past capacity" W
   s" == text and binary reach the handler" W
   s" > text 5 68656c6c6f" W
   s" < text 5 68656c6c6f" W
   s" > binary 5 0726456483" W
   s" < binary 5 0726456483" W
   W-FAREWELL
   s" == the peer's close reaches the handler" W
   s" > close 2 0fa1" W
   s" < close 2 0fa1" W
   s" < end of stream" W
   s" == a throwing handler" W
   s" > text 5 7468726f77" W
   s" < close 2 03f3" W-FAILED
   s" == more sockets than workers" W
   HTTP:MAX-WORKERS 1+ 0 ?do
      s" > text 4 65616368" W
      s" < text 4 65616368" W
   loop
   HTTP:MAX-WORKERS 1+ 0 ?do W-FAREWELL loop
   s" == a push from another task" W
   s" < text 6 707573686564" W
   W-FAREWELL
   s" == a queue past its bound" W
   s" > text 10 756e68696e6465726564" W
   s" < text 10 756e68696e6465726564" W
   W-FAREWELL
   s" == a stalled peer" W
   s" > text 9 6d65616e7768696c65" W
   s" < text 9 6d65616e7768696c65" W
   s" > text 5 6166746572" W
   s" < text 5 6166746572" W
   W-FAREWELL
   s" == a stop" W
   s" < close 2 03e9" W-FAILED ;

\ The artifact as another reader will find it: read back from its file and
\ compared whole with the transcript this file expects.
: TRANSCRIPT-CASE ( -- )
   s" the transcript" T-LABEL
   s" build" MAKE-DIRS
   s" build/ws-transcript.txt" SCRIPT$ WRITE-ALL
   s" build/ws-transcript.txt" BACK-BUF SPAN:$ READ-ALL {: got:n :}
   WANT-TRANSCRIPT
   BACK-BUF got SPAN:TAKE SPAN:$ WANT$ T$= ;


\ ---- the lifecycle -----------------------------------------------------------

$3E8 constant DRAIN-MS            \ how long a stop waits for the ring to drain
10 constant RETRY-MS

\ AIO:STOP is E-AIO-BUSY until the loop has drained every record, so the stop
\ is retried to a bound (src/lib/net/http-test.f AIO-STOP). Only E-AIO-BUSY is
\ retried: any other code from AIO:STOP is a fault and is rethrown at once. A
\ ring still busy past DRAIN-MS is a deadline: the wait names itself and throws
\ E-PROC-TIMEOUT.
: AIO-STOP ( -- )
   mono-ns DRAIN-MS NS-PER-MS * + {: deadline:n :}
   begin
      [: AIO:STOP ;] catch {: code:n :}
      code 0= if exit then
      code E-AIO-BUSY <> if code throw then
      mono-ns deadline > if s" ws-test AIO-STOP" TEST-WAIT:EXPIRED then
      RETRY-MS >MS TASK:SLEEP
   again ;

: FILL-BUFFERS ( -- )
   BUF-CAP 0 ?do i 31 * 7 + $FF and PATTERN i SPAN:U8! loop ;

\ The table full first, while no other socket holds a slot.
: CASES ( -- )
   CAPACITY-CASE ECHO-CASE PEER-CLOSE-CASE THROW-CASE MANY-CASE PUSH-CASE
   OVERFLOW-CASE STALL-CASE ;

public

\ The loop first: every wait the server, WS's task and this client make rides
\ it. WS stops before the server, and the server's stop kills no task: no
\ socket holds a worker.
: RUN ( -- )
   T-RESET
   0 SCRIPT-U !
   0 CLOSED-N !
   0 LAST-CLOSE !
   FILL-BUFFERS
   0 ACCEPTED TASK:SEMAPHORE-INIT
   AIO:START
   INSTALL-ROUTES
   LOOPBACK 0 WORKERS IDLE-MS HTTP:START
   SOCKETS WS:START
   CASES
   STOP-CASE
   HTTP:STOP
   s" every handler returned" T-LABEL
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   HTTP:KILLED-TASKS 0 T=
   AIO-STOP
   ACCEPTED TASK:SEMAPHORE-DESTROY
   TRANSCRIPT-CASE ;

;package

WS-TEST:RUN
T-REPORT
