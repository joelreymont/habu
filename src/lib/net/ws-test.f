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
\ come back as they came, a few texts are cues (TEXT-IN), and each `closed` is
\ logged with its socket, so a case checks the status of the socket it opened
\ and no close of an earlier case can land in a later one. Every handler is
\ defined before the server starts, because Habu forbids dictionary mutation
\ while a task is live.
\ Run: bin/hb --load src/lib/net/ws-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/fd.f
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

\ White-box helpers: reopens WS for what no public word reaches. ACCEPT-STALLED
\ is ACCEPT with a stall run once the handshake holds its slot, as a worker the
\ OS keeps off the processor would; LAST-GEN gives a free slot the last
\ generation a handle carries. The definitions land in WS-TEST, so WS's public
\ surface gains nothing for being tested.
package WS
: WS-TEST:ACCEPT-STALLED ( HTTP:request HTTP:response [ socket message -- ] [ -- ] -- accepted )
   {: asked:HTTP:request answer:HTTP:response q stall :}
   RUNNING? 0= if E-WS-STATE throw then
   HTTP:WORKER-SLOT {: w:n :}
   asked w VERDICT {: status:n :}
   status 0 <> if answer status REFUSE exit then
   RESERVE {: idx:n :}
   idx 0 < if answer 503 REFUSE exit then
   stall execute
   asked answer q idx w OPEN-SOCKET ;
: WS-TEST:LAST-GEN ( n -- ) {: idx:n :}
   LOCK TASK:GET
   GEN-MASK idx GEN !
   LOCK TASK:RELEASE ;
;package

\ White-box helper: reopens HTTP for its listener's descriptor, which the fault
\ case polls so the listener's own re-polls never free a descriptor under
\ Darwin's poll bound. It lands in WS-TEST, so HTTP's public surface gains
\ nothing for being tested.
package HTTP
: WS-TEST:LISTEN-FD ( -- fd ) LISTENER@ TCP4:LISTENER-FD ;
;package

package WS-TEST

$7F000001 constant LOOPBACK
2 constant WORKERS                \ fewer than the sockets a case holds open at once
$1F4 constant IDLE-MS
10 constant POLL-MS               \ how often a wait looks again
$7D0 constant ANSWER-MS           \ a loopback answer that slow is a hang
$3E8 constant GRACE-MS            \ what a bounded wait may run past its bound on a loaded box
$190 constant STOP-BOUND-MS       \ WS's GOODBYE-MS and STOP-MARGIN-MS: how long WS:STOP may take
STOP-BOUND-MS 2 / constant IDLE-STOP-MS \ an idle socket's stop: far over its few ms, under a stop that waits out its bound
STOP-BOUND-MS GRACE-MS + constant HOLD-MS \ how long a stalled handshake holds its slot: past any stop's bound
$400000 constant QUEUE-MAX        \ WS's: the unwritten bytes behind which a frame loses its socket
$10000 constant PUSH-BYTES        \ one pushed frame's payload
$200000 constant STALL-PUSH       \ what a stalled peer's socket queues: half of QUEUE-MAX, far past its peer's buffers
$1000 constant PEER-RCVBUF       \ the stalled peer's receive buffer, fixed before it connects; macOS lo0 rounds it up
$10 constant SOCKADDR-N          \ sockaddr_in
$2000000 constant FLOOD-PUSH      \ what the overflow case may push: QUEUE-MAX and every kernel buffer, many times over
$101 constant CROWD               \ WS:START's limit: one socket past a one-byte slot field
$400 constant LOG-CAP             \ the sockets one run may close
$2800 constant FILL-CAP              \ Darwin's poll bound (OPEN_MAX), past Linux's 512 records: the most polls that fill AIO
CROWD 2 * $40 + FILL-CAP 2 * + constant FDS-WANT \ both ends of CROWD+1 loopback connections, the server's, the files' and the fill's
8 constant STRAYS                 \ the wake pipe's two, the only ones a stale send's POKE writes, with room to spare
$12000 constant BUF-CAP           \ the longest frame a case sends or is sent, with its header
$11170 constant BIG               \ 70000 bytes: past the 16-bit length form
5000 constant BLAST-BYTES         \ one frame of a task that sends while another does
200 constant BLAST-N              \ the frames each of those two tasks sends, far under QUEUE-MAX in all
$50 constant PUSH-BYTE            \ 'P', what this task's frames are full of
$48 constant HANDLER-BYTE         \ 'H', what the handler's are
4000 constant BURST-PINGS         \ empty pings the client sends in one write
6 constant PING-FRAME             \ an empty masked ping: its header and mask key
BURST-PINGS PING-FRAME * constant VOLLEY-CAP
$400 constant HEAD-CAP
$10000 constant STREAM-CAP        \ what one read takes of the server's stream
$3000 constant SCRIPT-CAP
$40 constant RAW-CAP
$100 constant ERR-CAP
0 constant F-DUPFD
$A constant SAVE-FD-MIN           \ the saved stderr lands clear of 0..2
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
BLAST-BYTES SPAN-BUFFER: PUSH-BUF
BLAST-BYTES SPAN-BUFFER: HANDLER-BUF
VOLLEY-CAP SPAN-BUFFER: VOLLEY-BUF  \ the pings the client sends in one write
2 SPAN-BUFFER: NOT-UTF8
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
ERR-CAP SPAN-BUFFER: ERR-BUF        \ what WS wrote to stderr, read back
ERR-CAP SPAN-BUFFER: ERR-WANT       \ the lines this file expects there
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
variable STALLING                 \ set once the stalled handshake holds its slot
variable BAD-TEXT                 \ what the handler's send of text that is not UTF-8 threw
variable BAD-CODE                 \ what the handler's close under 1005 threw
variable LATE-SEND                \ what the handler's send behind its own close threw
variable SECOND-ACCEPT            \ what accepting one request twice threw
variable HANDLER-STOP             \ what the handler's WS:STOP threw
variable GATE-READY               \ set once STOP-TASK waits at the gate
variable GATE-OPEN                \ set to let it through
variable PUSHED-N                 \ whole frames the client read from this task
variable HANDLER-N                \ and from the handler
variable TORN                     \ frames that were neither
variable PONGS                    \ whole pongs read of those the burst is owed
LOG-CAP TYPED-BUFFER CLOSED-SOCK WS:socket  \ each socket that closed, in order
LOG-CAP TYPED-BUFFER CLOSED-CODE n          \ and the status it closed under
variable CLOSED-N                 \ entries logged; written last, by WS's task alone
1 TYPED-BUFFER HELD WS:socket     \ the socket the route accepted last
1 TYPED-BUFFER NOTED WS:socket    \ the socket whose cue the handler is acting on
1 TYPED-BUFFER KEPT-REQUEST HTTP:request
1 TYPED-BUFFER KEPT-RESPONSE HTTP:response
1 TYPED-BUFFER WATCH WS:socket    \ the socket a case awaits the close of
1 TYPED-BUFFER PUSHED-TO WS:socket  \ the socket this task pushes to
variable PEER-HELD                \ what the stalled peer held unread when last looked at
variable PEER-MOVED               \ when that last changed
variable CLOSE-GOAL               \ the count of logged closes a case awaits
variable HALT-ARMED               \ set while the next `closed` handler is to block
variable HALT-FD                  \ the file that handler opened
variable THROW-ARMED              \ set while the next `closed` handler is to throw
variable SAVED-STDERR             \ the process's own stderr while a case's is a file
variable ERR-U
variable ERR-WANT-U
STRAYS TYPED-BUFFER STRAY-FD n    \ descriptors on one file, taking the numbers a stop gave back
create ONE-BYTE $01 c,
CROWD 1+ TYPED-BUFFER CONNS TCP4:connection  \ the client connections a case holds open at once
TASK:SEMAPHORE ACCEPTED           \ the route has kept its outcome
TASK:SEMAPHORE FILLED             \ AIO is full: the held handshake goes on
FILL-CAP TYPED-BUFFER FILL-TICKET AIO:ticket  \ polls that never fire, holding AIO at its ceiling
FILL-CAP TYPED-BUFFER FILL-R n    \ each on its own pipe: Darwin bounds descriptors, and polls on
FILL-CAP TYPED-BUFFER FILL-W n    \ duplicates of one pipe made each AIO turn take 2-3 s (2026-10-29)
1 TYPED-BUFFER PIN-TICKET AIO:ticket \ a poll on HTTP's listener, whose timeouts then free no descriptor
variable PINNED                   \ whether PIN-TICKET holds a poll in flight
variable FILL-N                   \ the polls in flight
variable FILL-CODE                \ what AIO refused the poll past them with
TASK:MIN-STACK TASK:TASK STOP-TASK \ runs WS:STOP while this task answers a close


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

: SECTION ( ptr u8 n -- ) {: a u:n :}
   a u T-LABEL
   s" == " LOG a u LOG-LINE ;

: PATTERN$ ( n -- ptr u8 n )
   PATTERN swap SPAN:TAKE SPAN:$ ;

\ The head as it arrived, a line to a line.
: LOG-HEAD ( -- )
   HEAD$ {: a u:n :}
   u 0 ?do a i + c@ dup CR = if drop else LOG-C then loop ;

: FIRST-LINE ( ptr u8 n -- ptr u8 n ) {: a u:n :}
   u 0 ?do a i + c@ CR = if a i unloop exit then loop
   a u ;


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
   k 1+ CLOSED-N atomic! ;

: SEND-BAD ( -- )
   0 NOTED @ NOT-UTF8 SPAN:$ WS:SEND-TEXT ;

: CLOSE-UNSENDABLE ( -- )
   0 NOTED @ 1005 WS:CLOSE ;

: SEND-LATE ( -- )
   0 NOTED @ s" late" WS:SEND-TEXT ;

\ The handler closes first: a status no frame may carry is refused, a second
\ close is nothing new, and nothing is sent behind the close.
: CLOSE-FIRST ( WS:socket -- ) {: sock:WS:socket :}
   [: CLOSE-UNSENDABLE ;] catch BAD-CODE atomic!
   sock 1000 WS:CLOSE
   sock 1000 WS:CLOSE
   [: SEND-LATE ;] catch LATE-SEND atomic! ;

\ The handler's BLAST-N frames, sent while this task sends its own.
: BLAST ( WS:socket -- ) {: sock:WS:socket :}
   BLAST-N 0 ?do sock HANDLER-BUF SPAN:$ WS:SEND-BINARY loop ;

\ Text comes back as it came, but for the cues: `throw` makes the handler
\ throw, `badtext` sends text that is not UTF-8 and nothing else, `go` closes
\ first, `bye` closes under 4000, `blast` sends BLAST-N frames, and `stop`
\ calls WS:STOP.
: TEXT-IN ( ptr u8 n WS:socket -- ) {: a u:n sock:WS:socket :}
   sock 0 NOTED !
   a u s" throw" STR= if E-HANDLER throw then
   a u s" stop" STR= if [: WS:STOP ;] catch HANDLER-STOP atomic! exit then
   a u s" badtext" STR= if [: SEND-BAD ;] catch BAD-TEXT atomic! exit then
   a u s" go" STR= if sock CLOSE-FIRST exit then
   a u s" bye" STR= if sock 4000 WS:CLOSE exit then
   a u s" blast" STR= if sock BLAST exit then
   sock a u WS:SEND-TEXT ;

: BINARY-IN ( ptr u8 n WS:socket -- ) {: a u:n sock:WS:socket :}
   sock a u WS:SEND-BINARY ;

\ The file the cases below open on descriptors the stop gave back.
: STRAY$ ( -- ptr u8 n )
   s" build/ws-stray.txt" ;

: HALTED$ ( -- ptr u8 n )
   s" build/ws-halted.txt" ;

\ Once HALT-CASE arms it, the next `closed` handler opens a file, on the
\ descriptor number its socket's connection has just given back, and blocks
\ past any stop's bound, until WS:STOP kills WS's task.
: BLOCK-IN-CLOSED ( -- )
   HALT-ARMED @ 0= if exit then
   0 HALT-ARMED !
   HALTED$ FS:OPEN-APPEND-FD HALT-FD !
   mono-ns STOP-BOUND-MS GRACE-MS + NS-PER-MS * + {: deadline:n :}
   begin
      POLL-MS >MS TASK:SLEEP
      TASK:PAUSE
      mono-ns deadline >
   until ;

\ Once THROW-CASE arms it, the next `closed` handler throws.
: THROW-IN-CLOSED ( -- )
   THROW-ARMED @ 0= if exit then
   0 THROW-ARMED !
   E-HANDLER throw ;

\ Every socket's handler, on WS's task.
: NOTE ( WS:socket WS:message -- ) {: sock:WS:socket msg :}
   msg
   MATCH WS:message
      text OF sock TEXT-IN ENDOF
      binary OF sock BINARY-IN ENDOF
      closed OF sock CLOSED-IN BLOCK-IN-CLOSED THROW-IN-CLOSED ENDOF
   ;MATCH ;

: KEEP ( WS:accepted -- )
   MATCH WS:accepted
      open OF 0 HELD ! ENDOF
      refused OF REFUSED-WITH ! ENDOF
   ;MATCH ;

\ The route keeps what ACCEPT answered and says so.
: OPEN ( HTTP:request HTTP:response -- )
   [: NOTE ;] WS:ACCEPT KEEP
   ACCEPTED TASK:SIGNAL ;

: ACCEPT-AGAIN ( -- )
   0 KEPT-REQUEST @ 0 KEPT-RESPONSE @ [: NOTE ;] WS:ACCEPT KEEP ;

\ One request is one socket: a second accept of it is refused, and the first
\ socket serves on.
: TWICE ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   asked 0 KEPT-REQUEST !
   answer 0 KEPT-RESPONSE !
   asked answer [: NOTE ;] WS:ACCEPT KEEP
   [: ACCEPT-AGAIN ;] catch SECOND-ACCEPT !
   ACCEPTED TASK:SIGNAL ;

\ The handshake stalls HOLD-MS once it holds its slot.
: STALL ( -- )
   1 STALLING !
   HOLD-MS >MS TASK:SLEEP ;

: STALLED ( HTTP:request HTTP:response -- )
   [: NOTE ;] [: STALL ;] WS-TEST:ACCEPT-STALLED KEEP
   ACCEPTED TASK:SIGNAL ;

\ The handshake waits, once it holds its slot, until AIO is full.
: AWAIT-FILL ( -- )
   1 STALLING !
   FILLED TASK:WAIT ;

: FILLING ( HTTP:request HTTP:response -- )
   [: NOTE ;] [: AWAIT-FILL ;] WS-TEST:ACCEPT-STALLED KEEP
   ACCEPTED TASK:SIGNAL ;

: PLAIN ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   answer 200 HTTP:STATUS!
   answer s" plain" HTTP:BODY! ;

: INSTALL-ROUTES ( -- )
   HTTP:ROUTES-RESET
   s" GET" s" /ws" [: OPEN ;] HTTP:ROUTE
   s" POST" s" /ws" [: OPEN ;] HTTP:ROUTE
   s" GET" s" /twice" [: TWICE ;] HTTP:ROUTE
   s" GET" s" /stall" [: STALLED ;] HTTP:ROUTE
   s" GET" s" /fill" [: FILLING ;] HTTP:ROUTE
   s" GET" s" /plain" [: PLAIN ;] HTTP:ROUTE ;


\ ---- the cases ---------------------------------------------------------------

\ The socket's handler is told `closed` with this status within that many ms.
: CLOSES-WITHIN ( WS:socket n n -- ) {: sock:WS:socket code:n limit:n :}
   sock 0 WATCH !
   [: WATCHED ;] limit AWAITED code T= ;

\ The server fails the connection with this status and ends the stream, and
\ the socket's handler is told so.
: FAILED ( TCP4:connection WS:socket n -- ) {: conn:TCP4:connection sock:WS:socket code:n :}
   conn HEAR code GOT-CLOSE
   conn ENDS
   sock code ANSWER-MS CLOSES-WITHIN ;

\ The socket of the handshake the route has just accepted.
: OPENED ( TCP4:connection -- TCP4:connection WS:socket )
   ACCEPTED TASK:WAIT
   0 HELD @ ;

\ A handshake on /ws that passed: the client's connection and its socket.
: OPENS-OVER ( TCP4:connection -- TCP4:connection WS:socket )
   s" /ws" UPGRADE-OVER OPENED ;

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

\ RFC 6455 section 1.3's key and the accept value it names. A client that ends
\ its stream with no close frame is reported as status 1006.
: HANDSHAKE-CASE ( -- )
   s" handshake" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   HEAD$ S\" \r\nSec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n" CONTAINS? TTRUE
   HEAD$ S\" \r\nUpgrade: websocket\r\n" CONTAINS? TTRUE
   HEAD$ S\" \r\nConnection: Upgrade\r\n" CONTAINS? TTRUE
   HEAD$ s" Content-Length" CONTAINS? TFALSE
   LOG-HEAD
   conn TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   conn ENDS
   sock LOST ANSWER-MS CLOSES-WITHIN ;

\ The status line a refused handshake is answered with, and the status the
\ route was told it was refused with: 0 when the server refused the request
\ before any route ran.
: REFUSED ( ptr u8 n ptr u8 n n -- ) {: label lu:n want wu:n status:n :}
   label lu T-LABEL
   0 REFUSED-WITH !
   S\" \r\n" REQ+
   REQ-SEND {: conn:TCP4:connection :}
   status 0 <> if ACCEPTED TASK:WAIT then
   HEAD$ want wu STARTS-WITH? TTRUE
   label lu LOG s" : " LOG HEAD$ FIRST-LINE LOG-LINE
   conn TCP4:CLOSE DROP-STATUS
   REFUSED-WITH @ status T= ;

: BAD$ ( -- ptr u8 n )
   s" HTTP/1.1 400 Bad Request" ;

\ Section 4.2.1's first two items: the handshake is an HTTP/1.1 GET carrying
\ one valid Host. The server refuses a request without one before any route
\ runs.
: REQUEST-REFUSALS ( -- )
   s" POST" s" /ws" REQ-START GOOD-LINES
   s" POST" BAD$ 400 REFUSED
   s" GET" s" /ws" s" HTTP/1.0" REQ-BARE HOST+ GOOD-LINES
   s" HTTP/1.0" BAD$ 400 REFUSED
   s" GET" s" /ws" s" HTTP/1.1" REQ-BARE GOOD-LINES
   s" no Host" BAD$ 0 REFUSED
   s" GET" s" /ws" REQ-START HOST+ GOOD-LINES
   s" two Hosts" BAD$ 0 REFUSED
   s" GET" s" /ws" s" HTTP/1.1" REQ-BARE s" Host" s" " LINE+ GOOD-LINES
   s" an empty Host" BAD$ 0 REFUSED
   s" GET" s" /ws" s" HTTP/1.1" REQ-BARE s" Host" s" a b" LINE+ GOOD-LINES
   s" a Host with a space" BAD$ 0 REFUSED ;

: REFUSAL-CASES ( -- )
   s" refused handshakes" SECTION
   s" GET" s" /ws" REQ-START
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" no Upgrade" BAD$ 400 REFUSED
   s" GET" s" /ws" REQ-START
   s" Upgrade" s" h2c" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" Upgrade to h2c" BAD$ 400 REFUSED
   s" GET" s" /ws" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" close" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" Connection without Upgrade" BAD$ 400 REFUSED
   s" GET" s" /ws" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" no key" BAD$ 400 REFUSED
   s" GET" s" /ws" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" s" dGhlIHNhbXBsZSBub25jZQ=" LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" a key of 23 characters" BAD$ 400 REFUSED
   s" GET" s" /ws" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" s" dGhlIHNhbXBsZSBub25jZQAA" LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" a key of 18 bytes" BAD$ 400 REFUSED
   s" GET" s" /ws" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" s" dGhlIHNhbXBsZSBub25jZ!==" LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+
   s" a key outside base64" BAD$ 400 REFUSED
   s" GET" s" /ws" REQ-START GOOD-LINES
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" two keys" BAD$ 400 REFUSED
   REQUEST-REFUSALS
   s" GET" s" /ws" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" no version" s" HTTP/1.1 426 Upgrade Required" 426 REFUSED
   s" GET" s" /ws" REQ-START
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 12" LINE+
   s" version 12" s" HTTP/1.1 426 Upgrade Required" 426 REFUSED
   s" a 426 names the version and the protocol" T-LABEL
   HEAD$ S\" \r\nSec-WebSocket-Version: 13\r\n" CONTAINS? TTRUE
   HEAD$ S\" \r\nUpgrade: websocket\r\n" CONTAINS? TTRUE ;

\ Header names and the two tokens are matched whatever their case, and the
\ Connection header is a list.
: TOLERANT-CASE ( -- )
   s" a handshake in another spelling" SECTION
   s" GET" s" /ws" REQ-START
   s" upgrade" s" WebSocket" LINE+
   s" connection" s" keep-alive, Upgrade" LINE+
   s" sec-websocket-key" KEY$ LINE+
   s" sec-websocket-version" s" 13" LINE+
   S\" \r\n" REQ+
   REQ-SEND OPENED drop {: conn:TCP4:connection :}
   HEAD$ s" HTTP/1.1 101 " STARTS-WITH? TTRUE
   conn WS-OPCODE:text s" ok" EXCHANGE
   conn FAREWELL ;

\ Text, empty or not, and binary reach the handler as the messages they are,
\ and its replies come back as such.
: ECHO-CASE ( -- )
   s" text and binary reach the handler" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn WS-OPCODE:text s" hello" EXCHANGE
   conn WS-OPCODE:text s" hello" drop 0 EXCHANGE
   conn WS-OPCODE:text s" 68c3a96c6c6f20e282ac" BYTES EXCHANGE
   conn WS-OPCODE:binary 5 PATTERN$ EXCHANGE
   conn FAREWELL
   sock 1000 ANSWER-MS CLOSES-WITHIN ;

\ The handler's text that is not UTF-8 is refused before a byte of it goes
\ out: the first frame the client hears is the echo of its next text.
: BAD-TEXT-CASE ( -- )
   s" text the server may not send" SECTION
   0 BAD-TEXT atomic!
   OPENS drop {: conn:TCP4:connection :}
   conn true WS-OPCODE:text s" badtext" SAY
   conn WS-OPCODE:text s" ok" EXCHANGE
   conn FAREWELL
   s" text that is not UTF-8 is refused" T-LABEL
   BAD-TEXT atomic@ E-WS-TEXT T= ;

\ Each length form at its edges: 7 bits to 125, 16 bits from 126 to 65535, and
\ 64 bits past that.
: LENGTH-CASES ( -- )
   s" echo binary in each length form" SECTION
   OPENS drop {: conn:TCP4:connection :}
   conn WS-OPCODE:binary 125 PATTERN$ EXCHANGE
   conn WS-OPCODE:binary 126 PATTERN$ EXCHANGE
   conn WS-OPCODE:binary 65535 PATTERN$ EXCHANGE
   conn WS-OPCODE:binary 65536 PATTERN$ EXCHANGE
   conn WS-OPCODE:binary BIG PATTERN$ EXCHANGE
   conn FAREWELL ;

\ A message in fragments comes back as one frame: control frames pass between
\ the fragments, a fragment may be empty, and a scalar may straddle two.
: FRAGMENT-CASES ( -- )
   s" fragments" SECTION
   OPENS drop {: conn:TCP4:connection :}
   conn false WS-OPCODE:text s" Hel" SAY
   conn false WS-OPCODE:continuation s" lo," SAY
   conn true WS-OPCODE:continuation s"  world" SAY
   conn HEAR WS-OPCODE:text s" Hello, world" GOT
   conn false WS-OPCODE:binary s" 0102" BYTES SAY
   conn true WS-OPCODE:ping s" !" SAY
   conn HEAR WS-OPCODE:pong s" !" GOT
   conn true WS-OPCODE:continuation s" 0304" BYTES SAY
   conn HEAR WS-OPCODE:binary s" 01020304" BYTES GOT
   conn false WS-OPCODE:text s" c3" BYTES SAY
   conn true WS-OPCODE:continuation s" a9" BYTES SAY
   conn HEAR WS-OPCODE:text s" c3a9" BYTES GOT
   conn false WS-OPCODE:text s" hi" SAY
   conn true WS-OPCODE:continuation s" hi" drop 0 SAY
   conn HEAR WS-OPCODE:text s" hi" GOT
   conn FAREWELL ;

\ A ping is answered with its own payload, to the 125 bytes a control frame may
\ carry, and a pong nobody asked for is let pass.
: PING-CASES ( -- )
   s" ping and pong" SECTION
   OPENS drop {: conn:TCP4:connection :}
   conn true WS-OPCODE:ping s" hb" SAY
   conn HEAR WS-OPCODE:pong s" hb" GOT
   conn true WS-OPCODE:ping s" hb" drop 0 SAY
   conn HEAR WS-OPCODE:pong s" hb" drop 0 GOT
   conn true WS-OPCODE:ping 125 PATTERN$ SAY
   conn HEAR WS-OPCODE:pong 125 PATTERN$ GOT
   conn true WS-OPCODE:pong s" x" SAY
   conn WS-OPCODE:text s" ok" EXCHANGE
   conn FAREWELL ;

\ A client that does not wait for the 101: the first AT bytes of its frame ride
\ behind the request, where the HTTP server has already read them, and it
\ writes the rest once the 101 is in. The frame arrives whole wherever the
\ cut falls.
: EARLY ( ptr u8 n n -- ) {: a u:n at:n :}
   s" GET" s" /ws" REQ-START GOOD-LINES S\" \r\n" REQ+
   s" > " true WS-OPCODE:text a u LOG-FRAME
   true WS-OPCODE:text a u FRAME$ {: f fu:n :}
   f at REQ+
   REQ-SEND OPENED drop {: conn:TCP4:connection :}
   HEAD$ s" HTTP/1.1 101 " STARTS-WITH? TTRUE
   at fu < if conn f at + fu at - SEND-RAW then
   conn HEAR WS-OPCODE:text a u GOT
   conn FAREWELL ;

6 constant SHORT-HEAD              \ a masked header under 126 payload bytes: 2 bytes and the mask key

: EARLY-CASE ( -- )
   s" a frame behind the handshake" SECTION
   s" early" dup SHORT-HEAD + EARLY
   s" a frame whose payload the handshake cut" T-LABEL
   s" early payload" SHORT-HEAD 2 + EARLY
   s" a frame whose header the handshake cut" T-LABEL
   s" early header" 1 EARLY ;

\ Raw bytes on a fresh socket, and the status the server fails it with.
: BREAKS ( ptr u8 n ptr u8 n n -- ) {: label lu:n a u:n code:n :}
   label lu SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn a u SAY-RAW
   conn sock code FAILED ;

\ What the codec refuses in a header, each from the fewest bytes that show it.
: HEADER-FAULTS ( -- )
   s" an unmasked frame" s" 810548656c6c6f" 1002 BREAKS
   s" a control frame past 125 bytes" s" 89fe" 1002 BREAKS
   s" a fragmented control frame" s" 0980" 1002 BREAKS
   s" a reserved bit" s" c180" 1002 BREAKS
   s" a reserved opcode" s" 8380" 1002 BREAKS
   s" a length in a longer form than it needs" s" 82fe007d" 1002 BREAKS
   s" a message one byte past the bound" s" 82ff0000000000100001" 1009 BREAKS ;

\ What only the connection can see: the order of the frames and what they say.
: MESSAGE-FAULTS ( -- )
   s" a continuation of nothing" s" 808037fa213d" 1002 BREAKS
   s" a new message inside another" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn false WS-OPCODE:text s" a" SAY
   conn true WS-OPCODE:text s" b" SAY
   conn sock 1002 FAILED
   s" text that is not UTF-8" SECTION
   OPENS {: bad:TCP4:connection bad-sock:WS:socket :}
   bad true WS-OPCODE:text s" c328" BYTES SAY
   bad bad-sock 1007 FAILED
   s" fragments that are not UTF-8 together" SECTION
   OPENS {: split:TCP4:connection split-sock:WS:socket :}
   split false WS-OPCODE:text s" c3" BYTES SAY
   split true WS-OPCODE:continuation s" 28" BYTES SAY
   split split-sock 1007 FAILED
   s" fragments one byte past the bound" SECTION
   OPENS {: long:TCP4:connection long-sock:WS:socket :}
   long false WS-OPCODE:binary BIG PATTERN$ SAY
   long s" 80ff00000000000eee9137fa213d" SAY-RAW
   long long-sock 1009 FAILED ;

\ A frame whose header is accepted and whose payload never comes: the client
\ ends its stream, so the handler is told 1006 where a refused length would
\ have been 1009.
: BOUND-CASES ( -- )
   s" a message of exactly the bound" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn s" 82ff000000000010000037fa213d" SAY-RAW
   conn TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   conn ENDS
   sock LOST ANSWER-MS CLOSES-WITHIN
   s" fragments of exactly the bound" SECTION
   OPENS {: long:TCP4:connection long-sock:WS:socket :}
   long false WS-OPCODE:binary BIG PATTERN$ SAY
   long s" 80ff00000000000eee9037fa213d" SAY-RAW
   long TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   long ENDS
   long-sock LOST ANSWER-MS CLOSES-WITHIN ;

\ A close frame's status comes back as it was sent when a frame may carry it,
\ and is a protocol error when none may; the handler is told the status the
\ server closed under.
: CLOSES-WITH ( n n -- ) {: code:n answer:n :}
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:close code STATUS$ SAY
   conn sock answer FAILED ;

: CLOSE-STATUS-CASES ( -- )
   s" close statuses a frame may carry" SECTION
   1000 1000 CLOSES-WITH
   1003 1003 CLOSES-WITH
   1007 1007 CLOSES-WITH
   1014 1014 CLOSES-WITH
   3000 3000 CLOSES-WITH
   4999 4999 CLOSES-WITH
   s" close statuses no frame may carry" SECTION
   999 1002 CLOSES-WITH
   1004 1002 CLOSES-WITH
   1005 1002 CLOSES-WITH
   1006 1002 CLOSES-WITH
   1015 1002 CLOSES-WITH
   2999 1002 CLOSES-WITH
   5000 1002 CLOSES-WITH ;

: CLOSE-CASES ( -- )
   s" a close with no status" SECTION
   OPENS {: bare:TCP4:connection bare-sock:WS:socket :}
   bare true WS-OPCODE:close s" x" drop 0 SAY
   bare HEAR WS-OPCODE:close s" x" drop 0 GOT
   bare ENDS
   bare-sock 1005 ANSWER-MS CLOSES-WITHIN
   s" a close with a reason" SECTION
   OPENS {: reason:TCP4:connection reason-sock:WS:socket :}
   reason true WS-OPCODE:close s" 03e9627965" BYTES SAY
   reason reason-sock 1001 FAILED
   s" a close with half a status" SECTION
   OPENS {: half:TCP4:connection half-sock:WS:socket :}
   half true WS-OPCODE:close s" 03" BYTES SAY
   half half-sock 1002 FAILED
   s" a close whose reason is not UTF-8" SECTION
   OPENS {: garbled:TCP4:connection garbled-sock:WS:socket :}
   garbled true WS-OPCODE:close s" 03e8c328" BYTES SAY
   garbled garbled-sock 1007 FAILED ;

\ The handler closes first: a ping the client sends before it answers the close
\ is still answered (section 5.5.2), then the client answers the close and the
\ stream ends.
: SERVER-CLOSE-CASE ( -- )
   s" the server closes first" SECTION
   0 BAD-CODE atomic! 0 LATE-SEND atomic!
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:text s" go" SAY
   conn HEAR 1000 GOT-CLOSE
   conn true WS-OPCODE:ping s" x" SAY
   conn HEAR WS-OPCODE:pong s" x" GOT
   conn true WS-OPCODE:close 1000 STATUS$ SAY
   conn ENDS
   s" the handler saw the client's close" T-LABEL
   sock 1000 ANSWER-MS CLOSES-WITHIN
   s" status 1005 is not one to send" T-LABEL
   BAD-CODE atomic@ E-WS-CODE T=
   s" nothing is sent behind a close" T-LABEL
   LATE-SEND atomic@ E-WS-CLOSED T= ;

: ALL-CLOSED ( -- n )
   CLOSED-N atomic@ CLOSE-GOAL @ >= if 1 else 0 then ;

\ A socket in every slot START's limit allows, their connections in CONNS.
: OPEN-ALL ( -- )
   CROWD 0 ?do OPENS drop i CONNS ! loop ;

\ Every connection OPEN-ALL made closed, and each socket's close awaited.
: CLOSE-ALL ( -- )
   CLOSED-N @ CROWD + CLOSE-GOAL !
   CROWD 0 ?do i CONNS @ TCP4:CLOSE DROP-STATUS loop
   [: ALL-CLOSED ;] ANSWER-MS AWAITED drop ;

: TWICE-CASE ( -- )
   s" one request accepted twice" SECTION
   0 SECOND-ACCEPT !
   s" /twice" UPGRADE OPENED {: conn:TCP4:connection sock:WS:socket :}
   conn WS-OPCODE:text s" still here" EXCHANGE
   SECOND-ACCEPT @ HTTP:E-STATE T=
   conn FAREWELL
   sock 1000 ANSWER-MS CLOSES-WITHIN
   s" the refused accept keeps no slot" T-LABEL
   OPEN-ALL CLOSE-ALL ;

\ WS reports each handler fault on fd 2. For THROW-CASE fd 2 is a file, read
\ back after it; the process's own stderr waits on a saved descriptor and
\ comes back however the case ends.
: ERR-PATH$ ( -- ptr u8 n )
   s" build/ws-stderr.txt" ;

: ERR$ ( -- ptr u8 n )
   ERR-BUF ERR-U @ SPAN:TAKE SPAN:$ ;

: ERR-WANT$ ( -- ptr u8 n )
   ERR-WANT ERR-WANT-U @ SPAN:TAKE SPAN:$ ;

: ERR-W ( ptr u8 n -- ) {: a u:n :}
   a u ERR-WANT ERR-WANT-U @ SPAN:SKIP SPAN:COPY
   u ERR-WANT-U +! ;

\ Ends the expected line in the builder with the throw code and adds it to the
\ stderr WS must write.
: ERR-THROW ( n -- )
   FMT:SB-INT
   LF SB-APPEND-C
   SB$ ERR-W ;

\ The line WS writes when this socket's handler throws E-HANDLER.
: FAULT-LINE ( WS:socket -- ) {: sock:WS:socket :}
   SB-RESET
   s" ws: socket " SB-APPEND
   sock SOCK>N FMT:SB-U
   s"  failed, throw " SB-APPEND
   E-HANDLER ERR-THROW ;

: SAVE-STDERR ( -- )
   FD:STDERR FD>N F-DUPFD SAVE-FD-MIN fcntl {: saved:n :}
   saved 0 < if E-SILENT throw then
   saved SAVED-STDERR ! ;

: STDERR-TO-FILE ( -- )
   ERR-PATH$ s" " FS:WRITE-ALL
   ERR-PATH$ FS:OPEN-APPEND-FD {: file:n :}
   file FD:STDERR FD>N dup2 {: moved:n :}
   file close-rc drop
   moved 0 < if E-SILENT throw then ;

: RESTORE-STDERR ( -- )
   SAVED-STDERR @ FD:STDERR FD>N dup2 {: moved:n :}
   SAVED-STDERR @ close-rc drop
   moved 0 < if E-SILENT throw then ;

: ERR-WRITTEN ( -- n )
   ERR-PATH$ FS:FILE-SIZE ERR-WANT-U @ >= if 1 else 0 then ;

: THROWING ( -- )
   STDERR-TO-FILE
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   sock FAULT-LINE
   conn true WS-OPCODE:text s" throw" SAY
   conn sock 1011 FAILED
   s" a `closed` handler that throws" T-LABEL
   OPENS {: gone:TCP4:connection gone-sock:WS:socket :}
   gone-sock FAULT-LINE
   1 THROW-ARMED !
   gone TCP4:CLOSE DROP-STATUS
   gone-sock LOST ANSWER-MS CLOSES-WITHIN
   [: ERR-WRITTEN ;] ANSWER-MS AWAITED drop ;

\ A handler that throws closes its socket under 1011, and one that throws on
\ `closed` has no socket left to close: each throw is one line on stderr with
\ the socket's handle and the code.
: THROW-CASE ( -- )
   s" a throwing handler" SECTION
   s" build" FS:MAKE-DIRS
   0 ERR-WANT-U !
   SAVE-STDERR
   [: THROWING ;] [: RESTORE-STDERR ;] finally
   s" each fault reported" T-LABEL
   ERR-PATH$ ERR-BUF SPAN:$ FS:READ-ALL ERR-U !
   ERR$ ERR-WANT$ T$=
   ERR-PATH$ FS:REMOVE-FILE ;

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

: UNIFORM? ( ptr u8 n -- bool ) {: a u:n :}
   u 0 ?do a i + c@ a c@ <> if false unloop exit then loop
   true ;

\ The frame in hand is one this task pushed or one the handler sent, whole.
: TALLY ( -- )
   RX$ {: a u:n :}
   RX-EOF @ 0 <> u BLAST-BYTES <> or if 1 TORN +! exit then
   a u UNIFORM? 0= if 1 TORN +! exit then
   a c@ PUSH-BYTE = if 1 PUSHED-N +! exit then
   a c@ HANDLER-BYTE = if 1 HANDLER-N +! exit then
   1 TORN +! ;

\ This task pushes through a socket while its handler sends frames of its own.
\ Each send queues its frame whole, so every frame the client reads is one of
\ the two tasks', whole, however their sends interleave.
: BOTH-CASE ( -- )
   s" two tasks sending at once" SECTION
   0 PUSHED-N ! 0 HANDLER-N ! 0 TORN !
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:text s" blast" SAY
   BLAST-N 0 ?do sock PUSH-BUF SPAN:$ WS:SEND-BINARY loop
   BLAST-N 2 * 0 ?do conn NEXT-FRAME TALLY loop
   s" every frame of both tasks arrived whole" T-LABEL
   PUSHED-N @ BLAST-N T=
   HANDLER-N @ BLAST-N T=
   TORN @ 0 T=
   s" < " LOG PUSHED-N @ LOG-N s"  pushed frames and " LOG HANDLER-N @ LOG-N
   s"  of the handler's, " LOG TORN @ LOG-N s"  torn" LOG-LINE
   conn FAREWELL ;

\ BURST-PINGS empty pings, as the client sends them.
: VOLLEY-FILL ( -- )
   true WS-OPCODE:ping PATTERN SPAN:$ drop 0 FRAME$ {: fa fu:n :}
   BURST-PINGS 0 ?do fa fu VOLLEY-BUF i fu * SPAN:SKIP SPAN:COPY loop ;

: PONG-IN ( -- )
   RX-EOF @ 0 <> if exit then
   RX-OP @ OP$ s" pong" STR= RX-LEN @ 0= and if 1 PONGS +! then ;

\ Pings that arrive many to a read are each answered: the client writes
\ BURST-PINGS of them at once and reads a pong for every one.
: BURST-CASE ( -- )
   s" a burst of pings" SECTION
   0 PONGS !
   OPENS drop {: conn:TCP4:connection :}
   VOLLEY-FILL
   conn VOLLEY-BUF SPAN:$ SEND-RAW
   BURST-PINGS 0 ?do conn NEXT-FRAME PONG-IN loop
   s" < " LOG PONGS @ LOG-N s"  pongs" LOG-LINE
   PONGS @ BURST-PINGS T=
   conn FAREWELL ;

\ A handle kept past its socket names nothing, even once its slot serves the
\ next socket: a send through it is refused, a close through it is nothing
\ new, and the next socket hears only its own frames.
: HELD-CASE ( -- )
   s" a handle kept past its socket" SECTION
   OPENS {: held:TCP4:connection gone:WS:socket :}
   held FAREWELL
   gone 1000 ANSWER-MS CLOSES-WITHIN
   OPENS drop {: conn:TCP4:connection :}
   conn WS-OPCODE:text s" mine" EXCHANGE
   gone 0 PUSHED-TO !
   s" a send through it is refused" T-LABEL
   [: s" late" PUSH-ONE 2drop ;] E-WS-CLOSED TTHROWSQ
   s" and a close through it is nothing new" T-LABEL
   gone 1000 WS:CLOSE
   conn WS-OPCODE:text s" only mine" EXCHANGE
   conn FAREWELL ;

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
      code 0 <> if code LOST T= exit then
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

\ The socket past START's limit is refused 503, and no socket exists for it;
\ the body read behind its head is dropped with its connection.
\ The last socket within it, slot $100, past a one-byte slot field, echoes on
\ its own connection.
: CAPACITY-CASE ( -- )
   s" the socket past capacity" SECTION
   OPEN-ALL
   s" GET" s" /ws" REQ-START GOOD-LINES S\" \r\n" REQ+
   REQ-SEND {: conn:TCP4:connection :}
   ACCEPTED TASK:WAIT
   HEAD$ s" HTTP/1.1 503 " STARTS-WITH? TTRUE
   REFUSED-WITH @ 503 T=
   conn TCP4:CLOSE DROP-STATUS
   0 STREAM-AT ! 0 STREAM-U !
   CROWD 1- CONNS @ WS-OPCODE:text s" last" EXCHANGE
   CLOSE-ALL ;

\ WS:STOP sends 1001 to an open socket, tells its handler, and returns within
\ its bound; a stalled peer's socket, whose 1001 waits behind its queue, is
\ given up under 1006 and its handler told so before STOP returns.
: STOP-CASE ( -- )
   s" a stop" SECTION
   DIAL-SMALL OPENS-OVER {: still:TCP4:connection stuck:WS:socket :}
   stuck STALL-PUSH PUSH-BYTES / PUSHES 0 T=
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   mono-ns {: began:n :}
   WS:STOP
   stuck CLOSED-OF {: told:n :}
   mono-ns began - STOP-BOUND-MS GRACE-MS + NS-PER-MS * < TTRUE
   conn HEAR 1001 GOT-CLOSE
   conn ENDS
   sock CLOSED-OF 1001 T=
   s" and the stalled peer's handler was told before it returned" T-LABEL
   told LOST T=
   still TCP4:CLOSE DROP-STATUS ;

: SEND-STALE ( -- )
   0 HELD @ s" stale" WS:SEND-TEXT ;

\ Once WS has stopped, a send through a socket kept from before the stop is
\ E-WS-CLOSED and writes nothing to a descriptor the stop gave back, the wake
\ pipe's among them, now another file's.
: STALE-CASE ( -- )
   s" a send after the stop" T-LABEL
   STRAY$ s" " FS:WRITE-ALL
   STRAYS 0 ?do STRAY$ FS:OPEN-APPEND-FD i STRAY-FD ! loop
   [: SEND-STALE ;] E-WS-CLOSED TTHROWSQ
   STRAY$ FS:FILE-SIZE 0 T=
   STRAYS 0 ?do i STRAY-FD @ close-rc drop loop
   STRAY$ FS:REMOVE-FILE ;

\ With one idle socket open, WS:STOP returns once WS's task has given
\ everything back, never waiting out STOP-BOUND-MS.
: IDLE-STOP-CASE ( -- )
   s" a stop of an idle socket" T-LABEL
   [: 0 WS:START ;] E-WS-LIMIT TTHROWSQ
   [: $100000000 WS:START ;] E-WS-LIMIT TTHROWSQ
   CROWD WS:START
   OPENS drop {: conn:TCP4:connection :}
   mono-ns {: began:n :}
   WS:STOP
   mono-ns began - IDLE-STOP-MS NS-PER-MS * < TTRUE
   conn TCP4:CLOSE DROP-STATUS ;

\ A `closed` handler that blocks until WS:STOP kills WS's task leaves its slot
\ retired, so the task's cleanup does not close the connection again: the
\ file the handler opened on that descriptor number stays open. The kill
\ closes the other socket, whose 1001 the blocked handler kept from being
\ written: its peer reads the end of the stream.
: HALT-CASE ( -- )
   s" a stop that kills a blocked handler" SECTION
   HALTED$ s" " FS:WRITE-ALL
   0 HALT-FD !
   CROWD WS:START
   OPENS drop {: conn:TCP4:connection :}
   OPENS drop {: other:TCP4:connection :}
   1 HALT-ARMED !
   WS:STOP
   HALT-FD @ ONE-BYTE 1 write 1 T=
   HALTED$ FS:FILE-SIZE 1 T=
   HALT-FD @ close-rc drop
   other ENDS
   conn TCP4:CLOSE DROP-STATUS
   HALTED$ FS:REMOVE-FILE ;


: TASK-CODE ( -- n )
   STOP-TASK TASK:JOIN MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH ;

\ A stop leaves a socket that is already closing its own status: its peer
\ reads the 4000 the handler sent, never a 1001, and the answer it sends once
\ the stop has reached WS's task (an open socket's 1001 shows that) still
\ reaches the handler. WS:STOP runs on its own task while this one answers.
: CLOSING-STOP-CASE ( -- )
   s" a stop while a socket closes" SECTION
   CROWD WS:START
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   OPENS drop {: other:TCP4:connection :}
   conn true WS-OPCODE:text s" bye" SAY
   conn HEAR 4000 GOT-CLOSE
   [: WS:STOP 0 TASK:RETURN ;] STOP-TASK TASK:ACTIVATE
   other HEAR 1001 GOT-CLOSE
   conn true WS-OPCODE:close 4000 STATUS$ SAY
   conn ENDS
   other ENDS
   TASK-CODE 0 T=
   s" the closing socket took its peer's answer" T-LABEL
   sock CLOSED-OF 4000 T= ;

: CLOSE-STALE ( -- )
   0 HELD @ 1000 WS:CLOSE ;

\ A restart with a smaller limit leaves a socket kept from before it, in a slot
\ past the new limit, gone: a send through it is E-WS-CLOSED, and a close leaves
\ it as it is.
: SHRINK-CASE ( -- )
   s" a restart with a smaller limit" T-LABEL
   2 WS:START
   OPENS drop {: first:TCP4:connection :}
   OPENS drop {: second:TCP4:connection :}
   WS:STOP
   first TCP4:CLOSE DROP-STATUS
   second TCP4:CLOSE DROP-STATUS
   1 WS:START
   [: SEND-STALE ;] E-WS-CLOSED TTHROWSQ
   [: CLOSE-STALE ;] catch 0 T=
   WS:STOP ;

\ WS started for two sockets, one of them open, and a handshake on /stall that
\ holds the other's slot: the open socket's connection, then the handshake's.
: HOLD-SLOT ( -- TCP4:connection TCP4:connection )
   2 WS:START
   OPENS drop {: first:TCP4:connection :}
   0 STALLING !
   0 REFUSED-WITH !
   s" GET" s" /stall" REQ-START GOOD-LINES S\" \r\n" REQ+
   DIAL {: conn:TCP4:connection :}
   conn REQ$ SEND-RAW
   [: STALLING @ ;] ANSWER-MS AWAITED drop
   first conn ;

\ A handshake that holds a slot past a stop's bound is answered 503 by that
\ stop, which waits for it: it never joins the next run, here one whose limit
\ leaves its slot out.
: STRAGGLER-CASE ( -- )
   s" a stop while a handshake holds its slot" T-LABEL
   HOLD-SLOT {: first:TCP4:connection conn:TCP4:connection :}
   WS:STOP
   1 WS:START
   ACCEPTED TASK:WAIT
   REFUSED-WITH @ 503 T=
   conn READ-HEAD
   HEAD$ s" HTTP/1.1 503 " STARTS-WITH? TTRUE
   conn TCP4:CLOSE DROP-STATUS
   first TCP4:CLOSE DROP-STATUS
   0 STREAM-AT ! 0 STREAM-U !
   WS:STOP ;

: STOP-ENDED ( -- n )
   STOP-TASK TASK:DONE? if 1 else 0 then ;

\ A second WS:STOP while the first waits for a handshake that holds its slot
\ is E-WS-STATE, and the first returns once that handshake is refused. The
\ first stop runs on its own task; the open socket's 1001 shows it has begun.
: STOP-TWICE-CASE ( -- )
   s" a second stop while one runs" SECTION
   HOLD-SLOT {: first:TCP4:connection conn:TCP4:connection :}
   [: WS:STOP 0 TASK:RETURN ;] STOP-TASK TASK:ACTIVATE
   first HEAR 1001 GOT-CLOSE
   [: WS:STOP ;] E-WS-STATE TTHROWSQ
   ACCEPTED TASK:WAIT
   REFUSED-WITH @ 503 T=
   [: STOP-ENDED ;] ANSWER-MS AWAITED drop
   TASK-CODE 0 T=
   conn TCP4:CLOSE DROP-STATUS
   first TCP4:CLOSE DROP-STATUS
   0 STREAM-AT ! 0 STREAM-U ! ;

\ STOP-TASK's START, once this task lets it through the gate.
: GATED-START ( -- )
   1 GATE-READY atomic!
   begin GATE-OPEN atomic@ 0 <> until
   [: 2 WS:START ;] catch TASK:RETURN ;

\ Two STARTs let go together: one returns 0 and the other is E-WS-STATE, and
\ the run the first began stops.
: START-RACE-CASE ( -- )
   s" two starts at once" T-LABEL
   0 GATE-READY atomic!
   0 GATE-OPEN atomic!
   [: GATED-START ;] STOP-TASK TASK:ACTIVATE
   [: GATE-READY atomic@ ;] ANSWER-MS AWAITED drop
   1 GATE-OPEN atomic!
   [: 2 WS:START ;] catch {: here:n :}
   [: STOP-ENDED ;] ANSWER-MS AWAITED drop
   TASK-CODE {: there:n :}
   here there max 0 T=
   here there min E-WS-STATE T=
   [: WS:STOP ;] catch 0 T= ;

\ A handler's WS:STOP, on WS's own task, is E-WS-STATE and leaves WS serving:
\ this task's stop then closes the socket under 1001, and a new run starts and
\ stops.
: HANDLER-STOP-CASE ( -- )
   s" a stop from a handler" SECTION
   0 HANDLER-STOP atomic!
   2 WS:START
   OPENS drop {: conn:TCP4:connection :}
   conn true WS-OPCODE:text s" stop" SAY
   s" a handler's stop is refused" T-LABEL
   [: HANDLER-STOP atomic@ ;] ANSWER-MS AWAITED E-WS-STATE T=
   [: WS:STOP ;] catch 0 T=
   conn HEAR 1001 GOT-CLOSE
   conn ENDS
   [: 1 WS:START ;] catch 0 T=
   [: WS:STOP ;] catch 0 T= ;

: FILL-ONE ( -- )
   FILL-N @ {: idx:n :}
   idx FILL-R @ >FD AIO:READABLE -1 >MS AIO:POLL idx FILL-TICKET !
   1 FILL-N +! ;

\ Polls that never fire, each on its own pipe, until AIO refuses one at its
\ ceiling: Darwin's poll bound, or Linux's records. The first is on HTTP's
\ listener, which keeps that descriptor counted while the listener's own poll
\ times out and is made again, so only WS's task meets the bound.
: FILL ( -- )
   WS-TEST:LISTEN-FD AIO:READABLE -1 >MS AIO:POLL 0 PIN-TICKET !
   true PINNED !
   begin
      FILL-CODE @ 0= FILL-N @ FILL-CAP < and
   while
      pipe {: r:n w:n rc:n :}
      rc 0 <> if E-SILENT throw then
      r FILL-N @ FILL-R !
      w FILL-N @ FILL-W !
      [: FILL-ONE ;] catch FILL-CODE !
      FILL-CODE @ 0 <> if r close-rc drop w close-rc drop then
   repeat ;

: OUTCOME-GONE ( AIO:outcome -- )
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;

: UNFILL ( -- )
   FILL-N @ 0 ?do
      i FILL-TICKET @ {: t:AIO:ticket :}
      t AIO:CANCEL
      t AIO:AWAIT OUTCOME-GONE
      i FILL-R @ close-rc drop
      i FILL-W @ close-rc drop
   loop
   0 FILL-N !
   PINNED @ 0= if exit then
   0 PIN-TICKET @ {: t:AIO:ticket :}
   t AIO:CANCEL
   t AIO:AWAIT OUTCOME-GONE
   false PINNED ! ;

\ The line WS writes when its task ends on this code.
: TASK-LINE ( n -- ) {: code:n :}
   SB-RESET
   s" ws: task failed, throw " SB-APPEND
   code ERR-THROW ;

: CEILING ( -- n )
   HB-TARGET-MACOS? if E-AIO-POLL-BOUND else E-AIO-FULL then ;

\ AIO is filled while a handshake holds its slot, so the poll WS's task makes
\ for the new socket is refused.
: FAULTING ( -- )
   STDERR-TO-FILE
   0 STALLING !
   s" GET" s" /fill" REQ-START GOOD-LINES S\" \r\n" REQ+
   DIAL {: conn:TCP4:connection :}
   conn REQ$ SEND-RAW
   [: STALLING @ ;] ANSWER-MS AWAITED drop
   FILL
   s" AIO refuses the poll past its ceiling" T-LABEL
   FILL-CODE @ CEILING T=
   FILL-CODE @ TASK-LINE
   FILLED TASK:SIGNAL
   ACCEPTED TASK:WAIT
   [: ERR-WRITTEN ;] ANSWER-MS AWAITED drop
   UNFILL
   conn TCP4:CLOSE DROP-STATUS
   0 STREAM-AT ! 0 STREAM-U ! ;

\ A throw that ends WS's task, here the poll AIO refuses at its ceiling, is
\ one line on stderr with its code, and WS:STOP still ends the run.
: FAULT-CASE ( -- )
   s" a task that fails" T-LABEL
   s" build" FS:MAKE-DIRS
   0 ERR-WANT-U !
   0 FILL-N !
   0 FILL-CODE !
   false PINNED !
   2 WS:START
   SAVE-STDERR
   [: FAULTING ;] [: UNFILL RESTORE-STDERR ;] finally
   [: WS:STOP ;] catch 0 T=
   s" the task's fault reported once" T-LABEL
   ERR-PATH$ ERR-BUF SPAN:$ FS:READ-ALL ERR-U !
   ERR$ ERR-WANT$ T$=
   ERR-PATH$ FS:REMOVE-FILE ;

: SEND-HELD ( -- )
   0 HELD @ s" wrapped" WS:SEND-TEXT ;

\ A slot's generation wraps within a handle's upper half: the socket after the
\ one that carried the last generation is current, and a send through it is
\ queued.
: WRAP-CASE ( -- )
   s" a slot whose generation wraps" T-LABEL
   1 WS:START
   0 WS-TEST:LAST-GEN
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn TCP4:CLOSE DROP-STATUS
   sock LOST ANSWER-MS CLOSES-WITHIN
   OPENS drop {: next:TCP4:connection :}
   [: SEND-HELD ;] catch 0 T=
   next TCP4:CLOSE DROP-STATUS
   WS:STOP ;


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
   s" > text 4 6c617374" W
   s" < text 4 6c617374" W
   s" == handshake" W
   s" HTTP/1.1 101 Switching Protocols" W
   s" Upgrade: websocket" W
   s" Connection: Upgrade" W
   s" Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" W
   s" " W
   s" < end of stream" W
   s" == refused handshakes" W
   s" no Upgrade: HTTP/1.1 400 Bad Request" W
   s" Upgrade to h2c: HTTP/1.1 400 Bad Request" W
   s" Connection without Upgrade: HTTP/1.1 400 Bad Request" W
   s" no key: HTTP/1.1 400 Bad Request" W
   s" a key of 23 characters: HTTP/1.1 400 Bad Request" W
   s" a key of 18 bytes: HTTP/1.1 400 Bad Request" W
   s" a key outside base64: HTTP/1.1 400 Bad Request" W
   s" two keys: HTTP/1.1 400 Bad Request" W
   s" POST: HTTP/1.1 400 Bad Request" W
   s" HTTP/1.0: HTTP/1.1 400 Bad Request" W
   s" no Host: HTTP/1.1 400 Bad Request" W
   s" two Hosts: HTTP/1.1 400 Bad Request" W
   s" an empty Host: HTTP/1.1 400 Bad Request" W
   s" a Host with a space: HTTP/1.1 400 Bad Request" W
   s" no version: HTTP/1.1 426 Upgrade Required" W
   s" version 12: HTTP/1.1 426 Upgrade Required" W
   s" == a handshake in another spelling" W
   s" > text 2 6f6b" W
   s" < text 2 6f6b" W
   W-FAREWELL
   s" == text and binary reach the handler" W
   s" > text 5 68656c6c6f" W
   s" < text 5 68656c6c6f" W
   s" > text 0" W
   s" < text 0" W
   s" > text 10 68c3a96c6c6f20e282ac" W
   s" < text 10 68c3a96c6c6f20e282ac" W
   s" > binary 5 0726456483" W
   s" < binary 5 0726456483" W
   W-FAREWELL
   s" == text the server may not send" W
   s" > text 7 62616474657874" W
   s" > text 2 6f6b" W
   s" < text 2 6f6b" W
   W-FAREWELL
   s" == echo binary in each length form" W
   s" > binary 125 sum 16101" W
   s" < binary 125 sum 16101" W
   s" > binary 126 sum 16143" W
   s" < binary 126 sum 16143" W
   s" > binary 65535 sum 8355608" W
   s" < binary 65535 sum 8355608" W
   s" > binary 65536 sum 8355840" W
   s" < binary 65536 sum 8355840" W
   s" > binary 70000 sum 8925000" W
   s" < binary 70000 sum 8925000" W
   W-FAREWELL
   s" == fragments" W
   s" > text+ 3 48656c" W
   s" > continuation+ 3 6c6f2c" W
   s" > continuation 6 20776f726c64" W
   s" < text 12 48656c6c6f2c20776f726c64" W
   s" > binary+ 2 0102" W
   s" > ping 1 21" W
   s" < pong 1 21" W
   s" > continuation 2 0304" W
   s" < binary 4 01020304" W
   s" > text+ 1 c3" W
   s" > continuation 1 a9" W
   s" < text 2 c3a9" W
   s" > text+ 2 6869" W
   s" > continuation 0" W
   s" < text 2 6869" W
   W-FAREWELL
   s" == ping and pong" W
   s" > ping 2 6862" W
   s" < pong 2 6862" W
   s" > ping 0" W
   s" < pong 0" W
   s" > ping 125 sum 16101" W
   s" < pong 125 sum 16101" W
   s" > pong 1 78" W
   s" > text 2 6f6b" W
   s" < text 2 6f6b" W
   W-FAREWELL
   s" == a frame behind the handshake" W
   s" > text 5 6561726c79" W
   s" < text 5 6561726c79" W
   W-FAREWELL
   s" > text 13 6561726c79207061796c6f6164" W
   s" < text 13 6561726c79207061796c6f6164" W
   W-FAREWELL
   s" > text 12 6561726c7920686561646572" W
   s" < text 12 6561726c7920686561646572" W
   W-FAREWELL
   s" == an unmasked frame" W
   s" > raw 810548656c6c6f" W
   s" < close 2 03ea" W-FAILED
   s" == a control frame past 125 bytes" W
   s" > raw 89fe" W
   s" < close 2 03ea" W-FAILED
   s" == a fragmented control frame" W
   s" > raw 0980" W
   s" < close 2 03ea" W-FAILED
   s" == a reserved bit" W
   s" > raw c180" W
   s" < close 2 03ea" W-FAILED
   s" == a reserved opcode" W
   s" > raw 8380" W
   s" < close 2 03ea" W-FAILED
   s" == a length in a longer form than it needs" W
   s" > raw 82fe007d" W
   s" < close 2 03ea" W-FAILED
   s" == a message one byte past the bound" W
   s" > raw 82ff0000000000100001" W
   s" < close 2 03f1" W-FAILED
   s" == a continuation of nothing" W
   s" > raw 808037fa213d" W
   s" < close 2 03ea" W-FAILED
   s" == a new message inside another" W
   s" > text+ 1 61" W
   s" > text 1 62" W
   s" < close 2 03ea" W-FAILED
   s" == text that is not UTF-8" W
   s" > text 2 c328" W
   s" < close 2 03ef" W-FAILED
   s" == fragments that are not UTF-8 together" W
   s" > text+ 1 c3" W
   s" > continuation 1 28" W
   s" < close 2 03ef" W-FAILED
   s" == fragments one byte past the bound" W
   s" > binary+ 70000 sum 8925000" W
   s" > raw 80ff00000000000eee9137fa213d" W
   s" < close 2 03f1" W-FAILED
   s" == a message of exactly the bound" W
   s" > raw 82ff000000000010000037fa213d" W
   s" < end of stream" W
   s" == fragments of exactly the bound" W
   s" > binary+ 70000 sum 8925000" W
   s" > raw 80ff00000000000eee9037fa213d" W
   s" < end of stream" W
   s" == close statuses a frame may carry" W
   s" > close 2 03e8" W
   s" < close 2 03e8" W-FAILED
   s" > close 2 03eb" W
   s" < close 2 03eb" W-FAILED
   s" > close 2 03ef" W
   s" < close 2 03ef" W-FAILED
   s" > close 2 03f6" W
   s" < close 2 03f6" W-FAILED
   s" > close 2 0bb8" W
   s" < close 2 0bb8" W-FAILED
   s" > close 2 1387" W
   s" < close 2 1387" W-FAILED
   s" == close statuses no frame may carry" W
   s" > close 2 03e7" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 03ec" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 03ed" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 03ee" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 03f7" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 0bb7" W
   s" < close 2 03ea" W-FAILED
   s" > close 2 1388" W
   s" < close 2 03ea" W-FAILED
   s" == a close with no status" W
   s" > close 0" W
   s" < close 0" W-FAILED
   s" == a close with a reason" W
   s" > close 5 03e9627965" W
   s" < close 2 03e9" W-FAILED
   s" == a close with half a status" W
   s" > close 1 03" W
   s" < close 2 03ea" W-FAILED
   s" == a close whose reason is not UTF-8" W
   s" > close 4 03e8c328" W
   s" < close 2 03ef" W-FAILED
   s" == the server closes first" W
   s" > text 2 676f" W
   s" < close 2 03e8" W
   s" > ping 1 78" W
   s" < pong 1 78" W
   s" > close 2 03e8" W
   s" < end of stream" W
   s" == one request accepted twice" W
   s" > text 10 7374696c6c2068657265" W
   s" < text 10 7374696c6c2068657265" W
   W-FAREWELL
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
   s" == two tasks sending at once" W
   s" > text 5 626c617374" W
   s" < 200 pushed frames and 200 of the handler's, 0 torn" W
   W-FAREWELL
   s" == a burst of pings" W
   s" < 4000 pongs" W
   W-FAREWELL
   s" == a handle kept past its socket" W
   W-FAREWELL
   s" > text 4 6d696e65" W
   s" < text 4 6d696e65" W
   s" > text 9 6f6e6c79206d696e65" W
   s" < text 9 6f6e6c79206d696e65" W
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
   s" < close 2 03e9" W-FAILED
   s" == a stop that kills a blocked handler" W
   s" < end of stream" W
   s" == a stop while a socket closes" W
   s" > text 3 627965" W
   s" < close 2 0fa0" W
   s" < close 2 03e9" W
   s" > close 2 0fa0" W
   s" < end of stream" W
   s" < end of stream" W
   s" == a second stop while one runs" W
   s" < close 2 03e9" W
   s" == a stop from a handler" W
   s" > text 4 73746f70" W
   s" < close 2 03e9" W
   s" < end of stream" W ;

\ The artifact as another reader will find it: read back from its file and
\ compared whole with the transcript this file expects.
: TRANSCRIPT-CASE ( -- )
   s" the transcript" T-LABEL
   s" build" FS:MAKE-DIRS
   s" build/ws-transcript.txt" SCRIPT$ FS:WRITE-ALL
   s" build/ws-transcript.txt" BACK-BUF SPAN:$ FS:READ-ALL {: got:n :}
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

\ The soft descriptor limit raised to FDS-WANT when it is lower: a login
\ shell's soft limit (launchd's 256 on macOS) is under what CROWD holds open.
\ Linux's RLIM_INFINITY reads as -1 here, and is left as it is.
PROCESS-SYMBOLS
FUNCTION: GET-LIMIT getrlimit ( n ptr u8 -- i32 )
   1 16 WRITES-BYTES
;FUNCTION
FUNCTION: SET-LIMIT setrlimit ( n ptr u8 -- i32 ) ;FUNCTION

create FD-LIMIT 16 allot

: NOFILE ( -- n ) HB-TARGET-MACOS? if 8 else 7 then ;

: RAISE-FDS ( -- )
   NOFILE FD-LIMIT GET-LIMIT 0 T=
   FD-LIMIT @ {: soft:n :}
   soft 0< soft FDS-WANT >= or if exit then
   FDS-WANT FD-LIMIT !
   NOFILE FD-LIMIT SET-LIMIT 0 T= ;

: FILL-BUFFERS ( -- )
   BUF-CAP 0 ?do i 31 * 7 + $FF and PATTERN i SPAN:U8! loop
   PUSH-BYTE PUSH-BUF SPAN:FILL
   HANDLER-BYTE HANDLER-BUF SPAN:FILL
   $C3 NOT-UTF8 0 SPAN:U8!
   $28 NOT-UTF8 1 SPAN:U8! ;

\ The table full first, while no other socket holds a slot.
: CASES ( -- )
   CAPACITY-CASE
   HANDSHAKE-CASE REFUSAL-CASES TOLERANT-CASE
   ECHO-CASE BAD-TEXT-CASE LENGTH-CASES FRAGMENT-CASES PING-CASES EARLY-CASE
   HEADER-FAULTS MESSAGE-FAULTS BOUND-CASES
   CLOSE-STATUS-CASES CLOSE-CASES SERVER-CLOSE-CASE TWICE-CASE THROW-CASE
   MANY-CASE PUSH-CASE BOTH-CASE BURST-CASE HELD-CASE OVERFLOW-CASE STALL-CASE ;

public

\ The loop first: every wait the server, WS's task and this client make rides
\ it. WS stops before the server, and the server's stop kills no task: no
\ socket holds a worker.
: RUN ( -- )
   T-RESET
   RAISE-FDS
   0 SCRIPT-U !
   0 CLOSED-N !
   FILL-BUFFERS
   0 ACCEPTED TASK:SEMAPHORE-INIT
   0 FILLED TASK:SEMAPHORE-INIT
   AIO:START
   INSTALL-ROUTES
   LOOPBACK 0 WORKERS IDLE-MS HTTP:START
   CROWD WS:START
   CASES
   STOP-CASE STALE-CASE IDLE-STOP-CASE HALT-CASE CLOSING-STOP-CASE
   SHRINK-CASE STRAGGLER-CASE STOP-TWICE-CASE START-RACE-CASE HANDLER-STOP-CASE
   FAULT-CASE
   WRAP-CASE
   HTTP:STOP
   s" every handler returned" T-LABEL
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   HTTP:KILLED-TASKS 0 T=
   AIO-STOP
   ACCEPTED TASK:SEMAPHORE-DESTROY
   FILLED TASK:SEMAPHORE-DESTROY
   TRANSCRIPT-CASE ;

;package

WS-TEST:RUN
T-REPORT
