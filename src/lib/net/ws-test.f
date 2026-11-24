\ ws-test.f - RFC 6455 connections over a real loopback port: a Habu client on
\ TCP4 shakes hands with src/lib/net/ws.f inside an HTTP server and speaks frames
\ built and read with src/lib/net/ws-frame.f. One handler, NOTE, serves every
\ socket and logs each `closed` with its socket, and RECORD, in front of it,
\ also notes each call of its socket; every handler is defined before the
\ server starts.
\ Run: bin/hb --load src/lib/net/ws-test.f
\
\ Each frame the client sends or reads one at a time is one transcript line:
\ direction, opcode (`+` when more fragments follow), length, and the payload in
\ hex or, past HEX-MAX bytes, as the sum of its bytes. Pushes log no frame. The
\ transcript is written to build/ws-transcript.txt, read back and compared whole
\ with WANT$.

require src/lib/test.f
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
require src/lib/test/stderr.f
require src/lib/test/aio-fill.f
require src/lib/date.f

\ ACCEPT-STALLED is ACCEPT with a stall run once the handshake holds its slot,
\ as a worker the OS keeps off the processor would.
package WS
: WS-TEST:ACCEPT-STALLED ( HTTP:request HTTP:response [ socket n message -- ] n [ -- ] -- accepted )
   {: asked:HTTP:request answer:HTTP:response q arg:n stall :}
   RUNNING? 0= if E-WS-STATE throw then
   HTTP:WORKER-SLOT {: w:n :}
   asked w VERDICT {: status:n :}
   status 0 <> if answer status REFUSE exit then
   RESERVE {: idx:n :}
   idx 0 < if answer 503 REFUSE exit then
   stall execute
   asked answer q arg idx w OPEN-SOCKET ;
: WS-TEST:LAST-GEN ( n -- ) {: idx:n :}
   LOCK TASK:GET
   GEN-MASK idx GEN !
   LOCK TASK:RELEASE ;
: WS-TEST:LEFT ( -- n )
   LOCK TASK:GET
   LEFT @
   LOCK TASK:RELEASE ;
;package

\ Reopens HTTP for its listener's descriptor, which the fault case polls so the
\ listener's own re-polls never free a descriptor under Darwin's poll bound.
package HTTP
: WS-TEST:LISTEN-FD ( -- fd ) LISTENER@ TCP4:LISTENER-FD ;
;package

package WS-TEST

$7F000001 constant LOOPBACK
2 constant WORKERS                \ fewer than the sockets a case holds open at once
$1F4 constant IDLE-MS
10 constant POLL-MS
$3E8 constant GRACE-MS            \ what a bounded wait may run past its bound on a loaded box
60000 constant WAIT-BOUND-MS      \ the longest a wait for a condition polls before its case fails
$190 constant STOP-BOUND-MS       \ how long WS:STOP may take
STOP-BOUND-MS 2 / constant IDLE-STOP-MS \ an idle socket's stop: far over its few ms, under a stop that waits out its bound
$400000 constant QUEUE-MAX        \ the unwritten bytes behind which a frame loses its socket
$10000 constant PUSH-BYTES
$200000 constant STALL-PUSH       \ what a stalled peer's socket queues: half of QUEUE-MAX, far past both ends' pinned buffers
$1000 constant PEER-RCVBUF       \ the stalled peer's receive buffer, fixed before it connects; macOS lo0 rounds it up
$1000 constant SERVER-SNDBUF     \ the stalled peer's server end's send buffer
$10 constant SOCKADDR-N          \ sockaddr_in
$2000000 constant FLOOD-PUSH      \ what the overflow case may push: QUEUE-MAX and every kernel buffer, many times over
$101 constant CROWD               \ the run's sockets: one past a one-byte slot field
$400 constant LOG-CAP             \ the sockets one run may close
CROWD 2 * $40 + AIO-FILL:FILL-CAP 2 * + constant FDS-WANT \ both ends of CROWD+1 loopback connections, the server's, the files' and the fill's
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
$18 constant HEX-MAX              \ the longest payload a transcript line spells out
$37FA213D constant MASK-KEY       \ the masking key of RFC 6455 section 5.7's examples
1006 constant LOST                \ the status of a socket that ended with no close frame
$1F4 constant SHORT-MS            \ a bound a case waits out, far under its other waits
1000000 constant NS-PER-MS
13 constant CR
10 constant LF
32 constant SP
-9980 constant E-SILENT           \ this file's own fixture refusals, outside every lib block
-9981 constant E-HANDLER          \ the handler that is meant to throw
-9982 constant E-DIAL
-9983 constant E-PLATFORM          \ a target this file has no socket option values for

TYPED-VARIABLE SERVER-CFG HTTP:config

: SERVER-CONFIG ( -- ^HTTP:config )
   LOOPBACK SERVER-CFG HTTP-CONFIG:ADDRESS !
   0 SERVER-CFG HTTP-CONFIG:PORT !
   WORKERS SERVER-CFG HTTP-CONFIG:WORKERS !
   IDLE-MS >MS SERVER-CFG HTTP-CONFIG:IDLE !
   SERVER-CFG ;

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
: SO-SNDBUF ( -- n )
   HB-TARGET-LINUX-KERNEL? if 7 exit then
   HB-TARGET-MACOS? if $1001 exit then
   E-PLATFORM throw ;
: ERRNO-INPROGRESS ( -- n )
   HB-TARGET-LINUX-KERNEL? if 115 exit then
   HB-TARGET-MACOS? if 36 exit then
   E-PLATFORM throw ;
$18 constant POLL-FAILED          \ POLLERR and POLLHUP

\ Linux and macOS grow a loopback socket's buffers as traffic goes, and grown
\ ones take a whole push into the kernel, where no byte waits in WS's queue.
PROCESS-SYMBOLS
FUNCTION: SET-OPT-CALL setsockopt ( n n n str -- i32 ) ;FUNCTION
FUNCTION: CONNECT-CALL connect ( n str -- i32 ) ;FUNCTION

BUF-CAP SPAN-BUFFER: PATTERN
BUF-CAP SPAN-BUFFER: CLIENT-TX
BUF-CAP SPAN-BUFFER: CLIENT-RX
BLAST-BYTES SPAN-BUFFER: PUSH-BUF
BLAST-BYTES SPAN-BUFFER: HANDLER-BUF
VOLLEY-CAP SPAN-BUFFER: VOLLEY-BUF
2 SPAN-BUFFER: NOT-UTF8
WS:HEADER-MAX SPAN-BUFFER: RX-HEAD
HEAD-CAP SPAN-BUFFER: HEAD-BUF
STREAM-CAP SPAN-BUFFER: STREAM-BUF  \ what the client has read and not yet taken
HEAD-CAP SPAN-BUFFER: REQ-BUF
RAW-CAP SPAN-BUFFER: RAW-BUF
2 SPAN-BUFFER: STATUS-BUF
2 SPAN-BUFFER: HEX-PAIR
4 SPAN-BUFFER: OPT-VAL
SOCKADDR-N SPAN-BUFFER: PEER-SA  \ the server's address, for DIAL-SMALL's connect
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
variable CLOSED-N                 \ entries logged; written last, under CLOSED-LOCK
8 TYPED-BUFFER CALL-KIND n        \ each call RECORD's socket got: 0 opened, 1 text, 2 binary, 3 closed
8 TYPED-BUFFER CALL-ARG n         \ and the value it carried
variable CALLS-N                  \ entries noted; written last
TASK:FACILITY CLOSED-LOCK         \ `closed` calls run on any worker
1 TYPED-BUFFER HELD WS:socket     \ the socket the route accepted last
1 TYPED-BUFFER NOTED WS:socket    \ the socket whose cue the handler is acting on
1 TYPED-BUFFER KEPT-REQUEST HTTP:request
1 TYPED-BUFFER KEPT-RESPONSE HTTP:response
1 TYPED-BUFFER WATCH WS:socket    \ the socket a case awaits the close of
1 TYPED-BUFFER PUSHED-TO WS:socket  \ the socket this task pushes to
variable PEER-HELD                \ what the stalled peer held unread when last looked at
variable PEER-MOVED               \ when that last changed
variable CLOSE-GOAL               \ the count of logged closes a case awaits
variable BLOCKING                 \ set while the handler of `wait` waits on RELEASED
variable FIRST-DONE               \ set once that wait has ended
variable NEXT-SAW                 \ FIRST-DONE as the handler of `next` found it
TYPED-VARIABLE CFG WS:config      \ the config every WS:START here is given
variable THROW-ARMED              \ set while the next `closed` handler is to throw
variable HOLDING                  \ set while the handler of `hold` waits on UNHELD
variable PROBING                  \ set while the handler of `brief` sends its probes
variable PROBE-END                \ what the probe that ended them threw
variable ERR-U
variable ERR-WANT-U
STRAYS TYPED-BUFFER STRAY-FD n    \ descriptors on one file, taking the numbers a stop gave back
CROWD 1+ TYPED-BUFFER CONNS TCP4:connection  \ the client connections a case holds open at once
TASK:SEMAPHORE ACCEPTED           \ the route has kept its outcome
variable ROUTE-CODE               \ what the route threw before it posted ACCEPTED
TASK:SEMAPHORE FILLED             \ AIO is full: the held handshake goes on
TASK:SEMAPHORE UNHELD             \ lets the handler of `hold` return
TASK:SEMAPHORE RELEASED           \ lets the handler of `wait` return
TASK:MIN-STACK TASK:TASK STOP-TASK \ runs WS:STOP while this task answers a close

: SIZED ( n -- ^WS:config )
   CFG WS:DEFAULTS
   CFG WS-CONFIG:SOCKETS !
   CFG ;

: STALL-SHORT ( n -- ^WS:config )
   SIZED
   SHORT-MS >MS CFG WS-CONFIG:STALL ! ;

: PING-SHORT ( n -- ^WS:config )
   SIZED
   SHORT-MS >MS CFG WS-CONFIG:PING !
   SHORT-MS >MS CFG WS-CONFIG:PONG ! ;

: STALL-MS ( -- n ) CFG WS-CONFIG:STALL @ MS>N ;

: SILENT-MS ( -- n )
   CFG WS-CONFIG:PING @ MS>N
   CFG WS-CONFIG:PONG @ MS>N + ;

\ The cases that wait out a bound run on their own WS, with that bound short.
: RESTART ( ^WS:config -- )
   WS:STOP
   WS:START ;


\ True, with the case failed, once WAIT-BOUND-MS has passed since the given
\ mono-ns.
: GIVEN-UP? ( n -- bool ) {: began:n :}
   mono-ns began - WAIT-BOUND-MS NS-PER-MS * < if false exit then
   T-NEXT
   s" no answer within WAIT-BOUND-MS" T-ASSERT-DETAIL
   true ;

\ The probe's first non-zero answer, or 0 once WAIT-BOUND-MS ends the wait.
: AWAITED ( [ -- n ] -- n ) {: probe :}
   mono-ns {: began:n :}
   begin probe execute dup 0 = while
      began GIVEN-UP? if exit then
      drop POLL-MS >MS TASK:SLEEP
   repeat ;

: SCRIPT$ ( -- str )
   SCRIPT-BUF SCRIPT-U @ SPAN:TAKE SPAN:$ ;

: LOG ( str -- ) {: s:str :}
   s SCRIPT-BUF SCRIPT-U @ SPAN:SKIP SPAN:COPY
   s STR:LEN SCRIPT-U +! ;

: LOG-C ( n -- ) {: c:n :}
   c SCRIPT-BUF SCRIPT-U @ SPAN:U8!
   1 SCRIPT-U +! ;

: LOG-LINE ( str -- )
   LOG LF LOG-C ;

: LOG-N ( n -- ) {: v:n :}
   v 10 >= if v 10 / RECURSE then
   v 10 mod [char] 0 + LOG-C ;

: LOG-HEX ( str -- ) {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ HEX-PAIR SPAN:$ STR:DATA BYTE>HEX
      HEX-PAIR SPAN:$ LOG
   loop ;

: BYTE-SUM ( str -- n ) {: s:str :}
   0 s STR:LEN 0 ?do s STR:DATA i + c@ + loop ;

: LOG-PAYLOAD ( str -- ) {: s:str :}
   s STR:LEN 0= if exit then
   SP LOG-C
   s STR:LEN HEX-MAX <= if s LOG-HEX exit then
   s" sum " LOG s BYTE-SUM LOG-N ;

: OP$ ( WS:opcode -- str )
   MATCH WS:opcode
      continuation OF s" continuation" ENDOF
      text OF s" text" ENDOF
      binary OF s" binary" ENDOF
      close OF s" close" ENDOF
      ping OF s" ping" ENDOF
      pong OF s" pong" ENDOF
   ;MATCH ;

: LOG-FRAME ( str bool WS:opcode str -- ) {: dir:str fin:bool op s:str :}
   dir LOG op OP$ LOG
   fin 0= if [char] + LOG-C then
   SP LOG-C s STR:LEN LOG-N s LOG-PAYLOAD LF LOG-C ;


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

\ The socket is non-blocking, so its connect completes when POLL finds it
\ writable; an error or hang-up there is a refused connect.
: CONNECTED? ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   conn TCP4:CONNECTION-FD {: f:fd :}
   f FD>N PEER-SA SPAN:$ CONNECT-CALL 0 = if true exit then
   FFI:ERRNO ERRNO-INPROGRESS <> if false exit then
   f AIO:WRITABLE -1 >MS AIO:WAIT-READY
   MATCH AIO:readiness
      ready OF POLL-FAILED and 0= ENDOF
      timed-out OF false ENDOF
      refused OF drop false ENDOF
   ;MATCH ;

: SOCKET-OPT! ( n n n -- n )
   {: fd:n opt:n val:n :}
   4 0 ?do val i 8 * rshift $FF and OPT-VAL i SPAN:U8! loop
   fd SOL-SOCKET opt OPT-VAL SPAN:$ SET-OPT-CALL ;

\ PEER-RCVBUF and the server end's SERVER-SNDBUF hold from the first byte. The
\ listener's accepted sockets copy its send buffer: every later accept keeps it.
: DIAL-SMALL ( -- TCP4:connection )
   0 STREAM-AT ! 0 STREAM-U !
   TCP4:SOCKET
   MATCH TCP4:socket-result
      opened OF ENDOF
      failed OF drop E-DIAL throw ENDOF
   ;MATCH {: conn:TCP4:connection :}
   conn TCP4:CONNECTION-FD FD>N SO-RCVBUF PEER-RCVBUF SOCKET-OPT!
   0 <> if conn TCP4:CLOSE DROP-STATUS E-DIAL throw then
   WS-TEST:LISTEN-FD FD>N SO-SNDBUF SERVER-SNDBUF SOCKET-OPT!
   0 <> if conn TCP4:CLOSE DROP-STATUS E-DIAL throw then
   PEER-SA!
   conn CONNECTED? 0= if conn TCP4:CLOSE DROP-STATUS E-DIAL throw then
   conn ;

: SEND-RAW ( TCP4:connection str -- ) {: conn:TCP4:connection s:str :}
   conn s TCP4:WRITE DROP-STATUS ;

: AWAIT ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn TCP4:CONNECTION-FD AIO:READABLE -1 >MS AIO:WAIT-READY
   MATCH AIO:readiness
      ready OF drop ENDOF
      timed-out OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;

: REFILL ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn AWAIT
   conn STREAM-BUF SPAN:$ str> TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N ENDOF
      closed OF drop 0 ENDOF
      failed OF drop 0 ENDOF
   ;MATCH
   STREAM-U !
   0 STREAM-AT ! ;

: READ-SOME ( TCP4:connection SPAN:span<u8> -- n ) {: conn:TCP4:connection dst :}
   STREAM-AT @ STREAM-U @ >= if conn REFILL then
   STREAM-U @ STREAM-AT @ - dst SPAN:LEN min {: take:n :}
   STREAM-BUF STREAM-AT @ take SPAN:SUB SPAN:$ dst SPAN:COPY
   take STREAM-AT +!
   take ;

: READ-N ( TCP4:connection SPAN:span<u8> -- bool ) {: conn:TCP4:connection dst :}
   dst SPAN:LEN 0= if true exit then
   conn dst READ-SOME {: got:n :}
   got 0= if false exit then
   conn dst got SPAN:SKIP RECURSE ;

: HEAD$ ( -- str )
   HEAD-BUF HEAD-U @ SPAN:TAKE SPAN:$ ;

\ The response head and not a byte past it: frames may follow at once.
: READ-HEAD ( TCP4:connection -- ) {: conn:TCP4:connection :}
   0 HEAD-U !
   begin
      HEAD$ S\" \r\n\r\n" STR:ENDS-WITH? if exit then
      conn HEAD-BUF HEAD-U @ 1 SPAN:SUB READ-N 0= if exit then
      1 HEAD-U +!
   again ;

: REQ$ ( -- str )
   REQ-BUF REQ-U @ SPAN:TAKE SPAN:$ ;

: REQ+ ( str -- ) {: s:str :}
   s REQ-BUF REQ-U @ SPAN:SKIP SPAN:COPY
   s STR:LEN REQ-U +! ;

: LINE+ ( str str -- ) {: name:str value:str :}
   name REQ+ s" : " REQ+ value REQ+ S\" \r\n" REQ+ ;

: REQ-BARE ( str str str -- ) {: method:str path:str version:str :}
   0 REQ-U !
   method REQ+ s"  " REQ+ path REQ+ s"  " REQ+ version REQ+ S\" \r\n" REQ+ ;

: HOST+ ( -- )
   s" Host" s" ws-test" LINE+ ;

: REQ-START ( str str -- ) {: method:str path:str :}
   method path s" HTTP/1.1" REQ-BARE
   HOST+ ;

: KEY$ ( -- str )
   s" dGhlIHNhbXBsZSBub25jZQ==" ;

: GOOD-LINES ( -- )
   s" Upgrade" s" websocket" LINE+
   s" Connection" s" Upgrade" LINE+
   s" Sec-WebSocket-Key" KEY$ LINE+
   s" Sec-WebSocket-Version" s" 13" LINE+ ;

: REQ-OVER ( TCP4:connection -- TCP4:connection ) {: conn:TCP4:connection :}
   conn REQ$ SEND-RAW
   conn READ-HEAD
   conn ;

: REQ-SEND ( -- TCP4:connection )
   DIAL REQ-OVER ;

: UPGRADE-OVER ( TCP4:connection str -- TCP4:connection ) {: conn:TCP4:connection path:str :}
   s" GET" path REQ-START GOOD-LINES S\" \r\n" REQ+
   conn REQ-OVER
   HEAD$ s" HTTP/1.1 101 " STR:STARTS-WITH? TTRUE ;

: UPGRADE ( str -- TCP4:connection )
   DIAL swap UPGRADE-OVER ;

: FRAME$ ( bool WS:opcode str -- str ) {: fin:bool op s:str :}
   fin op s STR:LEN MASK-KEY WS-HEADER:MAKE WS-SENDER:CLIENT CLIENT-TX WS:ENCODE-HEADER {: size:n :}
   s CLIENT-TX size SPAN:SKIP SPAN:COPY
   MASK-KEY CLIENT-TX size s STR:LEN SPAN:SUB WS:MASK
   CLIENT-TX size s STR:LEN + SPAN:TAKE SPAN:$ ;

: SAY ( TCP4:connection bool WS:opcode str -- )
   {: conn:TCP4:connection fin:bool op s:str :}
   s" > " fin op s LOG-FRAME
   conn fin op s FRAME$ SEND-RAW ;

: NIBBLE ( n -- n ) {: c:n :}
   c [char] a >= if c [char] a - 10 + exit then
   c [char] 0 - ;

\ Bytes written in hex, two lowercase digits each.
: BYTES ( str -- str ) {: s:str :}
   s STR:LEN 2 / {: k:n :}
   k 0 ?do
      s STR:DATA i 2 * + c@ NIBBLE 4 lshift
      s STR:DATA i 2 * + 1+ c@ NIBBLE or $FF and
      RAW-BUF i SPAN:U8!
   loop
   RAW-BUF k SPAN:TAKE SPAN:$ ;

: SAY-RAW ( TCP4:connection str -- ) {: conn:TCP4:connection s:str :}
   s" > raw " LOG s LOG-LINE
   conn s BYTES SEND-RAW ;

: STATUS$ ( n -- str ) {: code:n :}
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
   RX-HEAD SPAN:$ STR:DATA held >str
   WS-SENDER:SERVER BUF-CAP WS:DECODE-HEADER
   MATCH WS:decoded
      need OF false ENDOF
      frame OF KEEP-HEADER held swap ENDOF
   ;MATCH ;

: READ-HEADER ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   conn RX-HEAD 2 SPAN:TAKE READ-N 0= if false exit then
   2 HEADER? if drop true exit then {: size:n :}
   conn RX-HEAD 2 size 2 - SPAN:SUB READ-N 0= if false exit then
   size HEADER? nip ;

: NEXT-FRAME ( TCP4:connection -- ) {: conn:TCP4:connection :}
   0 RX-EOF ! 0 RX-LEN !
   conn READ-HEADER 0= if 1 RX-EOF ! exit then
   conn CLIENT-RX RX-LEN @ SPAN:TAKE READ-N 0= if 1 RX-EOF ! then ;

: RX$ ( -- str )
   CLIENT-RX RX-LEN @ SPAN:TAKE SPAN:$ ;

: HEARD ( -- )
   RX-EOF @ 0 <> if s" < end of stream" LOG-LINE exit then
   s" < " RX-FIN @ 0 <> RX-OP @ RX$ LOG-FRAME ;

: HEAR ( TCP4:connection -- )
   NEXT-FRAME HEARD ;

: PROBE-FRAME? ( -- bool )
   RX-EOF @ 0= RX-OP @ OP$ s" text" STR:= and ;

\ The probes the handler of `brief` sent are read past unlogged, since how
\ many it sent before the stop is the scheduler's; the frame after them is
\ heard.
: HEAR-PAST-PROBES ( TCP4:connection -- ) {: conn:TCP4:connection :}
   begin conn NEXT-FRAME PROBE-FRAME? 0= until
   HEARD ;

: GOT ( WS:opcode str -- ) {: op want:str :}
   RX-EOF @ 0 T=
   RX-FIN @ 1 T=
   RX-OP @ OP$ op OP$ T$=
   RX$ want T$= ;

: GOT-CLOSE ( n -- ) {: code:n :}
   WS-OPCODE:CLOSE code STATUS$ GOT ;

: GOT-END ( -- )
   RX-EOF @ 1 T= ;

\ The server ends the stream, which it does once the handler has returned.
: ENDS ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn HEAR GOT-END
   conn TCP4:CLOSE DROP-STATUS ;

: EXCHANGE ( TCP4:connection WS:opcode str -- ) {: conn:TCP4:connection op s:str :}
   conn true op s SAY
   conn HEAR op s GOT ;

: FAREWELL ( TCP4:connection -- ) {: conn:TCP4:connection :}
   conn true WS-OPCODE:CLOSE 1000 STATUS$ SAY
   conn HEAR 1000 GOT-CLOSE
   conn ENDS ;

: SECTION ( str -- ) {: s:str :}
   s T-LABEL
   s" == " LOG s LOG-LINE ;

: PATTERN$ ( n -- str )
   PATTERN swap SPAN:TAKE SPAN:$ ;

: LOG-HEAD ( -- )
   HEAD$ {: h:str :}
   h STR:LEN 0 ?do h STR:DATA i + c@ dup CR = if drop else LOG-C then loop ;

: FIRST-LINE ( str -- str ) {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ CR = if s STR:DATA i >str unloop exit then
   loop s ;


: CLOSED-OF ( WS:socket -- n ) {: sock:WS:socket :}
   CLOSED-N atomic@ 0 ?do
      i CLOSED-SOCK @ SOCK>N sock SOCK>N = if i CLOSED-CODE @ unloop exit then
   loop
   0 ;

: WATCHED ( -- n )
   0 WATCH @ CLOSED-OF ;

: CLOSED-LOGGED ( n WS:socket -- ) {: code:n sock:WS:socket :}
   CLOSED-N @ {: k:n :}
   k LOG-CAP >= if E-SILENT throw then
   sock k CLOSED-SOCK !
   code k CLOSED-CODE !
   k 1+ CLOSED-N atomic! ;

: CLOSED-IN ( n WS:socket -- )
   CLOSED-LOCK TASK:GET
   [: CLOSED-LOGGED ;] [: CLOSED-LOCK TASK:RELEASE ;] finally ;

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

\ The handler holds its worker until the case signals RELEASED, then answers
\ `waited`.
: BLOCK ( WS:socket -- ) {: sock:WS:socket :}
   1 BLOCKING atomic!
   RELEASED TASK:WAIT-SEM
   1 FIRST-DONE atomic!
   0 BLOCKING atomic!
   sock s" waited" WS:SEND-TEXT ;

\ The handler holds its worker until the case signals UNHELD.
: HOLD ( -- )
   1 HOLDING atomic!
   UNHELD TASK:WAIT-SEM
   0 HOLDING atomic! ;

: SEND-PROBE ( -- )
   0 NOTED @ s" ." WS:SEND-TEXT ;

\ The handler sends a probe every POLL-MS until one is refused, as it is once
\ a stop has closed the socket, and returns: the stop's own close ends the
\ call, not a step the case takes.
: PROBE ( -- )
   1 PROBING atomic!
   begin [: SEND-PROBE ;] catch dup 0 = while
      drop POLL-MS >MS TASK:SLEEP
   repeat
   PROBE-END atomic!
   0 PROBING atomic! ;

: TEXT-IN ( str WS:socket -- ) {: s:str sock:WS:socket :}
   sock 0 NOTED !
   s s" throw" STR:= if E-HANDLER throw then
   s s" stop" STR:= if [: WS:STOP ;] catch HANDLER-STOP atomic! exit then
   s s" badtext" STR:= if [: SEND-BAD ;] catch BAD-TEXT atomic! exit then
   s s" go" STR:= if sock CLOSE-FIRST exit then
   s s" bye" STR:= if sock 4000 WS:CLOSE exit then
   s s" blast" STR:= if sock BLAST exit then
   s s" wait" STR:= if sock BLOCK exit then
   s s" hold" STR:= if HOLD exit then
   s s" brief" STR:= if PROBE exit then
   s s" next" STR:= if FIRST-DONE atomic@ NEXT-SAW atomic! then
   sock s WS:SEND-TEXT ;

: BINARY-IN ( str WS:socket -- ) {: s:str sock:WS:socket :}
   sock s WS:SEND-BINARY ;

\ The file the cases below open on descriptors the stop gave back.
: STRAY$ ( -- str )
   s" build/ws-stray.txt" ;

: THROW-IN-CLOSED ( -- )
   THROW-ARMED @ 0= if exit then
   0 THROW-ARMED !
   E-HANDLER throw ;

\ Every socket's handler, on a request worker.
: NOTE ( WS:socket n WS:message -- ) {: sock:WS:socket arg:n msg :}
   msg
   MATCH WS:message
      opened OF ENDOF
      text OF >str sock TEXT-IN ENDOF
      binary OF >str sock BINARY-IN ENDOF
      closed OF sock CLOSED-IN THROW-IN-CLOSED ENDOF
   ;MATCH ;

: KIND ( WS:message -- n )
   MATCH WS:message
      opened OF 0 ENDOF
      text OF 2drop 1 ENDOF
      binary OF 2drop 2 ENDOF
      closed OF drop 3 ENDOF
   ;MATCH ;

\ NOTE, behind a note of the call's kind and value, so `closed` is noted before
\ NOTE logs it. One socket's calls run one at a time.
: RECORD ( WS:socket n WS:message -- ) {: sock:WS:socket arg:n msg :}
   CALLS-N atomic@ {: k:n :}
   msg KIND k CALL-KIND !
   arg k CALL-ARG !
   k 1+ CALLS-N atomic!
   sock arg msg NOTE ;

: KEEP ( WS:accepted -- )
   MATCH WS:accepted
      open OF 0 HELD ! ENDOF
      refused OF REFUSED-WITH ! ENDOF
   ;MATCH ;

\ A route posts ACCEPTED whether or not its body threw, so the case's wait
\ ends and AWAIT-ROUTE fails with the route's own error.
: ROUTED ( HTTP:request HTTP:response [ HTTP:request HTTP:response -- HTTP:request HTTP:response ] -- )
   catch nip nip ROUTE-CODE !
   ACCEPTED TASK:SIGNAL-SEM ;

: OPEN ( HTTP:request HTTP:response -- )
   [: 2dup [: NOTE ;] 0 WS:ACCEPT KEEP ;] ROUTED ;

: SEVEN ( HTTP:request HTTP:response -- )
   [: 2dup [: RECORD ;] 7 WS:ACCEPT KEEP ;] ROUTED ;

: ACCEPT-AGAIN ( -- )
   0 KEPT-REQUEST @ 0 KEPT-RESPONSE @ [: NOTE ;] 0 WS:ACCEPT KEEP ;

\ One request is one socket: a second accept of it is refused, and the first
\ socket serves on.
: ACCEPT-TWICE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked 0 KEPT-REQUEST !
   answer 0 KEPT-RESPONSE !
   asked answer [: NOTE ;] 0 WS:ACCEPT KEEP
   [: ACCEPT-AGAIN ;] catch SECOND-ACCEPT ! ;

: TWICE ( HTTP:request HTTP:response -- )
   [: 2dup ACCEPT-TWICE ;] ROUTED ;

\ Holds the handshake's slot until WS's task has left, which a stop waits for
\ before it waits for the handshake.
: STALL ( -- )
   1 STALLING !
   [: WS-TEST:LEFT ;] AWAITED drop ;

: STALLED ( HTTP:request HTTP:response -- )
   [: 2dup [: NOTE ;] 0 [: STALL ;] WS-TEST:ACCEPT-STALLED KEEP ;] ROUTED ;

: AWAIT-FILL ( -- )
   1 STALLING !
   FILLED TASK:WAIT-SEM ;

: FILLING ( HTTP:request HTTP:response -- )
   [: 2dup [: NOTE ;] 0 [: AWAIT-FILL ;] WS-TEST:ACCEPT-STALLED KEEP ;] ROUTED ;

: PLAIN ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   answer 200 HTTP:STATUS!
   answer s" plain" HTTP:BODY! ;

: INSTALL-ROUTES ( -- )
   HTTP:ROUTES-RESET
   s" GET" s" /ws" [: OPEN ;] HTTP:ROUTE
   s" POST" s" /ws" [: OPEN ;] HTTP:ROUTE
   s" GET" s" /twice" [: TWICE ;] HTTP:ROUTE
   s" GET" s" /seven" [: SEVEN ;] HTTP:ROUTE
   s" GET" s" /stall" [: STALLED ;] HTTP:ROUTE
   s" GET" s" /fill" [: FILLING ;] HTTP:ROUTE
   s" GET" s" /plain" [: PLAIN ;] HTTP:ROUTE ;


: CLOSED-WITH ( WS:socket n -- ) {: sock:WS:socket code:n :}
   sock 0 WATCH !
   [: WATCHED ;] AWAITED code T= ;

: FAILED ( TCP4:connection WS:socket n -- ) {: conn:TCP4:connection sock:WS:socket code:n :}
   conn HEAR code GOT-CLOSE
   conn ENDS
   sock code CLOSED-WITH ;

: AWAIT-ROUTE ( -- )
   ACCEPTED TASK:WAIT-SEM
   ROUTE-CODE @ {: code:n :}
   code 0<> if code throw then ;

: OPENED ( TCP4:connection -- TCP4:connection WS:socket )
   AWAIT-ROUTE
   0 HELD @ ;

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
   HEAD$ s" HTTP/1.1 200 OK" STR:STARTS-WITH? TTRUE
   conn TCP4:CLOSE DROP-STATUS
   0 STREAM-AT ! 0 STREAM-U ! ;

\ RFC 6455 section 1.3's key and the accept value it names.
: HANDSHAKE-CASE ( -- )
   s" handshake" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   HEAD$ S\" \r\nSec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n" STR:CONTAINS? TTRUE
   HEAD$ S\" \r\nUpgrade: websocket\r\n" STR:CONTAINS? TTRUE
   HEAD$ S\" \r\nConnection: Upgrade\r\n" STR:CONTAINS? TTRUE
   HEAD$ s" Content-Length" STR:CONTAINS? TFALSE
   LOG-HEAD
   conn TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   conn ENDS
   sock LOST CLOSED-WITH ;

\ The status line a refused handshake is answered with, and the status the
\ route was told it was refused with: 0 when the server refused the request
\ before any route ran.
: REFUSED ( str str n -- ) {: label:str want:str status:n :}
   label T-LABEL
   0 REFUSED-WITH !
   S\" \r\n" REQ+
   REQ-SEND {: conn:TCP4:connection :}
   status 0 <> if AWAIT-ROUTE then
   HEAD$ want STR:STARTS-WITH? TTRUE
   label LOG s" : " LOG HEAD$ FIRST-LINE LOG-LINE
   conn TCP4:CLOSE DROP-STATUS
   REFUSED-WITH @ status T= ;

: BAD$ ( -- str )
   s" HTTP/1.1 400 Bad Request" ;

\ Section 4.2.1's first two items: the handshake is an HTTP/1.1 GET carrying
\ one valid Host.
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
   HEAD$ S\" \r\nSec-WebSocket-Version: 13\r\n" STR:CONTAINS? TTRUE
   HEAD$ S\" \r\nUpgrade: websocket\r\n" STR:CONTAINS? TTRUE ;

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
   HEAD$ s" HTTP/1.1 101 " STR:STARTS-WITH? TTRUE
   conn WS-OPCODE:TEXT s" ok" EXCHANGE
   conn FAREWELL ;

: ECHO-CASE ( -- )
   s" text and binary reach the handler" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn WS-OPCODE:TEXT s" hello" EXCHANGE
   conn WS-OPCODE:TEXT s" hello" STR:DATA 0 >str EXCHANGE
   conn WS-OPCODE:TEXT s" 68c3a96c6c6f20e282ac" BYTES EXCHANGE
   conn WS-OPCODE:BINARY 5 PATTERN$ EXCHANGE
   conn FAREWELL
   sock 1000 CLOSED-WITH ;

\ The handler's text that is not UTF-8 is refused before a byte of it goes
\ out: the first frame the client hears is the echo of its next text.
: BAD-TEXT-CASE ( -- )
   s" text the server may not send" SECTION
   0 BAD-TEXT atomic!
   OPENS drop {: conn:TCP4:connection :}
   conn true WS-OPCODE:TEXT s" badtext" SAY
   conn WS-OPCODE:TEXT s" ok" EXCHANGE
   conn FAREWELL
   s" text that is not UTF-8 is refused" T-LABEL
   BAD-TEXT atomic@ E-WS-TEXT T= ;

\ Each length form at its edges: 7 bits to 125, 16 bits from 126 to 65535, and
\ 64 bits past that.
: LENGTH-CASES ( -- )
   s" echo binary in each length form" SECTION
   OPENS drop {: conn:TCP4:connection :}
   conn WS-OPCODE:BINARY 125 PATTERN$ EXCHANGE
   conn WS-OPCODE:BINARY 126 PATTERN$ EXCHANGE
   conn WS-OPCODE:BINARY 65535 PATTERN$ EXCHANGE
   conn WS-OPCODE:BINARY 65536 PATTERN$ EXCHANGE
   conn WS-OPCODE:BINARY BIG PATTERN$ EXCHANGE
   conn FAREWELL ;

\ A message in fragments comes back as one frame: control frames pass between
\ the fragments, a fragment may be empty, and a scalar may straddle two.
: FRAGMENT-CASES ( -- )
   s" fragments" SECTION
   OPENS drop {: conn:TCP4:connection :}
   conn false WS-OPCODE:TEXT s" Hel" SAY
   conn false WS-OPCODE:CONTINUATION s" lo," SAY
   conn true WS-OPCODE:CONTINUATION s"  world" SAY
   conn HEAR WS-OPCODE:TEXT s" Hello, world" GOT
   conn false WS-OPCODE:BINARY s" 0102" BYTES SAY
   conn true WS-OPCODE:PING s" !" SAY
   conn HEAR WS-OPCODE:PONG s" !" GOT
   conn true WS-OPCODE:CONTINUATION s" 0304" BYTES SAY
   conn HEAR WS-OPCODE:BINARY s" 01020304" BYTES GOT
   conn false WS-OPCODE:TEXT s" c3" BYTES SAY
   conn true WS-OPCODE:CONTINUATION s" a9" BYTES SAY
   conn HEAR WS-OPCODE:TEXT s" c3a9" BYTES GOT
   conn false WS-OPCODE:TEXT s" hi" SAY
   conn true WS-OPCODE:CONTINUATION s" hi" STR:DATA 0 >str SAY
   conn HEAR WS-OPCODE:TEXT s" hi" GOT
   conn FAREWELL ;

\ A ping is answered with its own payload, to the 125 bytes a control frame may
\ carry, and a pong nobody asked for is let pass.
: PING-CASES ( -- )
   s" ping and pong" SECTION
   OPENS drop {: conn:TCP4:connection :}
   conn true WS-OPCODE:PING s" hb" SAY
   conn HEAR WS-OPCODE:PONG s" hb" GOT
   conn true WS-OPCODE:PING s" hb" STR:DATA 0 >str SAY
   conn HEAR WS-OPCODE:PONG s" hb" STR:DATA 0 >str GOT
   conn true WS-OPCODE:PING 125 PATTERN$ SAY
   conn HEAR WS-OPCODE:PONG 125 PATTERN$ GOT
   conn true WS-OPCODE:PONG s" x" SAY
   conn WS-OPCODE:TEXT s" ok" EXCHANGE
   conn FAREWELL ;

\ A client that does not wait for the 101: the first AT bytes of its frame ride
\ behind the request, where the HTTP server has already read them, and it
\ writes the rest once the 101 is in.
: EARLY ( str n -- ) {: s:str at:n :}
   s" GET" s" /ws" REQ-START GOOD-LINES S\" \r\n" REQ+
   s" > " true WS-OPCODE:TEXT s LOG-FRAME
   true WS-OPCODE:TEXT s FRAME$ {: f:str :}
   f STR:DATA at >str REQ+
   REQ-SEND OPENED drop {: conn:TCP4:connection :}
   HEAD$ s" HTTP/1.1 101 " STR:STARTS-WITH? TTRUE
   at f STR:LEN < if conn f STR:DATA at + f STR:LEN at - >str SEND-RAW then
   conn HEAR WS-OPCODE:TEXT s GOT
   conn FAREWELL ;

6 constant SHORT-HEAD              \ a masked header under 126 payload bytes: 2 bytes and the mask key

: EARLY-CASE ( -- )
   s" a frame behind the handshake" SECTION
   s" early" dup STR:LEN SHORT-HEAD + EARLY
   s" a frame whose payload the handshake cut" T-LABEL
   s" early payload" SHORT-HEAD 2 + EARLY
   s" a frame whose header the handshake cut" T-LABEL
   s" early header" 1 EARLY ;

: KIND$ ( n -- str ) {: kind:n :}
   kind 0= if s" opened" exit then
   kind 1 = if s" text" exit then
   kind 2 = if s" binary" exit then
   s" closed" ;

: LOG-CALLS ( -- )
   CALLS-N atomic@ 0 ?do
      s" handler " LOG i CALL-KIND @ KIND$ LOG
      SP LOG-C i CALL-ARG @ LOG-N LF LOG-C
   loop ;

\ The handler is called `opened` before the text the client sent behind its
\ request, and every call carries the value the route accepted with.
: FIRST-CALL-CASE ( -- )
   s" opened comes first, with the route's value" SECTION
   0 CALLS-N atomic!
   s" GET" s" /seven" REQ-START GOOD-LINES S\" \r\n" REQ+
   s" > " true WS-OPCODE:TEXT s" first" LOG-FRAME
   true WS-OPCODE:TEXT s" first" FRAME$ REQ+
   REQ-SEND OPENED {: conn:TCP4:connection sock:WS:socket :}
   conn HEAR WS-OPCODE:TEXT s" first" GOT
   conn FAREWELL
   sock 1000 CLOSED-WITH
   LOG-CALLS ;

: BREAKS ( str str n -- ) {: label:str s:str code:n :}
   label SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn s SAY-RAW
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
   conn false WS-OPCODE:TEXT s" a" SAY
   conn true WS-OPCODE:TEXT s" b" SAY
   conn sock 1002 FAILED
   s" text that is not UTF-8" SECTION
   OPENS {: bad:TCP4:connection bad-sock:WS:socket :}
   bad true WS-OPCODE:TEXT s" c328" BYTES SAY
   bad bad-sock 1007 FAILED
   s" fragments that are not UTF-8 together" SECTION
   OPENS {: split:TCP4:connection split-sock:WS:socket :}
   split false WS-OPCODE:TEXT s" c3" BYTES SAY
   split true WS-OPCODE:CONTINUATION s" 28" BYTES SAY
   split split-sock 1007 FAILED
   s" fragments one byte past the bound" SECTION
   OPENS {: long:TCP4:connection long-sock:WS:socket :}
   long false WS-OPCODE:BINARY BIG PATTERN$ SAY
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
   sock LOST CLOSED-WITH
   s" fragments of exactly the bound" SECTION
   OPENS {: long:TCP4:connection long-sock:WS:socket :}
   long false WS-OPCODE:BINARY BIG PATTERN$ SAY
   long s" 80ff00000000000eee9037fa213d" SAY-RAW
   long TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   long ENDS
   long-sock LOST CLOSED-WITH ;

\ A close frame's status comes back as it was sent when a frame may carry it,
\ and is a protocol error when none may; the handler is told the status the
\ server closed under.
: CLOSES-WITH ( n n -- ) {: code:n answer:n :}
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:CLOSE code STATUS$ SAY
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
   bare true WS-OPCODE:CLOSE s" x" STR:DATA 0 >str SAY
   bare HEAR WS-OPCODE:CLOSE s" x" STR:DATA 0 >str GOT
   bare ENDS
   bare-sock 1005 CLOSED-WITH
   s" a close with a reason" SECTION
   OPENS {: reason:TCP4:connection reason-sock:WS:socket :}
   reason true WS-OPCODE:CLOSE s" 03e9627965" BYTES SAY
   reason reason-sock 1001 FAILED
   s" a close with half a status" SECTION
   OPENS {: half:TCP4:connection half-sock:WS:socket :}
   half true WS-OPCODE:CLOSE s" 03" BYTES SAY
   half half-sock 1002 FAILED
   s" a close whose reason is not UTF-8" SECTION
   OPENS {: garbled:TCP4:connection garbled-sock:WS:socket :}
   garbled true WS-OPCODE:CLOSE s" 03e8c328" BYTES SAY
   garbled garbled-sock 1007 FAILED ;

\ The handler closes first: a ping the client sends before it answers the close
\ is still answered (section 5.5.2).
: SERVER-CLOSE-CASE ( -- )
   s" the server closes first" SECTION
   0 BAD-CODE atomic! 0 LATE-SEND atomic!
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:TEXT s" go" SAY
   conn HEAR 1000 GOT-CLOSE
   conn true WS-OPCODE:PING s" x" SAY
   conn HEAR WS-OPCODE:PONG s" x" GOT
   conn true WS-OPCODE:CLOSE 1000 STATUS$ SAY
   conn ENDS
   s" the handler saw the client's close" T-LABEL
   sock 1000 CLOSED-WITH
   s" status 1005 is not one to send" T-LABEL
   BAD-CODE atomic@ E-WS-CODE T=
   s" nothing is sent behind a close" T-LABEL
   LATE-SEND atomic@ E-WS-CLOSED T= ;

: ALL-CLOSED ( -- n )
   CLOSED-N atomic@ CLOSE-GOAL @ >= if 1 else 0 then ;

: OPEN-ALL ( -- )
   CROWD 0 ?do OPENS drop i CONNS ! loop ;

: CLOSE-ALL ( -- )
   CLOSED-N @ CROWD + CLOSE-GOAL !
   CROWD 0 ?do i CONNS @ TCP4:CLOSE DROP-STATUS loop
   [: ALL-CLOSED ;] AWAITED drop ;

: TWICE-CASE ( -- )
   s" one request accepted twice" SECTION
   0 SECOND-ACCEPT !
   s" /twice" UPGRADE OPENED {: conn:TCP4:connection sock:WS:socket :}
   conn WS-OPCODE:TEXT s" still here" EXCHANGE
   SECOND-ACCEPT @ HTTP:E-STATE T=
   conn FAREWELL
   sock 1000 CLOSED-WITH
   s" the refused accept keeps no slot" T-LABEL
   OPEN-ALL CLOSE-ALL ;

\ WS reports each handler fault on fd 2. For THROW-CASE fd 2 is a file, read
\ back after it.
: ERR-PATH$ ( -- str )
   s" build/ws-stderr.txt" ;

: ERR$ ( -- str )
   ERR-BUF ERR-U @ SPAN:TAKE SPAN:$ ;

: ERR-WANT$ ( -- str )
   ERR-WANT ERR-WANT-U @ SPAN:TAKE SPAN:$ ;

: ERR-W ( str -- ) {: s:str :}
   s ERR-WANT ERR-WANT-U @ SPAN:SKIP SPAN:COPY
   s STR:LEN ERR-WANT-U +! ;

: ERR-THROW ( n -- )
   FMT:SB-INT
   LF SB-APPEND-C
   SB$ ERR-W ;

\ The line WS logs after its stamp when this socket's handler throws E-HANDLER.
: FAULT-LINE ( WS:socket -- )
   {: sock:WS:socket :}
   SB-RESET
   s"  error ws: socket " SB-APPEND
   sock SOCK>N FMT:SB-U
   s"  failed, throw " SB-APPEND
   E-HANDLER ERR-THROW ;


: ERR-READ ( -- )
   ERR-PATH$ ERR-BUF SPAN:$ str> FS:READ-ALL ERR-U ! ;


: ERR-WRITTEN ( -- n )
   ERR-READ
   ERR$ LF STR:COUNT-CHAR ERR-WANT$ LF STR:COUNT-CHAR >= if 1 else 0 then ;


\ Drops each line's LOG stamp from what was read back, in place.
: UNSTAMP ( -- )
   0 0 ERR-U @ 0 ?do
      dup DATE:TIME-LEN >= if
         over ERR-BUF i SPAN:U8@ swap ERR-BUF swap SPAN:U8!
         swap 1+ swap
      then
      ERR-BUF i SPAN:U8@ LF = if drop 0 else 1+ then
   loop
   drop ERR-U ! ;


: THROWING ( -- )
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   sock FAULT-LINE
   conn true WS-OPCODE:TEXT s" throw" SAY
   conn sock 1011 FAILED
   s" a `closed` handler that throws" T-LABEL
   OPENS {: gone:TCP4:connection gone-sock:WS:socket :}
   gone-sock FAULT-LINE
   1 THROW-ARMED !
   gone TCP4:CLOSE DROP-STATUS
   gone-sock LOST CLOSED-WITH
   [: ERR-WRITTEN ;] AWAITED drop ;

\ A handler that throws closes its socket under 1011, and one that throws on
\ `closed` has no socket left to close: each throw is one line on stderr with
\ the socket's handle and the code.
: THROW-CASE ( -- )
   s" a throwing handler" SECTION
   s" build" FS:MAKE-DIRS
   0 ERR-WANT-U !
   ERR-PATH$ STDERR:CAPTURE
   [: THROWING ;] [: STDERR:;CAPTURE ;] finally
   s" each fault reported" T-LABEL
   ERR-READ UNSTAMP
   ERR$ ERR-WANT$ T$=
   ERR-PATH$ FS:REMOVE-FILE ;

\ More sockets than the server has workers stay open, and plain requests are
\ still answered: no socket holds a worker.
: MANY-CASE ( -- )
   s" more sockets than workers" SECTION
   HTTP:MAX-WORKERS 1+ {: n:n :}
   n 0 ?do OPENS drop i CONNS ! loop
   PLAIN-ONCE PLAIN-ONCE
   n 0 ?do i CONNS @ WS-OPCODE:TEXT s" each" EXCHANGE loop
   n 0 ?do i CONNS @ FAREWELL loop ;

\ A handler that blocks holds its own socket alone: another socket is answered
\ meanwhile, and the message sent behind the blocked one reaches the handler
\ only once that call has returned.
: BLOCK-CASE ( -- )
   s" a handler that blocks" SECTION
   OPENS drop {: a:TCP4:connection :}
   OPENS drop {: b:TCP4:connection :}
   0 BLOCKING atomic!
   0 FIRST-DONE atomic!
   -1 NEXT-SAW atomic!
   a true WS-OPCODE:TEXT s" wait" SAY
   [: BLOCKING atomic@ ;] AWAITED drop
   a true WS-OPCODE:TEXT s" next" SAY
   b WS-OPCODE:TEXT s" hi" EXCHANGE
   s" and while the other socket's handler blocks" T-LABEL
   BLOCKING atomic@ 1 T=
   FIRST-DONE atomic@ 0 T=
   BLOCKING atomic@ 0 <> if s" < answered while the other handler blocks" LOG-LINE then
   RELEASED TASK:SIGNAL-SEM
   a HEAR WS-OPCODE:TEXT s" waited" GOT
   a HEAR WS-OPCODE:TEXT s" next" GOT
   s" and the message behind it only once that call returned" T-LABEL
   NEXT-SAW atomic@ 1 T=
   NEXT-SAW atomic@ 1 = if s" < next handled after wait returned" LOG-LINE then
   a FAREWELL
   b FAREWELL ;

: PUSH-CASE ( -- )
   s" a push from another task" SECTION
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   sock s" pushed" WS:SEND-TEXT
   conn HEAR WS-OPCODE:TEXT s" pushed" GOT
   conn FAREWELL ;

: PUSH-ONE ( str -- str ) {: s:str :}
   0 PUSHED-TO @ s WS:SEND-BINARY
   s ;

\ That many PUSH-BYTES frames from this task to the socket; the code the first
\ refused push threw, or 0 when all of them were queued.
: PUSHES ( WS:socket n -- n ) {: sock:WS:socket n:n :}
   sock 0 PUSHED-TO !
   n 0 ?do
      PATTERN PUSH-BYTES SPAN:TAKE SPAN:$ [: PUSH-ONE ;] catch {: code:n :} drop
      code 0 <> if code unloop exit then
   loop
   0 ;

: UNIFORM? ( str -- bool ) {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ s STR:DATA c@ <> if false unloop exit then
   loop
   true ;

: TALLY ( -- )
   RX$ {: r:str :}
   RX-EOF @ 0 <> r STR:LEN BLAST-BYTES <> or if 1 TORN +! exit then
   r UNIFORM? 0= if 1 TORN +! exit then
   r STR:DATA c@ PUSH-BYTE = if 1 PUSHED-N +! exit then
   r STR:DATA c@ HANDLER-BYTE = if 1 HANDLER-N +! exit then
   1 TORN +! ;

\ Each send queues its frame whole, so every frame the client reads is one of
\ the two tasks', whole, however their sends interleave.
: BOTH-CASE ( -- )
   s" two tasks sending at once" SECTION
   0 PUSHED-N ! 0 HANDLER-N ! 0 TORN !
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:TEXT s" blast" SAY
   BLAST-N 0 ?do sock PUSH-BUF SPAN:$ WS:SEND-BINARY loop
   BLAST-N 2 * 0 ?do conn NEXT-FRAME TALLY loop
   s" every frame of both tasks arrived whole" T-LABEL
   PUSHED-N @ BLAST-N T=
   HANDLER-N @ BLAST-N T=
   TORN @ 0 T=
   s" < " LOG PUSHED-N @ LOG-N s"  pushed frames and " LOG HANDLER-N @ LOG-N
   s"  of the handler's, " LOG TORN @ LOG-N s"  torn" LOG-LINE
   conn FAREWELL ;

: VOLLEY-FILL ( -- )
   true WS-OPCODE:PING PATTERN SPAN:$ STR:DATA 0 >str FRAME$ {: f:str :}
   BURST-PINGS 0 ?do f VOLLEY-BUF i f STR:LEN * SPAN:SKIP SPAN:COPY loop ;

: PONG-IN ( -- )
   RX-EOF @ 0 <> if exit then
   RX-OP @ OP$ s" pong" STR:= RX-LEN @ 0= and if 1 PONGS +! then ;

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
   gone 1000 CLOSED-WITH
   OPENS drop {: conn:TCP4:connection :}
   conn WS-OPCODE:TEXT s" mine" EXCHANGE
   gone 0 PUSHED-TO !
   s" a send through it is refused" T-LABEL
   [: s" late" PUSH-ONE drop ;] E-WS-CLOSED TTHROWSQ
   s" and a close through it is nothing new" T-LABEL
   gone 1000 WS:CLOSE
   conn WS-OPCODE:TEXT s" only mine" EXCHANGE
   conn FAREWELL ;

: UNREAD-N ( TCP4:connection -- n )
   TCP4:UNREAD
   MATCH TCP4:queue-result
      queued OF BLEN>N ENDOF
      failed OF drop E-SILENT throw ENDOF
   ;MATCH ;

\ The socket's handler is told `closed` 1006 within the config's stall of the
\ last byte the peer's end took, as WS's own clock runs. That moment is the
\ kernel's: a zero-window probe can fill the part of a segment still free in
\ the peer's buffer just as the stall runs out, and WS rightly starts its
\ clock again.
: STALL-CLOSES ( TCP4:connection WS:socket -- ) {: peer:TCP4:connection sock:WS:socket :}
   sock 0 WATCH !
   peer UNREAD-N PEER-HELD !
   mono-ns dup PEER-MOVED ! {: began:n :}
   begin WATCHED dup 0 = while
      drop began GIVEN-UP? if exit then
      peer UNREAD-N {: held:n :}
      held PEER-HELD @ <> if held PEER-HELD ! mono-ns PEER-MOVED ! then
      POLL-MS >MS TASK:SLEEP
   repeat
   LOST T=
   s" closed within the stall bound of the peer's last move" T-LABEL
   mono-ns PEER-MOVED @ - STALL-MS GRACE-MS + NS-PER-MS * < TTRUE ;

: STALL-CASE ( -- )
   s" a stalled peer" SECTION
   2 STALL-SHORT RESTART
   DIAL-SMALL OPENS-OVER {: still:TCP4:connection stuck:WS:socket :}
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   stuck STALL-PUSH PUSH-BYTES / PUSHES 0 T=
   conn WS-OPCODE:TEXT s" meanwhile" EXCHANGE
   stuck 0 WATCH !
   WATCHED 0 T=
   still stuck STALL-CLOSES
   s" the other socket still talks and closes" T-LABEL
   conn WS-OPCODE:TEXT s" after" EXCHANGE
   conn FAREWELL
   still TCP4:CLOSE DROP-STATUS ;

\ A peer that sends nothing and answers no ping is lost within the config's
\ ping and pong of its socket's opening: it reads the ping, then the end of
\ the stream, and its handler is told `closed` 1006.
: SILENT-CASE ( -- )
   s" a peer that falls silent" SECTION
   1 PING-SHORT RESTART
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   mono-ns {: began:n :}
   sock LOST CLOSED-WITH
   s" lost within its quiet bound" T-LABEL
   mono-ns began - SILENT-MS GRACE-MS + NS-PER-MS * < TTRUE
   conn HEAR WS-OPCODE:PING s" " GOT
   conn ENDS ;

: OVERFLOW-CASE ( -- )
   s" a queue past its bound" SECTION
   OPENS {: still:TCP4:connection stuck:WS:socket :}
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   stuck FLOOD-PUSH PUSH-BYTES / PUSHES E-WS-CLOSED T=
   stuck LOST CLOSED-WITH
   conn WS-OPCODE:TEXT s" unhindered" EXCHANGE
   conn FAREWELL
   still TCP4:CLOSE DROP-STATUS ;

\ The socket past START's limit is refused 503, and no socket exists for it;
\ the body read behind its head is dropped with its connection.
: CAPACITY-CASE ( -- )
   s" the socket past capacity" SECTION
   OPEN-ALL
   s" GET" s" /ws" REQ-START GOOD-LINES S\" \r\n" REQ+
   REQ-SEND {: conn:TCP4:connection :}
   AWAIT-ROUTE
   HEAD$ s" HTTP/1.1 503 " STR:STARTS-WITH? TTRUE
   REFUSED-WITH @ 503 T=
   conn TCP4:CLOSE DROP-STATUS
   0 STREAM-AT ! 0 STREAM-U !
   CROWD 1- CONNS @ WS-OPCODE:TEXT s" last" EXCHANGE
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
   [: 0 SIZED WS:START ;] E-WS-LIMIT TTHROWSQ
   [: $100000000 SIZED WS:START ;] E-WS-LIMIT TTHROWSQ
   CROWD SIZED WS:START
   OPENS drop {: conn:TCP4:connection :}
   mono-ns {: began:n :}
   WS:STOP
   mono-ns began - IDLE-STOP-MS NS-PER-MS * < TTRUE
   conn TCP4:CLOSE DROP-STATUS ;

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
   CROWD SIZED WS:START
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   OPENS drop {: other:TCP4:connection :}
   conn true WS-OPCODE:TEXT s" bye" SAY
   conn HEAR 4000 GOT-CLOSE
   [: WS:STOP 0 TASK:RETURN ;] STOP-TASK TASK:ACTIVATE
   other HEAR 1001 GOT-CLOSE
   conn true WS-OPCODE:CLOSE 4000 STATUS$ SAY
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
   2 SIZED WS:START
   OPENS drop {: first:TCP4:connection :}
   OPENS drop {: second:TCP4:connection :}
   WS:STOP
   first TCP4:CLOSE DROP-STATUS
   second TCP4:CLOSE DROP-STATUS
   1 SIZED WS:START
   [: SEND-STALE ;] E-WS-CLOSED TTHROWSQ
   [: CLOSE-STALE ;] catch 0 T=
   WS:STOP ;

\ WS started for two sockets, one of them open, and a handshake on /stall that
\ holds the other's slot: the open socket's connection, then the handshake's.
: HOLD-SLOT ( -- TCP4:connection TCP4:connection )
   2 SIZED WS:START
   OPENS drop {: first:TCP4:connection :}
   0 STALLING !
   0 REFUSED-WITH !
   s" GET" s" /stall" REQ-START GOOD-LINES S\" \r\n" REQ+
   DIAL {: conn:TCP4:connection :}
   conn REQ$ SEND-RAW
   [: STALLING @ ;] AWAITED drop
   first conn ;

\ A handshake that holds a slot until WS's task has left is answered 503 by
\ the stop, which waits for it: it never joins the next run, here one whose
\ limit leaves its slot out.
: STRAGGLER-CASE ( -- )
   s" a stop while a handshake holds its slot" T-LABEL
   HOLD-SLOT {: first:TCP4:connection conn:TCP4:connection :}
   WS:STOP
   1 SIZED WS:START
   AWAIT-ROUTE
   REFUSED-WITH @ 503 T=
   conn READ-HEAD
   HEAD$ s" HTTP/1.1 503 " STR:STARTS-WITH? TTRUE
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
   AWAIT-ROUTE
   REFUSED-WITH @ 503 T=
   [: STOP-ENDED ;] AWAITED drop
   TASK-CODE 0 T=
   conn TCP4:CLOSE DROP-STATUS
   first TCP4:CLOSE DROP-STATUS
   0 STREAM-AT ! 0 STREAM-U ! ;

: GATED-START ( -- )
   1 GATE-READY atomic!
   begin GATE-OPEN atomic@ 0 <> until
   [: CFG WS:START ;] catch TASK:RETURN ;

: START-RACE-CASE ( -- )
   s" two starts at once" T-LABEL
   0 GATE-READY atomic!
   0 GATE-OPEN atomic!
   2 SIZED drop
   [: GATED-START ;] STOP-TASK TASK:ACTIVATE
   [: GATE-READY atomic@ ;] AWAITED drop
   1 GATE-OPEN atomic!
   [: CFG WS:START ;] catch {: here:n :}
   [: STOP-ENDED ;] AWAITED drop
   TASK-CODE {: there:n :}
   here there max 0 T=
   here there min E-WS-STATE T=
   [: WS:STOP ;] catch 0 T= ;

\ A handler's WS:STOP, on a request worker, waits out its bound for its own
\ call and returns: the peer reads 1001, and the stream ends once that call
\ has returned and retired its slot, so a new run starts and stops.
: HANDLER-STOP-CASE ( -- )
   s" a stop from a handler" SECTION
   -1 HANDLER-STOP atomic!
   2 SIZED WS:START
   OPENS drop {: conn:TCP4:connection :}
   conn true WS-OPCODE:TEXT s" stop" SAY
   conn HEAR 1001 GOT-CLOSE
   conn ENDS
   s" a handler's stop returns" T-LABEL
   HANDLER-STOP atomic@ 0 T=
   [: 1 SIZED WS:START ;] catch 0 T=
   [: WS:STOP ;] catch 0 T= ;

\ A stop while a handler call blocks past its bound returns within the bound,
\ its peer sent the 1001. A start is E-WS-STATE until that call returns,
\ whose end closes the socket and retires its slot, the handler untold; a
\ start then succeeds.
: BLOCKED-STOP-CASE ( -- )
   s" a stop while a handler call blocks" SECTION
   0 HOLDING atomic!
   2 SIZED WS:START
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:TEXT s" hold" SAY
   [: HOLDING atomic@ ;] AWAITED drop
   mono-ns {: began:n :}
   WS:STOP
   s" a stop past a blocked call returns within its bound" T-LABEL
   mono-ns began - STOP-BOUND-MS GRACE-MS + NS-PER-MS * < TTRUE
   conn HEAR 1001 GOT-CLOSE
   s" a start while that call is out" T-LABEL
   [: 1 SIZED WS:START ;] E-WS-STATE TTHROWSQ
   UNHELD TASK:SIGNAL-SEM
   conn ENDS
   s" a start once that call has retired its slot" T-LABEL
   sock CLOSED-OF 0 T=
   [: 1 SIZED WS:START ;] catch 0 T=
   [: WS:STOP ;] catch 0 T= ;

\ WS started again, 1, or 0 while a call a stop passed its bound still holds
\ its slot (E-WS-STATE), so the next case starts on a WS no call holds.
: RESTARTED ( -- n )
   [: 1 SIZED WS:START ;] catch {: code:n :}
   code E-WS-STATE = if 0 exit then
   code 0<> if code throw then
   1 ;

\ A stop while a handler call runs briefly: the stop closes the socket under
\ 1001, which ends the call, and the handler is told `closed`, under that
\ 1001, after the call has returned, unless the stop passed its bound first.
\ The stop's own close ends the call, so no step this task takes need fit
\ inside the bound. WS:STOP runs on its own task while this one reads.
: BRIEF-STOP-CASE ( -- )
   s" a stop while a handler call runs briefly" SECTION
   0 PROBING atomic!
   0 PROBE-END atomic!
   2 SIZED WS:START
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn true WS-OPCODE:TEXT s" brief" SAY
   [: PROBING atomic@ ;] AWAITED drop
   mono-ns {: began:n :}
   [: WS:STOP 0 TASK:RETURN ;] STOP-TASK TASK:ACTIVATE
   conn HEAR-PAST-PROBES 1001 GOT-CLOSE
   conn ENDS
   [: STOP-ENDED ;] AWAITED drop
   mono-ns began - {: took:n :}
   TASK-CODE 0 T=
   s" the call returned once the stop closed its socket" T-LABEL
   PROBE-END atomic@ E-WS-CLOSED T=
   s" the handler told `closed` after its call, or the stop passed its bound" T-LABEL
   sock CLOSED-OF {: code:n :}
   code 1001 = code 0= took STOP-BOUND-MS NS-PER-MS * >= and or TTRUE
   ['] RESTARTED AWAITED drop
   WS:STOP ;

\ The line WS logs after its stamp when its task ends on this code.
: TASK-LINE ( n -- )
   {: code:n :}
   SB-RESET
   s"  error ws: task failed, throw " SB-APPEND
   code ERR-THROW ;

\ AIO is filled while a handshake holds its slot, so the poll WS's task makes
\ for the new socket is refused.
: FAULTING ( -- )
   0 STALLING !
   s" GET" s" /fill" REQ-START GOOD-LINES S\" \r\n" REQ+
   DIAL {: conn:TCP4:connection :}
   conn REQ$ SEND-RAW
   [: STALLING @ ;] AWAITED drop
   AIO-FILL:FILL {: code:n :}
   s" AIO refuses the poll past its ceiling" T-LABEL
   code AIO-FILL:CEILING T=
   code TASK-LINE
   FILLED TASK:SIGNAL-SEM
   AWAIT-ROUTE
   [: ERR-WRITTEN ;] AWAITED drop
   AIO-FILL:UNFILL
   conn TCP4:CLOSE DROP-STATUS
   0 STREAM-AT ! 0 STREAM-U ! ;

\ A throw that ends WS's task, here the poll AIO refuses at its ceiling, is
\ one line on stderr with its code, and WS:STOP still ends the run.
: FAULT-CASE ( -- )
   s" a task that fails" T-LABEL
   s" build" FS:MAKE-DIRS
   0 ERR-WANT-U !
   2 SIZED WS:START
   ERR-PATH$ STDERR:CAPTURE
   [: FAULTING ;] [: AIO-FILL:UNFILL STDERR:;CAPTURE ;] finally
   [: WS:STOP ;] catch 0 T=
   s" the task's fault reported once" T-LABEL
   ERR-READ UNSTAMP
   ERR$ ERR-WANT$ T$=
   ERR-PATH$ FS:REMOVE-FILE ;

: SEND-HELD ( -- )
   0 HELD @ s" wrapped" WS:SEND-TEXT ;

\ A slot's generation wraps within a handle's upper half: the socket after the
\ one that carried the last generation is current, and a send through it is
\ queued.
: WRAP-CASE ( -- )
   s" a slot whose generation wraps" T-LABEL
   1 SIZED WS:START
   0 WS-TEST:LAST-GEN
   OPENS {: conn:TCP4:connection sock:WS:socket :}
   conn TCP4:CLOSE DROP-STATUS
   sock LOST CLOSED-WITH
   OPENS drop {: next:TCP4:connection :}
   [: SEND-HELD ;] catch 0 T=
   next TCP4:CLOSE DROP-STATUS
   WS:STOP ;


: WANT$ ( -- str )
   WANT-BUF WANT-U @ SPAN:TAKE SPAN:$ ;

: W ( str -- ) {: s:str :}
   s WANT-BUF WANT-U @ SPAN:SKIP SPAN:COPY
   s STR:LEN WANT-U +!
   LF WANT-BUF WANT-U @ SPAN:U8!
   1 WANT-U +! ;

: W-FAREWELL ( -- )
   s" > close 2 03e8" W
   s" < close 2 03e8" W
   s" < end of stream" W ;

: W-FAILED ( str -- )
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
   s" == opened comes first, with the route's value" W
   s" > text 5 6669727374" W
   s" < text 5 6669727374" W
   W-FAREWELL
   s" handler opened 7" W
   s" handler text 7" W
   s" handler closed 7" W
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
   s" == a handler that blocks" W
   s" > text 4 77616974" W
   s" > text 4 6e657874" W
   s" > text 2 6869" W
   s" < text 2 6869" W
   s" < answered while the other handler blocks" W
   s" < text 6 776169746564" W
   s" < text 4 6e657874" W
   s" < next handled after wait returned" W
   W-FAREWELL
   W-FAREWELL
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
   s" == a peer that falls silent" W
   s" < ping 0" W
   s" < end of stream" W
   s" == a stop" W
   s" < close 2 03e9" W-FAILED
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
   s" < end of stream" W
   s" == a stop while a handler call blocks" W
   s" > text 4 686f6c64" W
   s" < close 2 03e9" W
   s" < end of stream" W
   s" == a stop while a handler call runs briefly" W
   s" > text 5 6272696566" W
   s" < close 2 03e9" W
   s" < end of stream" W ;

\ The artifact as another reader will find it: read back from its file and
\ compared whole with the transcript this file expects.
: TRANSCRIPT-CASE ( -- )
   s" the transcript" T-LABEL
   s" build" FS:MAKE-DIRS
   s" build/ws-transcript.txt" SCRIPT$ FS:WRITE-ALL
   s" build/ws-transcript.txt" BACK-BUF SPAN:$ str> FS:READ-ALL {: got:n :}
   WANT-TRANSCRIPT
   BACK-BUF got SPAN:TAKE SPAN:$ WANT$ T$= ;


10 constant RETRY-MS

\ AIO:STOP is E-AIO-BUSY until the loop has drained every record, so the stop
\ is retried until it has.
: AIO-STOP ( -- )
   begin
      [: AIO:STOP ;] catch {: code:n :}
      code 0= if exit then
      code E-AIO-BUSY <> if code throw then
      RETRY-MS >MS TASK:SLEEP
   again ;

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
   FIRST-CALL-CASE
   HEADER-FAULTS MESSAGE-FAULTS BOUND-CASES
   CLOSE-STATUS-CASES CLOSE-CASES SERVER-CLOSE-CASE TWICE-CASE THROW-CASE
   MANY-CASE BLOCK-CASE PUSH-CASE BOTH-CASE BURST-CASE HELD-CASE OVERFLOW-CASE STALL-CASE
   SILENT-CASE ;

public

\ The loop first: every wait the server, WS's task and this client make rides
\ it. WS stops before the server, and the server's stop kills no task: no
\ socket holds a worker.
: RUN ( -- )
   T-RESET
   FDS-WANT AIO-FILL:RAISE-FDS
   0 SCRIPT-U !
   0 CLOSED-N !
   CLOSED-LOCK TASK:FACILITY-INIT
   FILL-BUFFERS
   0 ACCEPTED TASK:SEMAPHORE-INIT
   0 FILLED TASK:SEMAPHORE-INIT
   0 UNHELD TASK:SEMAPHORE-INIT
   0 RELEASED TASK:SEMAPHORE-INIT
   AIO:START
   INSTALL-ROUTES
   SERVER-CONFIG HTTP:START
   CROWD SIZED WS:START
   CASES
   CROWD SIZED RESTART
   STOP-CASE STALE-CASE IDLE-STOP-CASE CLOSING-STOP-CASE
   SHRINK-CASE STRAGGLER-CASE STOP-TWICE-CASE START-RACE-CASE HANDLER-STOP-CASE
   BLOCKED-STOP-CASE BRIEF-STOP-CASE
   FAULT-CASE
   WRAP-CASE
   HTTP:STOP
   s" every handler returned" T-LABEL
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   HTTP:KILLED-TASKS 0 T=
   AIO-STOP
   ACCEPTED TASK:SEMAPHORE-DESTROY
   FILLED TASK:SEMAPHORE-DESTROY
   UNHELD TASK:SEMAPHORE-DESTROY
   RELEASED TASK:SEMAPHORE-DESTROY
   TRANSCRIPT-CASE ;

;package

WS-TEST:RUN
T-REPORT
