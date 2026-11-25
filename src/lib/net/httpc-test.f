\ httpc-test.f - focused tests for src/lib/net/httpc.f over loopback HTTP/1.1.
\ One process holds both peers: a server task accepts on a loopback TCP4
\ listener bound to port 0 and answers each request with the bytes its case
\ scripts, counting accepts, requests per connection and request bytes and
\ keeping the last request head, while the main task drives HTTPC. Every
\ exchange has deadlines, so a server that fails to answer ends its case
\ `timeout` and not in a hang. A scripted close is a half-close: the server
\ sends its FIN and still counts any request that arrives after it. The
\ server counts the answers it has written, so a case reads an answer only
\ once all of it is ready and no receive waits. A room the size of an
\ answer's head, or of its head and body, bounds the head's read, so the body,
\ or a byte past it, stays on the socket.
\ The https cases speak to TEST-TLS-PEER's `openssl s_server` and its
\ certificates. The plain server reads a ClientHello as a request: an https
\ origin at it fails its handshake `timeout` at the stall deadline when it
\ answers nothing, and `handshake` when it closes or answers plain HTTP.
\ Without -www, s_server sends in one record what the test writes to its
\ stdin: two answers ride one session, and an answer whose record carries
\ bytes past its body leaves them in the TLS session, so the next request
\ dials anew. On TLS 1.2 with -www, a trusted GET / reads its answer to its
\ close_notify, an unrelated CA or an origin naming another host fails the
\ greeting with `trust`, and a relay task between HTTPC and s_server closes
\ HTTPC's socket in place of the server's close_notify, so an answer read to
\ the close fails `cut`. An https origin trusting a path TLS cannot read
\ fails `trust` at each SEND's first NEXT and leaves no descriptor open.
\ NEXT-UNLESS's cases poll a pipe's read end, readable once the case writes
\ a byte to it. On TLS 1.3 s_server sends its session tickets after the
\ handshake, so they reach the socket before any answer.
\ TEST-TABLE compares TEST-FSM:TABLE-MODULE's text for the LIFECYCLE table
\ with spec/HttpcTable.tla whole, so the table spec/HttpcFollows.tla ties to
\ the model spec/Httpc.tla is the code's.
\ Run: bin/hb --load src/lib/net/httpc-test.f </dev/null
HB-TARGET-LINUX? ?TARGET
require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/test/assert.f
require src/lib/test/wait.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/net/tcp4.f
require src/lib/net/httpc.f
require src/lib/arena.f
require src/lib/test-fsm.f
require src/lib/net/test-tls-peer.f
require src/lib/ffi-abi.f

\ White-box: the conn's socket, so a case waits for the server's FIN to reach
\ it before it sends again.
package HTTPC
: HTTPC-TEST:FD ( ^conn -- n ) CONN-FD @ ;
;package

package HTTPC-TEST

private

CAST: BLEN>N ( NUM:byte-len -- n )

PROCESS-SYMBOLS
FUNCTION: PEEK-CALL recv ( n ^u8 n n -- n ) ;FUNCTION
FUNCTION: OPTION-CALL getsockopt ( n n n ^u8 ^u8 -- i32 ) ;FUNCTION

$1000 constant REQ-CAP
$10000 constant ROOM-CAP
64 constant SMALL-ROOM                  \ holds a request head, not BIG-HEAD$
$4000 constant GOT-CAP                  \ s_server's status page fits
$40 constant ORIGIN-CAP
5000 constant TOTAL-MS
5000 constant STALL-MS
5000 constant INPUT-MS
100 constant SHORT-STALL-MS             \ the stall the timeout case expects
8 constant CONN-CAP                     \ connections one case may make
10 constant SIZED-SPAN                  \ shorter than the sized body
4 constant CHUNKED-SPAN                 \ crosses every chunk boundary
$40 constant WIDE-SPAN
$01 constant POLLIN
$42 constant PEEK-FLAGS                 \ MSG_DONTWAIT | MSG_PEEK
1000000 constant NS-PER-MS
100 constant PAST-MS                    \ the total deadline a case outlives
5 constant RECORD-HEAD                  \ a TLS record's header
22 constant HANDSHAKE-TYPE              \ the content type of a ClientHello
$7280 negate constant CONN-EOF          \ MBEDTLS_ERR_SSL_CONN_EOF
$7200 negate constant INVALID-RECORD    \ MBEDTLS_ERR_SSL_INVALID_RECORD
6 constant IPPROTO-TCP
1 constant TCP-NODELAY

create REQ-BUF REQ-CAP allot
create SEEN-BUF REQ-CAP allot
create ROOM-BUF ROOM-CAP allot
create SPAN-BUF WIDE-SPAN allot
create GOT-BUF GOT-CAP allot
create ORIGIN-BUF ORIGIN-CAP allot
create INPUT-PFD 8 allot
create PEEK-BUF REQ-CAP allot
create OPT-BUF 4 allot                  \ getsockopt's value and its length
create OPT-LEN 4 allot

variable LISTENER-CELL
variable PORT-CELL
variable READY
variable STOP
variable DONE
variable ACCEPTS
variable CLOSES                         \ connections the server has closed
variable REQUESTS
variable REQ-BYTES
variable REQ-U
variable SEEN-U                         \ the last request head answered
variable ORIGIN-U
variable GOT-U
variable WIDEST                         \ the largest piece one NEXT answered
variable LAST-STATUS
variable FAULTS
variable WRITTEN                        \ answers the server has written
TYPED-VARIABLE LAST-FAULT HTTPC:fault
CONN-CAP TYPED-BUFFER PER-CONN n

\ What the server answers in the running case.
ENUM script sized chunked closing interim no-content reuse idle-close
   unread cut-head cut-body silent bad-status bad-length bad-chunk big-head
   trailing trailer big-trailer long-size interims close-asked minor-zero
   wire-sized wire-chunked stale-byte not-http garbage bad-code low-code
   high-code held ;ENUM
TYPED-VARIABLE SCRIPT script
TYPED-VARIABLE LINK HTTPC:conn
TYPED-VARIABLE SPARE HTTPC:conn          \ a record OPEN finds full of bytes

CAST: CONN>BYTES ( ^HTTPC:conn -- ^u8 )

\ SPARE full of non-zero bytes, as OPEN may find a record.
: SPOIL ( -- )
   [type] HTTPC:conn width>n 0 do $A5 SPARE CONN>BYTES i + c! loop ;

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
: LOW-CODE$ ( -- str ) S\" HTTP/1.1 099 Low\r\nContent-Length: 0\r\n\r\n" ;
: HIGH-CODE$ ( -- str ) S\" HTTP/1.1 600 High\r\nContent-Length: 0\r\n\r\n" ;
: BAD-LENGTH$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: abc\r\n\r\nok" ;
: BAD-CHUNK$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\nok\r\n0\r\n\r\n" ;
: TRAILER$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n0\r\nX-Trailer: 1\r\n\r\n" ;
\ Three trailer lines, each shorter than SMALL-ROOM and together longer.
: BIG-TRAILER$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n0\r\nX-A: abcdefghijklmnopqrstuvwxyz\r\nX-B: abcdefghijklmnopqrstuvwxyz\r\nX-C: abcdefghijklmnopqrstuvwxyz\r\n\r\n" ;
\ A chunk-size line whose extension makes it longer than SMALL-ROOM.
: LONG-SIZE$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5;x=abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz\r\nhello\r\n0\r\n\r\n" ;
\ Three interim heads, each shorter than SMALL-ROOM and together longer,
\ then FINAL-HEAD$'s answer.
: INTERIMS$ ( -- str )
   S\" HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" ;
: BIG-HEAD$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nX-Pad: abcdefghijklmnopqrstuvwxyz\r\nContent-Length: 0\r\n\r\n" ;
\ FIRST$ and two bytes past its body, sent in one write.
: TRAILING$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nfirstXX" ;
: CLOSE-ASKED$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 5\r\nConnection: close\r\n\r\nfirst" ;
: MINOR-ZERO$ ( -- str ) S\" HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nfirst" ;

\ Heads padded to hold the request head, which shares the room.
: WIRE-SIZED$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 26\r\nX-Pad: abcdefghijklmnopq\r\n\r\nabcdefghijklmnopqrstuvwxyz" ;
: WIRE-SIZED-HEAD$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 26\r\nX-Pad: abcdefghijklmnopq\r\n\r\n" ;
: WIRE-CHUNKED$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nX-Pad: abcdefghijklmnopq\r\n\r\n5\r\nhello\r\n7;x=1\r\n, world\r\n0\r\nX-Trailer: 1\r\n\r\n" ;
: WIRE-CHUNKED-HEAD$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nX-Pad: abcdefghijklmnopq\r\n\r\n" ;
\ A whole answer and one byte past its body, sent in one write.
: STALE-BYTE$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 5\r\nX-Pad: abcdefghijklmnopq\r\n\r\nfirstX" ;

: TWO$ ( n -- str ) 0= if FIRST$ else SECOND$ then ;

\ The answer to the case's request K, and whether the server closes after it.
: REPLY ( n -- str bool ) {: k:n :}
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
      trailer OF TRAILER$ false ENDOF
      big-trailer OF BIG-TRAILER$ false ENDOF
      long-size OF LONG-SIZE$ false ENDOF
      interims OF INTERIMS$ false ENDOF
      close-asked OF k 0= if CLOSE-ASKED$ else SECOND$ then false ENDOF
      minor-zero OF k 0= if MINOR-ZERO$ else SECOND$ then false ENDOF
      wire-sized OF WIRE-SIZED$ false ENDOF
      wire-chunked OF WIRE-CHUNKED$ false ENDOF
      stale-byte OF k 0= if STALE-BYTE$ else SECOND$ then false ENDOF
      not-http OF S\" not http\r\n" true ENDOF
      garbage OF s" garbage" true ENDOF
      bad-code OF s" HTTP/1.1 2x" true ENDOF
      low-code OF LOW-CODE$ false ENDOF
      high-code OF HIGH-CODE$ false ENDOF
      held OF k 0= if s" " else SECOND$ then false ENDOF
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
   REQ-BUF REQ-U @ >str S\" \r\n\r\n" STR:FIND-SUB MATCH option
      none OF 0 ENDOF
      some OF IDX>N 4 + ENDOF
   ;MATCH ;

\ The body length a request head of H bytes declares; 0 for none.
: BODY-U ( n -- n ) {: h:n :}
   REQ-BUF h >str s" Content-Length: " STR:FIND-SUB MATCH option
      none OF 0 ENDOF
      some OF IDX>N 16 + {: at:n :}
         REQ-BUF at + h at - >str 13 STR:INDEX-OF MATCH option
            none OF 0 ENDOF
            some OF IDX>N REQ-BUF at + swap >str STR:PARSE-POS
               MATCH option none OF 0 ENDOF some OF ENDOF ;MATCH
            ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

\ The length of the TLS record at the address, its header included.
: RECORD-SIZE ( ^u8 -- n ) {: a:^ :}
   a 3 + c@ 8 lshift  a 4 + c@ or RECORD-HEAD + ;

\ The rest of a ClientHello's record; false at the client's close.
: READ-HELLO ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   begin REQ-U @ RECORD-HEAD < while
      conn REQ-CHUNK 0= if false exit then
   repeat
   begin REQ-U @ REQ-BUF RECORD-SIZE < while
      conn REQ-CHUNK 0= if false exit then
   repeat true ;

\ One whole request, its bytes counted; false at the client's close. A
\ ClientHello is one whole once its record is.
: READ-REQUEST ( TCP4:connection -- bool ) {: conn:TCP4:connection :}
   0 REQ-U !
   conn REQ-CHUNK 0= if false exit then
   REQ-BUF c@ HANDSHAKE-TYPE = if conn READ-HELLO exit then
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
      1 REQUESTS atomic-add REPLY {: reply:str shut:bool :}
      conn reply TCP4:WRITE PEER-STATUS
      1 WRITTEN atomic-add drop
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
   conn TCP4:CLOSE PEER-STATUS
   1 CLOSES atomic-add drop ;

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

: ORIGIN$ ( -- str ) ORIGIN-BUF ORIGIN-U @ >str ;

: ORIGIN! ( n -- ) {: port:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND port FMT:SB-U
   SB$ {: s:str :}
   s STR:DATA ORIGIN-BUF s STR:LEN BYTE-COPY  s STR:LEN ORIGIN-U ! ;

\ The server's script for a fresh case, its counts zeroed.
: SCRIPT-IS ( script -- )
   SCRIPT !
   0 ACCEPTS ! 0 CLOSES ! 0 REQUESTS ! 0 REQ-BYTES ! 0 SEEN-U ! 0 WRITTEN !
   CONN-CAP 0 do 0 i PER-CONN ! loop ;

\ A fresh case: the script, zeroed counts and a conn holding nothing, with a
\ room of ROOM bytes.
: BEGIN-IN ( script n -- ) {: room:n :}
   SCRIPT-IS
   LINK ORIGIN$ s" " ROOM-BUF room HTTPC:OPEN
   s" OPEN takes the loopback origin" T-LABEL TTRUE ;

: BEGIN-CASE ( script -- ) ROOM-CAP BEGIN-IN ;

: END-CASE ( -- ) LINK HTTPC:CLOSE ;

: PREPARE ( str str -- ) {: m:str t:str :}
   LINK m t HTTPC:REQUEST
   LINK TOTAL-MS STALL-MS HTTPC:DEADLINES! ;

: ASK ( -- ) s" GET" s" /case" PREPARE  LINK s" " HTTPC:SEND ;

: GOT$ ( -- str ) GOT-BUF GOT-U @ >str ;

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

\ Reads the answer to its end in NEXT-UNLESS spans of SPAN bytes, each
\ polling the descriptor too.
: READ-UNLESS ( n fd -- ) {: span:n f:fd :}
   ANSWER-RESET
   begin LINK SPAN-BUF span >LEN f HTTPC:NEXT-UNLESS ANSWERED? until ;

: STIR ( fd -- ) s" x" FD:WRITE-FULL ;

: SHUT-PIPE ( fd fd -- ) FD>N close FD>N close ;

\ One NEXT of SPAN bytes, which answers a piece of the body.
: ONE-PIECE ( n -- ) {: span:n :}
   ANSWER-RESET
   LINK SPAN-BUF span >LEN HTTPC:NEXT ANSWERED? TFALSE ;

: ANSWER-IS ( n str -- ) {: status:n s:str :}
   FAULTS @ 0<> if
      s" httpc-test: NEXT failed: " type LAST-FAULT @ HTTPC:FAULT$ type cr
   then
   FAULTS @ 0 T=  LAST-STATUS @ status T=  GOT$ s T$= ;

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
   SEEN-BUF SEEN-U @ >str s" Content-Length" STR:FIND-SUB MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ A NEXT after CLOSE fails `send` and leaves no socket held.
: CLOSED-NEXT ( -- )
   LINK HTTPC:CLOSE  ANSWER-RESET
   LINK SPAN-BUF WIDE-SPAN >LEN HTTPC:NEXT ANSWERED? TTRUE
   HTTPC-FAULT:SEND true FAULT-IS
   LINK HTTPC-TEST:FD -1 T= ;

\ Blocks until a byte or the server's FIN reaches the conn's socket.
: AWAIT-INPUT ( -- bool )
   LINK HTTPC-TEST:FD INPUT-PFD LE:U32!
   POLLIN INPUT-PFD 4 + LE:U16!  0 INPUT-PFD 6 + LE:U16!
   INPUT-PFD 1 INPUT-MS poll 1 = ;


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

\ No OPEN between CLOSE and the second request.
: TEST-CLOSE-REDIAL ( -- )
   construct script reuse BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" the answer before CLOSE" T-LABEL
   200 s" first" ANSWER-IS
   LINK HTTPC:CLOSE
   ASK WIDE-SPAN READ-ALL
   s" the request after CLOSE dials the same origin again" T-LABEL
   200 s" second" ANSWER-IS
   ACCEPTS @ 2 T=
   END-CASE ;

: TEST-IDLE-CLOSE ( -- )
   construct script idle-close BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s" the answer before the server closes the kept connection" T-LABEL
   200 s" first" ANSWER-IS
   REQ-BYTES @ {: one:n :}
   s" the server's FIN reaches the idle connection" T-LABEL
   AWAIT-INPUT TTRUE
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
   SEEN-BUF SEEN-U @ >str S\" \r\nContent-Length: 0\r\n" STR:FIND-SUB
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH TTRUE
   END-CASE ;

\ The conn's socket's TCP_NODELAY.
: NODELAY ( -- n )
   4 OPT-LEN LE:U32!
   LINK HTTPC-TEST:FD IPPROTO-TCP TCP-NODELAY OPT-BUF OPT-LEN OPTION-CALL
   0 T=  OPT-BUF LE:U32@ ;

: TEST-NODELAY ( -- )
   construct script sized BEGIN-CASE
   s" POST" s" /case" PREPARE  LINK ALPHABET$ HTTPC:SEND
   SIZED-SPAN READ-ALL
   s" a POST with a body is answered" T-LABEL
   200 ALPHABET$ ANSWER-IS
   s" the conn's socket has Nagle's algorithm off" T-LABEL
   NODELAY 1 T=
   END-CASE ;

\ An answer that forbids keeping its connection; the next request dials anew.
: SHUT-BY ( script str -- ) {: sc:script s:str :}
   sc BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s T-LABEL
   200 s" first" ANSWER-IS
   ASK WIDE-SPAN READ-ALL
   200 s" second" ANSWER-IS
   ACCEPTS @ 2 T=
   END-CASE ;

: TEST-SHUT ( -- )
   construct script close-asked
   s" the request after an answer asking `Connection: close` dials anew"
   SHUT-BY
   construct script minor-zero
   s" the request after an HTTP/1.0 answer dials anew" SHUT-BY ;

\ A room of the head's size leaves the whole body on the socket.
: TEST-WIRE-BODY ( -- )
   construct script wire-sized WIRE-SIZED-HEAD$ STR:LEN BEGIN-IN
   ASK SIZED-SPAN READ-ALL
   s" a Content-Length body read from the wire arrives whole" T-LABEL
   200 ALPHABET$ ANSWER-IS
   END-CASE
   construct script wire-chunked WIRE-CHUNKED-HEAD$ STR:LEN BEGIN-IN
   ASK CHUNKED-SPAN READ-ALL
   s" a chunked body read from the wire decodes whole" T-LABEL
   200 s" hello, world" ANSWER-IS
   END-CASE ;

\ The room holds the answer but not the byte past it, so the connection is
\ kept with that byte unread on its socket.
: TEST-STALE-BYTE ( -- )
   construct script stale-byte STALE-BYTE$ STR:LEN 1- BEGIN-IN
   ASK WIDE-SPAN READ-ALL
   s" an answer with a byte past it on the socket" T-LABEL
   200 s" first" ANSWER-IS
   s" the byte past the answer reaches the kept connection" T-LABEL
   AWAIT-INPUT TTRUE
   ASK WIDE-SPAN READ-ALL
   s" the probe finds a byte on the kept connection and dials anew" T-LABEL
   200 s" second" ANSWER-IS
   ACCEPTS @ 2 T=
   END-CASE ;

\ The server has read the request and holds its answer when the pipe
\ becomes readable; the second connection's request is answered.
: TEST-WOKEN ( -- )
   construct script held BEGIN-CASE
   FD:PIPE-PAIR {: r:fd w:fd :}
   ASK  WRITTEN 1 SERVER-TASK TEST-WAIT:FOR-CELL
   w STIR  WIDE-SPAN r READ-UNLESS
   s" a readable descriptor ends a held answer `woken`, hanging up" T-LABEL
   HTTPC-FAULT:WOKEN true FAULT-IS
   LINK HTTPC-TEST:FD -1 T=
   ASK WIDE-SPAN READ-ALL
   s" the request after `woken` dials again and gets its answer" T-LABEL
   200 s" second" ANSWER-IS
   ACCEPTS @ 2 T=
   w r SHUT-PIPE
   END-CASE ;

: TEST-UNWOKEN ( -- )
   construct script sized BEGIN-CASE
   FD:PIPE-PAIR {: r:fd w:fd :}
   ASK SIZED-SPAN r READ-UNLESS
   s" NEXT-UNLESS on an unreadable descriptor answers as NEXT" T-LABEL
   200 ALPHABET$ ANSWER-IS
   w r SHUT-PIPE
   END-CASE ;

\ A loopback port bound but not listening rejects every connection.
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

\ The whole answer is ready at the conn's socket when NEXT starts, after the
\ total deadline has passed, so no receive waits.
: TEST-TOTAL ( -- )
   construct script trailer BEGIN-CASE
   LINK s" GET" s" /case" HTTPC:REQUEST
   LINK PAST-MS 0 HTTPC:DEADLINES!
   LINK s" " HTTPC:SEND
   WRITTEN 1 SERVER-TASK TEST-WAIT:FOR-CELL
   s" the whole answer reaches the client" T-LABEL
   AWAIT-INPUT TTRUE
   PAST-MS >MS TASK:SLEEP
   WIDE-SPAN READ-ALL
   s" an answer read after the total deadline fails `timeout`" T-LABEL
   HTTPC-FAULT:TIMEOUT true FAULT-IS
   END-CASE ;

: MALFORMED-BY ( script str -- ) {: sc:script s:str :}
   sc BEGIN-CASE
   ASK WIDE-SPAN READ-ALL
   s T-LABEL
   HTTPC-FAULT:MALFORMED false FAULT-IS
   END-CASE ;

: TEST-MALFORMED ( -- )
   construct script bad-status s" a bad status line fails `malformed`"
   MALFORMED-BY
   construct script bad-length
   s" a non-numeric Content-Length fails `malformed`" MALFORMED-BY
   construct script bad-chunk s" a bad chunk size fails `malformed`"
   MALFORMED-BY
   construct script not-http
   s" a line that is not HTTP, then the close, fails `malformed`" MALFORMED-BY
   construct script garbage
   s" bytes that cannot begin HTTP, then the close, fail `malformed`"
   MALFORMED-BY
   construct script bad-code
   s" a status code that is not digits, then the close, fails `malformed`"
   MALFORMED-BY
   construct script low-code s" a status below 100 fails `malformed`"
   MALFORMED-BY
   construct script high-code s" a status above 599 fails `malformed`"
   MALFORMED-BY ;

\ A request with a stall only, in a room of SMALL-ROOM bytes, whose answer
\ fails `oversize`.
: OVERSIZE-BY ( script str -- ) {: sc:script s:str :}
   sc SMALL-ROOM BEGIN-IN
   LINK s" GET" s" /case" HTTPC:REQUEST
   LINK 0 STALL-MS HTTPC:DEADLINES!
   LINK s" " HTTPC:SEND  WIDE-SPAN READ-ALL
   s T-LABEL
   HTTPC-FAULT:OVERSIZE false FAULT-IS
   END-CASE ;

: TEST-OVERSIZE ( -- )
   construct script big-head SMALL-ROOM BEGIN-IN
   ASK WIDE-SPAN READ-ALL
   s" an answer head larger than the room fails `oversize`" T-LABEL
   HTTPC-FAULT:OVERSIZE false FAULT-IS
   END-CASE
   construct script big-trailer
   s" trailers larger than the room fail `oversize` with a stall only"
   OVERSIZE-BY
   construct script long-size
   s" a size line larger than the room fails `oversize` with a stall only"
   OVERSIZE-BY
   construct script interims
   s" interim heads larger than the room fail `oversize` with a stall only"
   OVERSIZE-BY ;

\ A marked request's first NEXT answers its fault.
: REJECTED ( HTTPC:fault -- ) {: f:HTTPC:fault :}
   LINK s" " HTTPC:SEND  WIDE-SPAN READ-ALL
   f false FAULT-IS ;

\ The good request after the rejected ones accepts every connection the
\ rejected ones would have made, so ACCEPTS counts them.
: TEST-REJECTED ( -- )
   construct script reuse SMALL-ROOM BEGIN-IN
   s" GET" s" /case" PREPARE  LINK ALPHABET$ HTTPC:HEADER+
   s" a request head larger than the room fails `oversize`" T-LABEL
   HTTPC-FAULT:OVERSIZE REJECTED
   s" GET" s" /case" PREPARE  LINK S\" X-Bad: a\rb" HTTPC:HEADER+
   s" a CR in a header line fails `malformed`" T-LABEL
   HTTPC-FAULT:MALFORMED REJECTED
   s" GET" s" /a b" PREPARE
   s" a SP in the target fails `malformed`" T-LABEL
   HTTPC-FAULT:MALFORMED REJECTED
   ASK WIDE-SPAN READ-ALL
   s" rejected requests dial nothing" T-LABEL
   200 s" first" ANSWER-IS
   ACCEPTS @ 1 T=
   s" GET" s" /a b" PREPARE
   s" a rejected request over a held connection" T-LABEL
   HTTPC-FAULT:MALFORMED REJECTED
   ASK WIDE-SPAN READ-ALL
   s" a rejected request drops the held connection" T-LABEL
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
: HTTPS-ORIGIN! ( str n -- ) {: h:str port:n :}
   SB-RESET s" https://" SB-APPEND h SB-APPEND s" :" SB-APPEND
   port FMT:SB-U SB$ {: s:str :}
   s STR:DATA ORIGIN-BUF s STR:LEN BYTE-COPY  s STR:LEN ORIGIN-U ! ;

: TRUST! ( str -- )
   TEST-TLS-PEER:IN-DIR {: path:str :}
   path STR:DATA TRUST-BUF path STR:LEN BYTE-COPY
   path STR:LEN TRUST-U ! ;

\ A conn opened on the https origin for HOST at PORT, trusting the scratch
\ file, with a room of ROOM bytes.
: BEGIN-HTTPS-AT ( str str n n -- ) {: h:str f:str port:n room:n :}
   f TRUST!  h port HTTPS-ORIGIN!
   LINK ORIGIN$ TRUST-BUF TRUST-U @ >str ROOM-BUF room HTTPC:OPEN
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
   GOT$ s" <HTML>" STR:STARTS-WITH? TTRUE
   LINK HTTPC:HEAD$ s" HTTP/1.0 200" STR:STARTS-WITH? TTRUE
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

\ The plain server answers the ClientHello by its script; the handshake
\ fails with mbedTLS's code as CAUSE.
: SHAKE-FAILS ( script n str -- ) {: sc:script cause:n s:str :}
   sc SCRIPT-IS
   s" localhost" s" ca.pem" PORT-CELL @ ROOM-CAP BEGIN-HTTPS-AT
   ASK-ROOT WIDE-SPAN READ-ALL
   s T-LABEL
   HTTPC-FAULT:HANDSHAKE true FAULT-IS
   LINK HTTPC:CAUSE cause T=
   END-CASE ;

: TEST-HTTPS-HANDSHAKE ( -- )
   construct script cut-head CONN-EOF
   s" a server that closes before any byte fails `handshake`" SHAKE-FAILS
   construct script sized INVALID-RECORD
   s" a server answering plain HTTP fails `handshake`" SHAKE-FAILS ;

\ An answer head padded to hold the request head, which shares the room.
: HELD-HEAD$ ( -- str )
   S\" HTTP/1.1 200 OK\r\nContent-Length: 5\r\nX-Pad: abcdefghijklmnopq\r\n\r\n" ;
: STALE$ ( -- str ) S\" HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nstale" ;

\ The head fills a room of its own size, so the head's read ends at it and the
\ body's at the body: STALE$, in the same record, stays in the TLS session. A
\ reused connection would answer the second request with it.
: TEST-HTTPS-HELD ( -- )
   s" localhost" s" ca.pem" TEST-TLS-PEER:PORT HELD-HEAD$ STR:LEN
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

: TEST-HTTPS-REUSE ( -- )
   TEST-TLS-PEER:HANDSHAKES {: before:n :}
   s" localhost" s" ca.pem" BEGIN-HTTPS
   ASK-ROOT  FIRST$ TEST-TLS-PEER:SCRIPTED  WIDE-SPAN READ-ALL
   s" the first of two https requests" T-LABEL
   200 s" first" ANSWER-IS
   ASK-ROOT  SECOND$ TEST-TLS-PEER:SCRIPTED  WIDE-SPAN READ-ALL
   s" the second https request rides the first one's session" T-LABEL
   200 s" second" ANSWER-IS
   TEST-TLS-PEER:HANDSHAKES before - 1 T=
   END-CASE ;

\ Unanswered, tickets alone wait to the stall and the pipe made readable
\ ends the wait `woken`; the next SEND, with the pipe still readable, is answered.
: TEST-HTTPS-WOKEN ( -- )
   s" localhost" s" ca.pem" BEGIN-HTTPS
   FD:PIPE-PAIR {: r:fd w:fd :}
   LINK s" GET" s" /" HTTPC:REQUEST
   LINK TOTAL-MS SHORT-STALL-MS HTTPC:DEADLINES!
   LINK s" " HTTPC:SEND
   s" TLS 1.3 session tickets reach the socket before any answer" T-LABEL
   AWAIT-INPUT TTRUE
   WIDE-SPAN r READ-UNLESS
   s" tickets with the descriptor unreadable wait to the stall" T-LABEL
   HTTPC-FAULT:TIMEOUT true FAULT-IS
   ASK-ROOT
   s" the next session's tickets reach the socket" T-LABEL
   AWAIT-INPUT TTRUE
   w STIR  WIDE-SPAN r READ-UNLESS
   s" tickets with the descriptor readable answer `woken`" T-LABEL
   HTTPC-FAULT:WOKEN true FAULT-IS
   ASK-ROOT  FIRST$ TEST-TLS-PEER:SCRIPTED  WIDE-SPAN READ-ALL
   s" the SEND after `woken` shakes hands without the descriptor" T-LABEL
   200 s" first" ANSWER-IS
   w r SHUT-PIPE
   END-CASE ;

3 constant TICKETED-RECORDS             \ s_server's two tickets, the answer

\ How many whole TLS records the first U bytes of PEEK-BUF hold.
: RECORDS-IN ( n -- n ) {: u:n :}
   0 0 begin {: k:n at:n :}
      at RECORD-HEAD + u > if k exit then
      PEEK-BUF at + RECORD-SIZE at + {: next:n :}
      next u > if k exit then
      k 1+ next
   again ;

\ Whether the conn's socket holds the tickets' and the answer's records
\ whole, read without taking them.
: TICKETED? ( -- bool )
   LINK HTTPC-TEST:FD PEEK-BUF REQ-CAP PEEK-FLAGS PEEK-CALL {: u:n :}
   u 0 > if u RECORDS-IN TICKETED-RECORDS >= else false then ;

\ True once TICKETED? is, false at INPUT-MS from now.
: AWAIT-TICKETED ( -- bool )
   INPUT-MS NS-PER-MS * mono-ns + {: end:n :}
   begin
      TICKETED? if true exit then
      mono-ns end < while
      TASK:PAUSE
   repeat false ;

\ Over TLS 1.3 the first read takes a ticket and answers want-read with
\ the rest on the socket: a wait that finds the socket and the pipe both
\ readable serves the socket.
: TEST-HTTPS-SERVED ( -- )
   s" localhost" s" ca.pem" BEGIN-HTTPS
   FD:PIPE-PAIR {: r:fd w:fd :}
   ASK-ROOT  FIRST$ TEST-TLS-PEER:SCRIPTED
   s" the tickets' and the answer's records reach the socket" T-LABEL
   AWAIT-TICKETED TTRUE
   w STIR  WIDE-SPAN r READ-UNLESS
   s" a socket holding the answer is served before `woken`" T-LABEL
   200 s" first" ANSWER-IS
   w r SHUT-PIPE
   END-CASE ;

\ ---- A trust path TLS cannot read

3 constant UNREAD-SENDS
1 constant F-GETFD
64 constant FD-SCAN                     \ above every descriptor the test holds
create LONG-TRUST PATH-CAP 1 + allot    \ a trust path FS cannot spell

\ How many of the descriptors below FD-SCAN are open. accept(2) reserves the
\ number of the socket it waits for before it blocks, so the lowest free
\ number moves with the server task; a reserved number is not open.
: OPEN-FDS ( -- n )
   0 FD-SCAN 0 do i F-GETFD 0 fcntl 0 >= if 1+ then loop ;

\ One SEND, whose first NEXT fails `trust` with X509_FILE_IO_ERROR. Once the
\ server has closed the Kth connection, the descriptors open are OPEN.
: UNREAD-SEND ( n n -- ) {: open:n k:n :}
   SPARE s" GET" s" /" HTTPC:REQUEST
   SPARE TOTAL-MS STALL-MS HTTPC:DEADLINES!
   SPARE s" " HTTPC:SEND  ANSWER-RESET
   SPARE SPAN-BUF WIDE-SPAN >LEN HTTPC:NEXT ANSWERED? TTRUE
   HTTPC-FAULT:TRUST false FAULT-IS
   SPARE HTTPC:CAUSE TEST-TLS-PEER:X509-FILE-IO T=
   CLOSES k SERVER-TASK TEST-WAIT:FOR-CELL
   OPEN-FDS open T= ;

\ An https origin at the plain server, opened in a record full of stale
\ bytes, trusting the path: each SEND dials, and TLS rejects the path
\ before it makes anything. The server has closed DONE connections.
: UNREAD-PATH ( str n -- ) {: t:str done:n :}
   SPOIL  s" 127.0.0.1" PORT-CELL @ HTTPS-ORIGIN!
   SPARE ORIGIN$ t ROOM-BUF ROOM-CAP HTTPC:OPEN TTRUE
   OPEN-FDS {: open:n :}
   UNREAD-SENDS 0 do open done i + 1+ UNREAD-SEND loop
   SPARE HTTPC:CLOSE ;

\ The server answers nothing; it closes each connection at HTTPC's close.
: TEST-HTTPS-UNREAD ( -- )
   construct script silent SCRIPT-IS
   s" a trust path holding a 0 byte fails each SEND `trust`, holding no socket"
   T-LABEL
   s\" ca\z.pem" 0 UNREAD-PATH
   PATH-CAP 1 + 0 do [char] a LONG-TRUST i + c! loop
   s" OPEN takes a trust path longer than PATH-CAP, which fails `trust`"
   T-LABEL
   LONG-TRUST PATH-CAP 1 + >str UNREAD-SENDS UNREAD-PATH ;

\ ---- A relay that withholds the close_notify

\ The relay task accepts HTTPC's connection and dials s_server. It forwards
\ the client's bytes as they come and the server's by whole records, each
\ framed by its 5-byte header. At the server's first alert record, the
\ close_notify, it closes the client's socket with that record unsent.
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
   RECORDS at + RECORD-SIZE
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
   client RECORDS sent >str TCP4:WRITE PEER-STATUS
   RECORDS sent + RECORDS RECORDS-U @ sent - BYTE-COPY
   sent negate RECORDS-U +!
   alert if true WITHHELD ! false exit then
   true ;

: UP ( TCP4:connection n -- bool ) {: server:TCP4:connection k:n :}
   server UPWARD k >str TCP4:WRITE PEER-STATUS true ;

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

: TEST-HTTPS-CUT ( -- )
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
   GOT$ s" <HTML>" STR:STARTS-WITH? TTRUE ;

: HTTPS-CASES ( -- )
   TEST-TLS-PEER:CERTS
   TEST-HTTPS-SILENT
   TEST-HTTPS-HANDSHAKE
   TEST-TLS-PEER:SCRIPT-SERVE
   TEST-HTTPS-HELD
   TEST-HTTPS-REUSE
   TEST-HTTPS-CLOSE
   TEST-HTTPS-WOKEN
   TEST-HTTPS-SERVED
   TEST-TLS-PEER:UNSERVE true TEST-TLS-PEER:WWW-SERVE
   TEST-HTTPS-TRUSTED TEST-HTTPS-UNRELATED TEST-HTTPS-OTHER-HOST
   TEST-HTTPS-CUT ;

: TEST-HTTPS ( -- )
   s" habu-httpc" TEST-TLS-PEER:SCRATCH
   [: HTTPS-CASES ;] [: TEST-TLS-PEER:CLEAN ;] finally ;


\ ---- The table against its model

\ The file at the path, whole, in the arena.
: FILE-IN ( str ^ARENA:arena -- str ) {: p:str ar :}
   p FS:FILE-SIZE {: u:n :}
   ar u ARENA:ALLOC drop {: b:^ :}
   b  p b u FS:READ-ALL >str ;

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
   SPOIL
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
   TEST-HTTPS-UNREAD
   PORT-CELL @ ORIGIN!
   TEST-SIZED
   TEST-CHUNKED
   TEST-CLOSING
   TEST-INTERIM
   TEST-NO-CONTENT
   TEST-REUSE
   TEST-CLOSE-REDIAL
   TEST-IDLE-CLOSE
   TEST-UNREAD
   TEST-TRAILING
   TEST-SHUT
   TEST-WIRE-BODY
   TEST-STALE-BYTE
   TEST-LENGTH-SENT
   TEST-NODELAY
   TEST-CONNECT
   TEST-CUT
   TEST-TIMEOUT
   TEST-TOTAL
   TEST-MALFORMED
   TEST-OVERSIZE
   TEST-REJECTED
   TEST-CLOSE
   TEST-WOKEN
   TEST-UNWOKEN
   TEST-HTTPS
   END-SERVER ;

;package

HTTPC-TEST:RUN
T-REPORT
