split-str
\ httpc-test.f - focused tests for src/lib/net/httpc.f over loopback HTTP/1.1.
\ One process holds both peers: a server task accepts on a loopback TCP4
\ listener bound to port 0 and answers each request with the bytes its case
\ scripts, counting accepts, requests per connection and request bytes, while
\ the main task drives HTTPC. Every exchange has deadlines, so a server that
\ fails to answer ends its case `timeout` and not in a hang. A scripted close
\ is a half-close: the server sends its FIN and still counts any request that
\ arrives after it.
\ Run: bin/hb --load src/lib/net/httpc-test.f
HB-TARGET-LINUX-KERNEL? ?TARGET
require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/test.f
require src/lib/test/wait.f
require src/lib/net/tcp4.f
require src/lib/net/httpc.f

\ White-box: the conn's socket, so a case waits for the server's FIN to reach
\ it before it sends again.
package HTTPC
: HTTPC-TEST:FD ( ^conn -- n ) CONN-FD @ ;
;package

package HTTPC-TEST

private

CAST: BLEN>N ( NUM:byte-len -- n )

$7F000001 constant LOOPBACK
8 constant BACKLOG
$1000 constant REQ-CAP
$10000 constant ROOM-CAP
$100 constant GOT-CAP
$40 constant ORIGIN-CAP
5000 constant TOTAL-MS
5000 constant STALL-MS
5000 constant FIN-MS
8 constant CONN-CAP                     \ connections one case may make
10 constant SIZED-SPAN                  \ shorter than the sized body
4 constant CHUNKED-SPAN                 \ crosses every chunk boundary
$40 constant WIDE-SPAN
$01 constant POLLIN

create REQ-BUF REQ-CAP allot
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
variable ORIGIN-U
variable GOT-U
variable WIDEST                         \ the largest piece one NEXT answered
variable LAST-STATUS
variable FAULTS
CONN-CAP TYPED-BUFFER PER-CONN n

\ What the server answers in the running case.
ENUM script sized chunked closing interim no-content reuse idle-close
   unread ;ENUM
TYPED-VARIABLE SCRIPT script
TYPED-VARIABLE LINK HTTPC:conn

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

\ The Content-Length HTTPC writes in every request head of H bytes.
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

: LISTEN-LOOPBACK ( -- bool )
   LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF TCP4:LISTENER>N LISTENER-CELL ! ENDOF
      failed OF drop false exit ENDOF
   ;MATCH
   LISTENER@ BACKLOG TCP4:LISTEN
   MATCH TCP4:status ok OF ENDOF failed OF drop false exit ENDOF ;MATCH
   LISTENER@ TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N PORT-CELL ! drop true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;

\ The port is published before readiness is.
: SERVER-WORK ( -- )
   LISTEN-LOOPBACK 0= if 1 STOP atomic-add drop then
   1 READY atomic-add drop
   SERVE-LOOP
   1 DONE atomic-add drop ;

: START-SERVER ( -- )
   ['] SERVER-WORK SERVER-TASK TASK:ACTIVATE
   READY 1 SERVER-TASK TEST-WAIT:FOR-CELL ;

: WAKE-SERVER ( -- )
   LOOPBACK TCP4:ADDRESS PORT-CELL @ TCP4:PORT TCP4:CONNECT
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

: ORIGIN! ( -- )
   SB-RESET s" http://127.0.0.1:" SB-APPEND PORT-CELL @ FMT:SB-U
   SB$ {: a:^ u:n :}
   a ORIGIN-BUF u BYTE-COPY  u ORIGIN-U ! ;

\ A fresh case: the script, zeroed counts and a conn holding nothing.
: BEGIN-CASE ( script -- )
   SCRIPT !
   0 ACCEPTS ! 0 REQUESTS ! 0 REQ-BYTES !
   CONN-CAP 0 do 0 i PER-CONN ! loop
   LINK ORIGIN$ s" " ROOM-BUF ROOM-CAP HTTPC:OPEN
   s" OPEN takes the loopback origin" T-LABEL TTRUE ;

: END-CASE ( -- ) LINK HTTPC:CLOSE ;

: ASK ( -- )
   LINK s" GET" s" /case" HTTPC:REQUEST
   LINK TOTAL-MS STALL-MS HTTPC:DEADLINES!
   LINK s" " HTTPC:SEND ;

: GOT$ ( -- str ) GOT-BUF GOT-U @ ;

: GOT+ ( n -- ) {: k:n :}
   SPAN-BUF GOT-BUF GOT-U @ + k BYTE-COPY
   k GOT-U +!  WIDEST @ k max WIDEST ! ;

: FAILED ( HTTPC:fault -- )
   s" httpc-test: NEXT failed: " type HTTPC:FAULT$ type cr
   1 FAULTS +! ;

: ANSWERED? ( HTTPC:chunk-result -- bool )
   MATCH HTTPC:chunk-result
      chunk OF LEN>N GOT+ LAST-STATUS ! false ENDOF
      response OF LAST-STATUS ! true ENDOF
      failed OF FAILED true ENDOF
   ;MATCH ;

\ Reads the answer to its end in NEXT spans of SPAN bytes.
: READ-ALL ( n -- ) {: span:n :}
   0 GOT-U ! 0 WIDEST ! -1 LAST-STATUS ! 0 FAULTS !
   begin LINK SPAN-BUF span >LEN HTTPC:NEXT ANSWERED? until ;

: ANSWER-IS ( n ^u8 n -- ) {: status:n a:^ u:n :}
   FAULTS @ 0 T=  LAST-STATUS @ status T=  GOT$ a u T$= ;

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

public

: RUN ( -- )
   T-RESET
   START-SERVER
   s" the server listens on loopback" T-LABEL
   STOP @ 0 T=
   ORIGIN!
   TEST-SIZED
   TEST-CHUNKED
   TEST-CLOSING
   TEST-INTERIM
   TEST-NO-CONTENT
   TEST-REUSE
   TEST-IDLE-CLOSE
   TEST-UNREAD
   END-SERVER ;

;package

HTTPC-TEST:RUN
T-REPORT
