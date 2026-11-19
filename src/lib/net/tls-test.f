split-str
\ tls-test.f - TLS sessions against TEST-TLS-PEER's `openssl s_server -www`
\ on a loopback port, with the certificates it makes: a trusted handshake
\ carries GET / and its answer to the close; another host name and an
\ unrelated CA are distrusted at the handshake, a missing trust file or a
\ trust path FS cannot spell at START, where a host holding a 0 byte fails
\ BAD-INPUT; STOP lets the same record START again. A peer that ends its
\ stream without close_notify reads `truncated`: the test relays the
\ handshake between the session and the server itself, then sends the
\ session its FIN. Every wait is a poll on a descriptor.
\ Run: bin/hb --load src/lib/net/tls-test.f </dev/null
require src/lib/test.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fd.f
require src/lib/process.f
require src/lib/net/tcp4.f
require src/lib/net/tls.f
require src/lib/net/test-tls-peer.f

package TLS-TEST
private

$4000 constant ANSWER-ROOM
ANSWER-ROOM BUFFER: ANSWER          \ the server's answer to GET /
variable ANSWER-U
variable RELAY-LIS                  \ LISTEN's cells: the listener, as a
variable RELAY-PORT                 \ number, and its port
TYPED-VARIABLE SESSION TLS:session

\ What a TLS:tls-step answered, as one number.
0 constant DONE
1 constant DISTRUSTED
2 constant FAILED
3 constant WANT-READ
4 constant WANT-WRITE
\ How a read ended, beside FAILED.
5 constant CLOSED
6 constant TRUNCATED
$4000 constant RELAY-ROOM
RELAY-ROOM BUFFER: RELAYED           \ bytes on their way through the relay

$3E00 negate constant FILE-IO       \ MBEDTLS_ERR_PK_FILE_IO_ERROR, from parse_file
135 negate constant BAD-INPUT       \ MBEDTLS_ERR_SSL_BAD_INPUT_DATA
create LONG PATH-CAP 1 + allot      \ a trust path FS cannot spell


: AWAIT ( fd n -- )
   PROCESS:PFD!  1 -1 >MS PROCESS:POLL drop ;


\ A connected, non-blocking socket to the loopback port.
: DIAL-AT ( n -- fd )
   {: port:n :}
   TEST-TLS-PEER:LOOPBACK TCP4:ADDRESS port TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF TCP4:CONNECTION-FD ENDOF
      failed OF TCP4:ERRNO>N drop s" connect" T-FAIL-AS -1 >FD ENDOF
   ;MATCH ;

\ A connected, non-blocking socket to the server.
: DIAL ( -- fd ) TEST-TLS-PEER:PORT DIAL-AT ;

: HANG-UP ( fd -- )
   SESSION TLS:STOP  FD>N close ;

\ The answer's kind and its code, 0 where it carries none.
: >STEP ( TLS:tls-step -- n n )
   MATCH TLS:tls-step
      done OF DONE 0 ENDOF
      want-read OF WANT-READ 0 ENDOF
      want-write OF WANT-WRITE 0 ENDOF
      distrusted OF DISTRUSTED swap ENDOF
      failed OF FAILED swap ENDOF
   ;MATCH ;

\ The poll events a want answer waits for, 0 for an answer that ends.
: EVENTS ( n -- n )
   {: what:n :}
   what WANT-READ = if FD:POLLIN exit then
   what WANT-WRITE = if FD:POLLOUT exit then
   0 ;

: SET-UP ( fd ^u8 n ^u8 n -- n n )
   {: sock:fd host:^ hu:n file:^ fu:n :}
   SESSION sock host hu file fu TEST-TLS-PEER:IN-DIR TLS:START
   >STEP ;

\ The handshake to its end, waiting on the socket as it asks.
: SHAKE ( fd -- n n )
   {: sock:fd :}
   begin
      SESSION TLS:HANDSHAKE >STEP {: what:n code:n :}
      what EVENTS {: ev:n :}
      ev 0= if what code exit then
      sock ev AWAIT
   again ;

\ The span written whole; false once WRITE answers closed or failed. After a
\ want answer the same span is written again, as WRITE requires.
: PUT ( fd ^u8 n -- bool )
   {: sock:fd a:^ u:n :}
   0
   begin {: sent:n :}
      SESSION a sent + u sent - TLS:WRITE
      MATCH TLS:tls-io
         moved OF LEN>N sent + ENDOF
         want-read OF sock FD:POLLIN AWAIT sent ENDOF
         want-write OF sock FD:POLLOUT AWAIT sent ENDOF
         closed OF false exit ENDOF
         truncated OF false exit ENDOF
         failed OF drop false exit ENDOF
      ;MATCH
      dup u >= if drop true exit then
   again ;

\ ANSWER filled until READ ends it: CLOSED, TRUNCATED or FAILED.
: TAKE ( fd -- n )
   {: sock:fd :}
   0 ANSWER-U !
   begin
      SESSION ANSWER ANSWER-U @ + ANSWER-ROOM ANSWER-U @ - TLS:READ
      MATCH TLS:tls-io
         moved OF LEN>N ANSWER-U +! ENDOF
         want-read OF sock FD:POLLIN AWAIT ENDOF
         want-write OF sock FD:POLLOUT AWAIT ENDOF
         closed OF CLOSED exit ENDOF
         truncated OF TRUNCATED exit ENDOF
         failed OF drop FAILED exit ENDOF
      ;MATCH
   again ;


: TRUSTED ( -- )
   s" a trusted server for localhost answers GET / and closes" T-LABEL
   DIAL {: sock:fd :}
   sock s" localhost" s" ca.pem" SET-UP 0 T= DONE T=
   sock SHAKE 0 T= DONE T=
   sock S\" GET / HTTP/1.0\r\n\r\n" PUT TTRUE
   sock TAKE CLOSED T=
   ANSWER ANSWER-U @ s" HTTP/1.0 200" STARTS-WITH? TTRUE
   sock HANG-UP ;

: OTHER-HOST ( -- )
   s" the server asked for another host name is distrusted" T-LABEL
   DIAL {: sock:fd :}
   sock s" example.org" s" ca.pem" SET-UP 0 T= DONE T=
   sock SHAKE {: what:n flags:n :}
   what DISTRUSTED T=
   flags TEST-TLS-PEER:CN-MISMATCH and TEST-TLS-PEER:CN-MISMATCH T=
   sock HANG-UP ;

: UNRELATED ( -- )
   s" a trust file of an unrelated CA is distrusted" T-LABEL
   DIAL {: sock:fd :}
   sock s" localhost" s" other.pem" SET-UP 0 T= DONE T=
   sock SHAKE {: what:n flags:n :}
   what DISTRUSTED T=
   flags TEST-TLS-PEER:NOT-TRUSTED and TEST-TLS-PEER:NOT-TRUSTED T=
   sock HANG-UP ;

\ START leaves the socket alone, so none is dialed.
: MISSING ( -- )
   s" a trust file that does not exist is distrusted at START" T-LABEL
   -1 >FD s" localhost" s" absent.pem" SET-UP
   FILE-IO T= DISTRUSTED T=
   SESSION TLS:STOP ;

\ A trust path FS cannot spell, given as it is.
: UNSPELLED ( str -- )
   {: path:^ u:n :}
   SESSION -1 >FD s" localhost" path u TLS:START >STEP
   TEST-TLS-PEER:X509-FILE-IO T= DISTRUSTED T=
   SESSION TLS:STOP ;

: BAD-PATHS ( -- )
   PATH-CAP 1 + 0 do [char] a LONG i + c! loop
   s" a trust path longer than PATH-CAP is distrusted at START" T-LABEL
   LONG PATH-CAP 1 + UNSPELLED
   s" a trust path holding a 0 byte is distrusted at START" T-LABEL
   s\" ca\z.pem" UNSPELLED ;

: NUL-HOST ( -- )
   s" a host holding a 0 byte fails BAD-INPUT at START" T-LABEL
   -1 >FD s\" local\zhost" s" ca.pem" SET-UP
   BAD-INPUT T= FAILED T=
   SESSION TLS:STOP ;

: SHAKE-HANDS ( -- )
   DIAL {: sock:fd :}
   sock s" localhost" s" ca.pem" SET-UP 0 T= DONE T=
   sock SHAKE 0 T= DONE T=
   sock HANG-UP ;

: RESTART ( -- )
   s" STOP then START again on the same record shakes hands" T-LABEL
   SHAKE-HANDS SHAKE-HANDS ;


\ ---- A peer that ends its stream without close_notify

\ The relay's two ends, as TCP4 connections.
: >CONN ( fd -- TCP4:connection ) FD>N TCP4:>CONNECTION ;

\ Whether a TCP4 call succeeded.
: OK? ( TCP4:status -- bool )
   MATCH TCP4:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;

\ Bytes one end has sent, written to the other; false once it has ended or
\ failed.
: MOVE ( TCP4:connection TCP4:connection -- bool )
   {: from:TCP4:connection to:TCP4:connection :}
   from RELAYED RELAY-ROOM TCP4:TRANSFER-BYTES TCP4:TRY-READ
   MATCH TCP4:try-read-result
      data OF to RELAYED rot TCP4:WRITE OK? ENDOF
      closed OF drop false ENDOF
      empty OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;

\ One wait for either end, forwarding what it sent; false once an end has
\ ended or failed.
: PUMP ( TCP4:connection TCP4:connection -- bool )
   {: peer:TCP4:connection far:TCP4:connection :}
   peer TCP4:CONNECTION-FD FD:POLLIN 0 >IDX PROCESS:PFD-AT!
   far TCP4:CONNECTION-FD FD:POLLIN 1 >IDX PROCESS:PFD-AT!
   2 -1 >MS PROCESS:POLL drop
   0 >IDX PROCESS:PFD-REVENTS 0<> if
      peer far MOVE 0= if false exit then
   then
   1 >IDX PROCESS:PFD-REVENTS 0<> if far peer MOVE exit then
   true ;

\ The handshake to its end, relayed between PEER, the session's own peer,
\ and the server.
: RELAYED-SHAKE ( TCP4:connection TCP4:connection -- n n )
   {: peer:TCP4:connection far:TCP4:connection :}
   begin
      SESSION TLS:HANDSHAKE >STEP {: what:n code:n :}
      what EVENTS 0= if what code exit then
      peer far PUMP 0= if FAILED 0 exit then
   again ;

\ A loopback listener on a port of its own, and the port.
: LISTEN ( -- TCP4:listener n )
   RELAY-LIS RELAY-PORT TEST-TLS-PEER:LISTEN
   0= if E-PROC-OUTPUT throw then
   RELAY-LIS @ TCP4:>LISTENER RELAY-PORT @ ;

\ The session's peer: the connection DIAL-AT made, accepted at once since it
\ is already queued.
: PEER ( TCP4:listener -- TCP4:connection )
   TCP4:ACCEPT MATCH TCP4:accept-result
      accepted OF 2drop ENDOF
      failed OF drop E-PROC-OUTPUT throw ENDOF
   ;MATCH ;

\ The relay ends the handshake once the session is done; the server's
\ answer to the session's Finished is never forwarded. The peer's FIN, a
\ shutdown and not a close, reaches the session even with that Finished still
\ unread at the peer, where a close would answer it with a reset.
: CUT-PEER ( -- )
   s" a peer that ends its stream without close_notify reads truncated" T-LABEL
   LISTEN {: lis:TCP4:listener port:n :}
   port DIAL-AT {: sock:fd :}
   lis PEER {: peer:TCP4:connection :}
   DIAL >CONN {: far:TCP4:connection :}
   sock s" localhost" s" ca.pem" SET-UP 0 T= DONE T=
   peer far RELAYED-SHAKE 0 T= DONE T=
   peer TCP4:SENDING TCP4:SHUTDOWN OK? TTRUE
   sock TAKE TRUNCATED T=
   ANSWER-U @ 0 T=
   sock HANG-UP
   peer TCP4:CLOSE OK? drop
   far TCP4:CLOSE OK? drop
   lis TCP4:CLOSE-LISTENER OK? drop ;


: CASES ( -- )
   TEST-TLS-PEER:CERTS false TEST-TLS-PEER:WWW-SERVE
   TRUSTED OTHER-HOST UNRELATED MISSING BAD-PATHS NUL-HOST RESTART CUT-PEER ;

: RUN ( -- )
   s" habu-tls" TEST-TLS-PEER:SCRATCH
   [: CASES ;] [: TEST-TLS-PEER:CLEAN ;] finally ;

T-RESET
RUN
s" tls-test: " type T-CASES FMT:.INT s"  checks" type cr
T-REPORT

;package
