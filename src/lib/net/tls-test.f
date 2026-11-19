split-str
\ tls-test.f - TLS sessions against `openssl s_server -www`, this test's own
\ child on a loopback port, with certificates made here in a scratch
\ directory under TMPDIR that the test removes: a trusted handshake carries
\ GET / and its answer to the close; another host name and an unrelated CA
\ are distrusted at the handshake, a missing trust file at START; STOP lets
\ the same record START again. Every wait is a poll on a descriptor.
\ Run: bin/hb --load src/lib/net/tls-test.f </dev/null
require src/lib/test.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/spawn.f
require src/lib/net/tcp4.f
require src/lib/net/tls.f

package TLS-TEST
private

$7F000001 constant LOOPBACK
$200 constant DIR-ROOM
DIR-ROOM BUFFER: DIR                \ the scratch directory's path
variable DIR-U
$400 constant OUT-ROOM
OUT-ROOM BUFFER: OUT                \ the server's output up to its port
variable OUT-U
$4000 constant ANSWER-ROOM
ANSWER-ROOM BUFFER: ANSWER          \ the server's answer to GET /
variable ANSWER-U
variable SERVER-PID                 \ 0 while no server runs
variable SERVER-OUT                 \ the read end of its output, an fd number
variable PORT
TYPED-VARIABLE SESSION TLS:session

\ What a TLS:tls-step answered, as one number.
0 constant DONE
1 constant DISTRUSTED
2 constant FAILED
3 constant WANT-READ
4 constant WANT-WRITE

$04 constant CN-MISMATCH            \ MBEDTLS_X509_BADCERT_CN_MISMATCH
$08 constant NOT-TRUSTED            \ MBEDTLS_X509_BADCERT_NOT_TRUSTED
$3E00 negate constant FILE-IO       \ MBEDTLS_ERR_PK_FILE_IO_ERROR, from parse_file

: DIR$ ( -- str ) DIR DIR-U @ ;

\ A file of the scratch directory, in SB's buffer.
: IN-DIR ( str -- str )
   {: a:^ u:n :}
   SB-RESET DIR$ SB-APPEND s" /" SB-APPEND a u SB-APPEND SB$ ;

: SCRATCH ( -- )
   s" habu-tls" FS:MKTEMP-DIR {: a:^ u:n :}
   a u DIR DIR-ROOM SPAN:MAKE SPAN:COPY
   u DIR-U ! ;


\ Two CAs, and a certificate for localhost the first signs. openssl's
\ chatter goes to a log the script shows only when a command fails.
: KEY+ ( -- )
   s"  -newkey ec -pkeyopt ec_paramgen_curve:P-256 -noenc" SB-APPEND ;

: CERTS$ ( -- str )
   SB-RESET
   s" { openssl req -x509 -days 1 -subj /CN=habu-tls-ca" SB-APPEND
   s"  -keyout ca.key -out ca.pem" SB-APPEND KEY+
   s"  && openssl req -x509 -days 1 -subj /CN=habu-tls-other" SB-APPEND
   s"  -keyout other.key -out other.pem" SB-APPEND KEY+
   s"  && openssl req -subj /CN=localhost" SB-APPEND
   s"  -addext subjectAltName=DNS:localhost" SB-APPEND
   s"  -keyout server.key -out server.csr" SB-APPEND KEY+
   s"  && openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key" SB-APPEND
   s"  -set_serial 1 -days 1 -copy_extensions copy -out server.pem; }" SB-APPEND
   s"  2>openssl.log || { cat openssl.log >&2; exit 1; }" SB-APPEND
   SB$ ;

: CERTS ( -- )
   s" certificates are made in the scratch directory" T-LABEL
   s" /bin/sh" PROCESS:NEW
   s" -c" PROCESS:ARG
   CERTS$ PROCESS:ARG
   DIR$ PROCESS:CWD
   PROCESS:SPAWN PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T= ;


: AWAIT ( fd n -- )
   PROCESS:PFD!  1 -1 >MS PROCESS:POLL drop ;

\ One read of the server's output, once there is some.
: READ-OUT ( -- )
   SERVER-OUT @ >FD FD:POLLIN AWAIT
   SERVER-OUT @ OUT OUT-U @ + OUT-ROOM OUT-U @ - read {: k:n :}
   k 0 <= if E-PROC-OUTPUT throw then
   k OUT-U +! ;

: ACCEPT$ ( -- str ) s" ACCEPT 127.0.0.1:" ;

\ The output after the ACCEPT line's prefix, empty until it is there.
: TAIL$ ( -- str )
   OUT OUT-U @ ACCEPT$ FIND-SUB
   MATCH option
      none OF OUT 0 ENDOF
      some OF IDX>N ACCEPT$ nip + dup OUT + swap OUT-U @ swap - ENDOF
   ;MATCH ;

\ The port's digits once their line has ended, else empty.
: PORT$ ( -- str )
   TAIL$ {: a:^ u:n :}
   a u 10 INDEX-OF
   MATCH option
      none OF a 0 ENDOF
      some OF IDX>N a swap ENDOF
   ;MATCH ;

: SERVER ( -- PROCESS:request )
   s" /usr/bin/env" PROCESS:NEW
   s" openssl" PROCESS:ARG
   s" s_server" PROCESS:ARG
   s" -accept" PROCESS:ARG
   s" 127.0.0.1:0" PROCESS:ARG
   s" -www" PROCESS:ARG
   s" -cert" PROCESS:ARG
   s" server.pem" PROCESS:ARG
   s" -key" PROCESS:ARG
   s" server.key" PROCESS:ARG
   DIR$ PROCESS:CWD ;

\ The server's stdout and stderr share one pipe, read only up to the ACCEPT
\ line: what it writes after that stays far below the pipe's capacity.
: SERVE ( -- )
   s" the server names its loopback port" T-LABEL
   FD:PIPE-PAIR {: r:fd w:fd :}
   SERVER 1 >FD w PROCESS:FD 2 >FD w PROCESS:FD
   PROCESS:SPAWN PID>N SERVER-PID !
   w FD>N close
   r FD>N SERVER-OUT !
   0 OUT-U !
   begin PORT$ nip 0= while READ-OUT repeat
   PORT$ STR-PARSE-POS
   MATCH option
      none OF s" no port" T-FAIL-AS ENDOF
      some OF PORT ! ENDOF
   ;MATCH ;

: UNSERVE ( -- )
   SERVER-PID @ 0= if exit then
   SERVER-PID @ >PID PROCESS:SIGKILL PROCESS:KILL-RAW drop
   SERVER-PID @ >PID PROCESS:WAIT PROCESS:STATUS>RC drop
   0 SERVER-PID !
   SERVER-OUT @ close ;


\ A connected, non-blocking socket to the server.
: DIAL ( -- fd )
   LOOPBACK TCP4:ADDRESS PORT @ TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF TCP4:CONNECTION-FD ENDOF
      failed OF TCP4:ERRNO>N drop s" connect" T-FAIL-AS -1 >FD ENDOF
   ;MATCH ;

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
   SESSION sock host hu file fu IN-DIR TLS:START >STEP ;

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
         failed OF drop false exit ENDOF
      ;MATCH
      dup u >= if drop true exit then
   again ;

\ ANSWER filled until the session closes; false once READ answers failed.
: TAKE ( fd -- bool )
   {: sock:fd :}
   0 ANSWER-U !
   begin
      SESSION ANSWER ANSWER-U @ + ANSWER-ROOM ANSWER-U @ - TLS:READ
      MATCH TLS:tls-io
         moved OF LEN>N ANSWER-U +! ENDOF
         want-read OF sock FD:POLLIN AWAIT ENDOF
         want-write OF sock FD:POLLOUT AWAIT ENDOF
         closed OF true exit ENDOF
         failed OF drop false exit ENDOF
      ;MATCH
   again ;


: TRUSTED ( -- )
   s" a trusted server for localhost answers GET / and closes" T-LABEL
   DIAL {: sock:fd :}
   sock s" localhost" s" ca.pem" SET-UP 0 T= DONE T=
   sock SHAKE 0 T= DONE T=
   sock S\" GET / HTTP/1.0\r\n\r\n" PUT TTRUE
   sock TAKE TTRUE
   ANSWER ANSWER-U @ s" HTTP/1.0 200" STARTS-WITH? TTRUE
   sock HANG-UP ;

: OTHER-HOST ( -- )
   s" the server asked for another host name is distrusted" T-LABEL
   DIAL {: sock:fd :}
   sock s" example.org" s" ca.pem" SET-UP 0 T= DONE T=
   sock SHAKE {: what:n flags:n :}
   what DISTRUSTED T=
   flags CN-MISMATCH and CN-MISMATCH T=
   sock HANG-UP ;

: UNRELATED ( -- )
   s" a trust file of an unrelated CA is distrusted" T-LABEL
   DIAL {: sock:fd :}
   sock s" localhost" s" other.pem" SET-UP 0 T= DONE T=
   sock SHAKE {: what:n flags:n :}
   what DISTRUSTED T=
   flags NOT-TRUSTED and NOT-TRUSTED T=
   sock HANG-UP ;

\ START does no I/O, so no socket is dialed.
: MISSING ( -- )
   s" a trust file that does not exist is distrusted at START" T-LABEL
   -1 >FD s" localhost" s" absent.pem" SET-UP
   FILE-IO T= DISTRUSTED T=
   SESSION TLS:STOP ;

: SHAKE-HANDS ( -- )
   DIAL {: sock:fd :}
   sock s" localhost" s" ca.pem" SET-UP 0 T= DONE T=
   sock SHAKE 0 T= DONE T=
   sock HANG-UP ;

: RESTART ( -- )
   s" STOP then START again on the same record shakes hands" T-LABEL
   SHAKE-HANDS SHAKE-HANDS ;


: CASES ( -- )
   CERTS SERVE TRUSTED OTHER-HOST UNRELATED MISSING RESTART ;

: RUN ( -- )
   SCRATCH
   [: CASES ;] [: UNSERVE DIR$ FS:REMOVE-TREE ;] finally ;

T-RESET
RUN
s" tls-test: " type T-CASES FMT:.INT s"  checks" type cr
T-REPORT

;package
