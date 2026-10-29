\ The HTTP listener keeps listening through an accept that fails for want of
\ descriptors (src/lib/net/http.f ACCEPT-FAILED). Its own process: it lowers
\ RLIMIT_NOFILE, fills the descriptor table and pipes stderr, process-wide
\ state. The client takes the last free descriptor, so accept is EMFILE; the
\ listener's stderr line is the failure event, and once the fillers close the
\ same listener answers a fresh connection.
require src/lib/test.f
require src/lib/test/wait.f
require src/lib/prelude.f
require src/lib/string.f
require src/lib/fd.f
require src/lib/ffi-abi.f
require src/lib/task.f
require src/lib/aio.f
require src/lib/net/tcp4.f
require src/lib/net/http.f

package HTTP-ACCEPT-STARVED-TEST

private

$7F000001 constant LOOPBACK
1 constant ONE-WORKER
$1F4 constant IDLE-MS
$7D0 constant ANSWER-MS           \ a loopback answer that slow is a hang
1000000 constant NS-PER-MS
$40 constant SOFT-LIMIT
$100 constant LINE-CAP
$1000 constant RES-CAP
0 constant F-DUPFD
4 constant F-SETFL
10 constant LF
12 constant STATUS-BYTES       \ "HTTP/1.1 200"
: RLIMIT-NOFILE ( -- n ) HB-TARGET-MACOS? if 8 else 7 then ;
: O-NONBLOCK ( -- n ) HB-TARGET-MACOS? if 4 else $800 then ;
: ECONNRESET ( -- n ) HB-TARGET-MACOS? if 54 else 104 then ;

CAST: BLEN>N ( NUM:byte-len -- n )

PROCESS-SYMBOLS
FUNCTION: GET-LIMIT getrlimit ( n ptr u8 -- i32 )
   1 16 WRITES-BYTES
;FUNCTION
FUNCTION: SET-LIMIT setrlimit ( n ptr u8 -- i32 ) ;FUNCTION
FUNCTION: DUP2 dup2 ( n n -- i32 ) ;FUNCTION

create LIMITS 16 allot            \ struct rlimit: soft, then hard
LINE-CAP BUFFER: LINE-BUF
RES-CAP BUFFER: RES-BUF
SOFT-LIMIT TYPED-BUFFER FILLER n
variable FILLED
variable SAVED-SOFT
variable LINE-U
variable PIPE-R
variable PIPE-W
variable SAVED-ERR
variable READ-ERRNO


: OK ( HTTP:request HTTP:response -- ) {: asked:HTTP:request answer:HTTP:response :}
   answer 200 HTTP:STATUS! ;


: LIMIT! ( n -- ) {: soft:n :}
   RLIMIT-NOFILE LIMITS GET-LIMIT 0 T=
   soft LIMITS !
   RLIMIT-NOFILE LIMITS SET-LIMIT 0 T= ;


\ Duplicates stdin until the soft limit refuses one: true when it did, false
\ when the limit never bit.
: FILL ( -- bool )
   0 FILLED !
   begin
      FILLED @ SOFT-LIMIT < while
      0 F-DUPFD 0 fcntl {: fd:n :}
      fd 0 < if true exit then
      fd FILLED @ FILLER !
      1 FILLED +!
   repeat
   false ;


: CLOSE-LAST-FILLER ( -- )
   -1 FILLED +!
   FILLED @ FILLER @ close ;


: CLOSE-FILLERS ( -- )
   begin FILLED @ 0 > while CLOSE-LAST-FILLER repeat ;


: STDERR-TO-PIPE ( -- )
   pipe {: r:n w:n rc:n :}
   rc 0 T=
   r PIPE-R !
   w PIPE-W !
   r F-SETFL O-NONBLOCK fcntl 0 T=
   FD:STDERR FD>N F-DUPFD 3 fcntl SAVED-ERR !
   0 LINE-U ! ;


: STDERR-BACK ( -- )
   SAVED-ERR @ FD:STDERR FD>N DUP2 FD:STDERR FD>N T=
   SAVED-ERR @ close
   PIPE-W @ close
   PIPE-R @ close ;


: LINE$ ( -- ptr u8 n )
   LINE-BUF LINE-U @ ;


\ What the pipe holds so far, read without waiting.
: TAKE-LINE ( -- )
   PIPE-R @ LINE-BUF LINE-U @ + LINE-CAP LINE-U @ - read {: got:n :}
   got 0 > if got LINE-U +! then ;


: LINE-ENDED? ( -- bool )
   LINE-U @ 0= if false exit then
   LINE-BUF LINE-U @ + 1- c@ LF = ;


\ The listener has either reported its failed accept or left.
: SETTLED? ( -- bool )
   TAKE-LINE
   LINE-ENDED? HTTP:ENDED-TASKS 0 > or ;


: RAW-OPEN ( -- TCP4:connection )
   LOOPBACK TCP4:ADDRESS HTTP:PORT TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF ENDOF
      failed OF drop -1 TCP4:>CONNECTION ENDOF
   ;MATCH ;


: DROP-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;


\ The answer's first bytes, once some have arrived within ANSWER-MS. A refused
\ read answers none and leaves its errno in READ-ERRNO.
: ANSWER$ ( TCP4:connection -- ptr u8 n ) {: conn:TCP4:connection :}
   0 READ-ERRNO !
   conn ANSWER-MS >MS TCP4:READABLE-WITHIN?
   MATCH TCP4:ready-result
      ready OF ENDOF
      idle OF s" http-accept-starved ANSWER$" TEST-WAIT:EXPIRED ENDOF
      failed OF drop ENDOF
   ;MATCH
   conn RES-BUF RES-CAP TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N ENDOF
      closed OF BLEN>N ENDOF
      failed OF TCP4:ERRNO>N READ-ERRNO ! 0 ENDOF
   ;MATCH {: got:n :}
   RES-BUF got STATUS-BYTES min ;


: GET ( -- TCP4:connection )
   RAW-OPEN {: conn:TCP4:connection :}
   conn s\" GET /ok HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n"
   TCP4:TRANSFER-BYTES TCP4:WRITE DROP-STATUS
   conn ;


\ Linux's accept(2) takes a descriptor before it takes the connection off the
\ queue, so EMFILE leaves the connection queued and the listener answers it
\ later. macOS takes the connection first and resets it when no descriptor is
\ left (xnu accept_nocancel).
: STARVED-ANSWER ( TCP4:connection -- ) {: conn:TCP4:connection :}
   HB-TARGET-MACOS? if
      s" macOS resets the starved connection" T-LABEL
      conn ANSWER$ nip 0 T=
      READ-ERRNO @ ECONNRESET T=
      exit
   then
   s" Linux answers the starved connection" T-LABEL
   conn ANSWER$ s" HTTP/1.1 200" T$= ;


: STARVED-CASE ( -- )
   STDERR-TO-PIPE
   RLIMIT-NOFILE LIMITS GET-LIMIT 0 T=
   LIMITS @ SAVED-SOFT !
   SOFT-LIMIT LIMIT!
   s" the soft limit refuses a descriptor" T-LABEL FILL TTRUE
   PIPE-W @ FD:STDERR FD>N DUP2 FD:STDERR FD>N T=
   CLOSE-LAST-FILLER
   GET {: starved:TCP4:connection :}
   [: SETTLED? ;] mono-ns ANSWER-MS NS-PER-MS * +
   s" http-accept-starved SETTLED?" TEST-WAIT:POLL
   STDERR-BACK
   s" the listener outlives a starved accept" T-LABEL HTTP:ENDED-TASKS 0 T=
   s" the starved accept is one stderr line" T-LABEL
   LINE$ s\" http: accept failed, errno 24\n" T$=
   CLOSE-FILLERS
   SAVED-SOFT @ LIMIT!
   starved STARVED-ANSWER
   starved TCP4:CLOSE DROP-STATUS
   GET {: fresh:TCP4:connection :}
   s" the listener answers once descriptors are free" T-LABEL
   fresh ANSWER$ s" HTTP/1.1 200" T$=
   fresh TCP4:CLOSE DROP-STATUS ;


: RUN ( -- )
   AIO:START
   s" GET" s" /ok" [: OK ;] HTTP:ROUTE
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   STARVED-CASE
   HTTP:STOP
   HTTP:ENDED-TASKS HTTP:TASK-TOTAL T=
   AIO:STOP ;


T-RESET
RUN
T-REPORT
s" http-accept-starved: ok" type LF emit

;package
