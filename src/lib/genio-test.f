\ genio-test.f - a TCP device hands back lines of any length.
\
\ One process holds both ends of a loopback connection. The client writes every
\ line and shuts its sending side down before the server reads, so no read waits
\ on a peer and none needs the AIO loop. The server end becomes a genio TCP
\ device and is the current input while a row reads, then the terminal is put
\ back before anything is asserted, so the REPL that follows the load reads its
\ own input again.
\
\ READ-LINE grows its line instead of refusing one: 1000 bytes spans several of
\ its reads, 512 ends exactly on one, and a worker task reads the 700-byte line
\ into its own line, which its exit chain releases. GENIO:ACCEPT on a stream
\ fills the caller's buffer and leaves the rest of the line for the next ACCEPT,
\ so successive ACCEPTs bring a long line across whole. The stream's end is a
\ zero-length line.
\ Run: bin/hb --load src/lib/genio-test.f </dev/null
require src/lib/test.f
require src/lib/string.f
require src/lib/task.f
require src/lib/net/tcp4.f
require src/lib/genio.f

package GENIO-TEST
private

$7F000001 constant LOOPBACK
1 constant BACKLOG
$A constant LF
$61 constant CH-A
$62 constant CH-B
$63 constant CH-C
$64 constant CH-D
1000 constant LONG-LEN                 \ several READ-LINE reads, the last partial
512 constant EVEN-LEN                  \ READ-LINE's reads end on the line's end
700 constant TASK-LEN                  \ read by the worker
1000 constant ACCEPT-LEN               \ read by successive ACCEPTs
$100 constant ACCEPT-CAP
$800 constant SRC-CAP

create SRC SRC-CAP allot
create PART ACCEPT-CAP allot

variable LISTENER
variable CLIENT
variable DEV
variable GOT-LEN
variable GOT-SAME
variable WANT-CH
variable GOT-CODE
variable PARTS
variable PART-LAST
TASK:MIN-STACK TASK:TASK READER

\ ---- the endpoints -----------------------------------------------------------

: STATUS-OK ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF TCP4:ERRNO>N drop s" tcp status failed" T-FAIL-AS ENDOF
   ;MATCH ;

: BIND-LISTENER ( -- n )
   LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF TCP4:LISTENER>N ENDOF
      failed OF TCP4:ERRNO>N drop GENIO:E-IO throw ENDOF
   ;MATCH ;

: LISTENER-PORT ( -- n )
   LISTENER @ TCP4:>LISTENER TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N swap TCP4:ADDRESS>N drop ENDOF
      failed OF TCP4:ERRNO>N drop GENIO:E-IO throw ENDOF
   ;MATCH ;

: CONNECT-TO ( n -- n )
   LOOPBACK TCP4:ADDRESS swap TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF TCP4:CONNECTION>N ENDOF
      failed OF TCP4:ERRNO>N drop GENIO:E-IO throw ENDOF
   ;MATCH ;

: ACCEPT-FD ( TCP4:accept-result -- n )
   MATCH TCP4:accept-result
      accepted OF TCP4:PORT>N drop TCP4:ADDRESS>N drop TCP4:CONNECTION>N ENDOF
      failed OF TCP4:ERRNO>N drop GENIO:E-IO throw ENDOF
   ;MATCH ;

: OPEN-PAIR ( -- )
   BIND-LISTENER LISTENER !
   LISTENER @ TCP4:>LISTENER BACKLOG TCP4:LISTEN STATUS-OK
   LISTENER-PORT CONNECT-TO CLIENT !
   LISTENER @ TCP4:>LISTENER TCP4:ACCEPT ACCEPT-FD
   TCP4:>CONNECTION GENIO:TCP-DEVICE GENIO:DEVICE>N DEV ! ;

\ ---- the client's lines -------------------------------------------------------

: SEND-LINE ( n n -- ) {: len:n c:n :}
   len 0 ?do c SRC i + c! loop
   LF SRC len + c!
   CLIENT @ TCP4:>CONNECTION SRC len 1+ TCP4:TRANSFER-BYTES TCP4:WRITE STATUS-OK ;

: SEND-ALL ( -- )
   LONG-LEN CH-A SEND-LINE
   EVEN-LEN CH-B SEND-LINE
   TASK-LEN CH-C SEND-LINE
   ACCEPT-LEN CH-D SEND-LINE
   CLIENT @ TCP4:>CONNECTION TCP4:SENDING TCP4:SHUTDOWN STATUS-OK ;

\ ---- reading through the device ------------------------------------------------

: SAME? ( ptr u8 n n -- bool ) {: a:ptr u:n c:n :}
   u 0 ?do a i + c@ c <> if false unloop exit then loop
   true ;

: DEV@ ( -- GENIO:device )
   DEV @ GENIO:>DEVICE ;


\ Each row reads with the device as input and keeps what it saw, its throw
\ included, for the caller to assert once WITH-IO has put the terminal back.
: LINE-BODY ( -- )
   GENIO:READ-LINE {: a:ptr u:n :}
   u GOT-LEN !  a u WANT-CH @ SAME? GOT-SAME ! ;

: PARTS-BODY ( -- )
   0 GOT-LEN !  0 PARTS !  true GOT-SAME !
   begin
      PART ACCEPT-CAP GENIO:ACCEPT {: got:n :}
      PART got WANT-CH @ SAME? 0= if false GOT-SAME ! then
      GOT-LEN @ got + GOT-LEN !  PARTS @ 1+ PARTS !  got PART-LAST !
      got ACCEPT-CAP <
   until ;

: LINE-ROW ( -- )
   ['] LINE-BODY catch GOT-CODE ! ;

: PARTS-ROW ( -- )
   ['] PARTS-BODY catch GOT-CODE ! ;

: ROW ( [ -- ] n -- ) {: body c:n :}
   -1 GOT-LEN !  false GOT-SAME !  0 GOT-CODE !  c WANT-CH !
   DEV@ GENIO:TERMINAL body GENIO:WITH-IO ;

: T-LINE ( n n ptr u8 n -- ) {: want:n c:n la:ptr lu:n :}
   la lu T-LABEL
   [: LINE-ROW ;] c ROW
   GOT-CODE @ 0 T=
   GOT-LEN @ want T=
   GOT-SAME @ TTRUE ;

\ ---- the worker ---------------------------------------------------------------
\ The worker makes the device its own input and reads into its own line; the
\ line's release is on the worker's exit chain, which TASK:JOIN has run.
: READER-BODY ( -- )
   DEV@ GENIO:INPUT!
   GENIO:READ-LINE {: a:ptr u:n :}
   a u CH-C SAME? if u else -1 then TASK:RETURN ;

: T-WORKER ( -- )
   s" a worker reads a 700-byte line" T-LABEL
   ['] READER-BODY READER TASK:ACTIVATE
   READER TASK:JOIN MATCH result
      ok OF TASK-LEN T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;

: T-PARTS ( -- )
   s" successive ACCEPTs bring a 1000-byte line whole" T-LABEL
   [: PARTS-ROW ;] CH-D ROW
   GOT-CODE @ 0 T=
   GOT-LEN @ ACCEPT-LEN T=
   GOT-SAME @ TTRUE
   PARTS @ ACCEPT-LEN ACCEPT-CAP / 1+ T=
   PART-LAST @ ACCEPT-LEN ACCEPT-CAP mod T= ;

: CLOSE-ALL ( -- )
   DEV@ GENIO:CLOSE
   CLIENT @ TCP4:>CONNECTION TCP4:CLOSE STATUS-OK
   LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER STATUS-OK ;

public

: GENIO-TEST-RUN ( -- )
   T-RESET
   OPEN-PAIR
   SEND-ALL
   LONG-LEN CH-A s" READ-LINE answers a 1000-byte line whole" T-LINE
   EVEN-LEN CH-B s" READ-LINE answers a 512-byte line whole" T-LINE
   T-WORKER
   T-PARTS
   0 CH-A s" the end of input is a zero-length line" T-LINE
   CLOSE-ALL
   T-REPORT ;

;package

GENIO-TEST:GENIO-TEST-RUN
