\ genio-test.f - a TCP device hands back lines of any length.
\ One process holds both ends of a loopback connection. The client writes every
\ line and shuts its sending side before the server reads, so no read waits on a
\ peer or needs the AIO loop. The terminal is put back before anything is
\ asserted, so the REPL that follows the load reads its own input again.
\ A device write that faults is caught with the output state its catch saved,
\ so the next write reaches its device rather than fd 1.
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
$10 constant SINK-CAP

create SRC SRC-CAP allot
create PART ACCEPT-CAP allot
create SINK SINK-CAP allot

variable LISTENER
variable CLIENT
variable DEV
variable GOT-LEN
variable GOT-SAME
variable WANT-CH
variable GOT-CODE
variable PARTS
variable PART-LAST
variable SINK-N
variable SINK-DEV
variable FAULTY-DEV
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

\ ---- a device write that faults ---------------------------------------------
\ The sink appends what it is written; the faulty device's write reads address 0.

: SINK-WRITE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do
      SINK-N @ SINK-CAP < if a i + c@ SINK SINK-N @ + c!  SINK-N @ 1+ SINK-N ! then
   loop ;

: FAULTY-WRITE ( ptr u8 n -- ) {: a:ptr u:n :}
   NULL-PTR BYTE-VIEW c@ drop ;

: NO-EMIT ( n -- ) drop ;
: NO-KEY ( -- n ) GENIO:E-IO throw ;
: NO-READY? ( -- bool ) false ;
: NO-READ ( ptr u8 n -- n ) {: a:ptr cap:n :} 0 ;
: NOTHING ( -- ) ;

: OUT-DEVICE ( [ ptr u8 n -- ] -- GENIO:device ) {: qwrite :}
   [: NO-EMIT ;] [: NO-KEY ;] [: NO-READY? ;] [: NO-READ ;]
   qwrite [: NO-READ ;] [: NOTHING ;] [: NOTHING ;] 0 GENIO:DEVICE ;

: FAULT-TYPE ( -- ) s" x" type ;
: SINK-TYPE ( -- ) s" y" type ;

: T-FAULTING-WRITE ( -- )
   s" a write after a caught faulting device write reaches its device" T-LABEL
   [: SINK-WRITE ;] OUT-DEVICE GENIO:DEVICE>N SINK-DEV !
   [: FAULTY-WRITE ;] OUT-DEVICE GENIO:DEVICE>N FAULTY-DEV !
   0 SINK-N !
   [: GENIO:TERMINAL FAULTY-DEV @ GENIO:>DEVICE [: FAULT-TYPE ;] GENIO:WITH-IO ;]
   catch GOT-CODE !
   GENIO:SELF GENIO:DEVICE>N {: self:n :}
   GENIO:TERMINAL SINK-DEV @ GENIO:>DEVICE [: SINK-TYPE ;] GENIO:WITH-IO
   FAULTY-DEV @ GENIO:>DEVICE GENIO:CLOSE
   SINK-DEV @ GENIO:>DEVICE GENIO:CLOSE
   GOT-CODE @ ENGINE-ERROR:MEMORY-FAULT T=
   self GENIO:TERMINAL GENIO:DEVICE>N T=
   SINK SINK-N @ s" y" T$= ;

\ ---- the worker ---------------------------------------------------------------
\ The worker's line is released on its exit chain, which TASK:JOIN has run.
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
   \ The x86-64 engine has no memory-fault throw: the fault kills the test.
   HB-TARGET-LINUX-X86-64? 0= if T-FAULTING-WRITE then
   T-REPORT ;

;package

GENIO-TEST:GENIO-TEST-RUN
