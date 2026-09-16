\ genio-test.f - a device carries the engine's own text, and a loopback TCP
\ connection carries a REPL. Run: bin/hb --load lib/genio-test.f
\
\ WHY THERE IS NO SECOND TASK. The REPL case sends a checked colon definition,
\ which is a COMPILE, and Habu forbids dictionary mutation while any task is
\ live (docs/threads.md; the engine exits $4F naming the rejected token). Both
\ ends of the connection therefore live in the main task: the client end writes
\ its whole session and half-closes, then the server end serves it. The socket
\ buffer holds the session and the replies, so neither side waits on the other.
require lib/test.f
require lib/prelude.f
require lib/string.f
require lib/net/tcp4.f
require lib/genio.f

package GENIO-TEST

CAST: BLEN>N ( NUM:byte-len -- n )

$7F000001 constant LOOPBACK
4 constant BACKLOG
$100 constant BUF-CAP
$A constant LF
$2A constant MEM-MARK             \ the memory device's own state, read back through SELF-STATE

\ ---- a memory device: eight operations over two byte buffers ----------------

create MEM-OUT BUF-CAP allot
create MEM-IN BUF-CAP allot
create LINE-BUF BUF-CAP allot
create READ-BUF BUF-CAP allot
create WIRE-BUF BUF-CAP allot
variable MEM-OUT-N
variable MEM-IN-N
variable MEM-IN-POS
variable MEM-FLUSHES
variable MEM-CLOSES
variable MEM-STATE-SEEN
variable MEM-DEV
variable SERVER-DEV
variable LISTENER
variable CLIENT
variable SERVER
variable WIRE-N
variable LINES-SERVED

: MEM-RESET ( -- )
   0 MEM-OUT-N !  0 MEM-IN-N !  0 MEM-IN-POS !
   0 MEM-FLUSHES !  0 MEM-CLOSES !  0 MEM-STATE-SEEN ! ;

: MEM-OUT$ ( -- ptr u8 n )
   MEM-OUT MEM-OUT-N @ ;

: MEM-FEED ( ptr u8 n -- ) {: src:ptr len:n :}
   len BUF-CAP > if GENIO:E-OPERAND throw then
   len 0 ?do src i + c@ MEM-IN i + c! loop
   len MEM-IN-N !
   0 MEM-IN-POS ! ;

\ Every operation records the state cell it was handed, so a wrong row shows up
\ as a wrong mark rather than as silence.
: MEM-NOTE-SELF ( -- )
   GENIO:SELF-STATE MEM-STATE-SEEN ! ;

: MEM-PUT ( n -- ) {: c:n :}
   MEM-NOTE-SELF
   MEM-OUT-N @ BUF-CAP >= if GENIO:E-OPERAND throw then
   c MEM-OUT MEM-OUT-N @ + c!
   MEM-OUT-N @ 1+ MEM-OUT-N ! ;

: MEM-WRITE ( ptr u8 n -- ) {: src:ptr len:n :}
   len 0 ?do src i + c@ MEM-PUT loop ;

: MEM-LEFT ( -- n )
   MEM-IN-N @ MEM-IN-POS @ - ;

: MEM-READY? ( -- bool )
   MEM-NOTE-SELF
   MEM-LEFT 0 > ;

: MEM-KEY ( -- n )
   MEM-NOTE-SELF
   MEM-LEFT 0 <= if GENIO:E-IO throw then
   MEM-IN MEM-IN-POS @ + c@
   MEM-IN-POS @ 1+ MEM-IN-POS ! ;

: MEM-READ ( ptr u8 n -- n ) {: dst:ptr cap:n :}
   MEM-NOTE-SELF
   MEM-LEFT cap min {: got:n :}
   got 0 ?do MEM-IN MEM-IN-POS @ i + + c@ dst i + c! loop
   MEM-IN-POS @ got + MEM-IN-POS !
   got ;

: MEM-ACCEPT ( ptr u8 n -- n ) {: dst:ptr cap:n :}
   MEM-NOTE-SELF
   0
   begin
      MEM-LEFT 0 <= if exit then
      MEM-IN MEM-IN-POS @ + c@ {: c:n :}
      MEM-IN-POS @ 1+ MEM-IN-POS !
      c LF = if exit then
      dup cap >= if GENIO:E-OPERAND throw then
      dup dst swap + c swap c!
      1+
   again ;

: MEM-FLUSH ( -- )
   MEM-NOTE-SELF
   MEM-FLUSHES @ 1+ MEM-FLUSHES ! ;

: MEM-CLOSE ( -- )
   MEM-NOTE-SELF
   MEM-CLOSES @ 1+ MEM-CLOSES ! ;

: MEM-BUILD ( -- GENIO:device )
   [: MEM-PUT ;] [: MEM-KEY ;] [: MEM-READY? ;] [: MEM-READ ;]
   [: MEM-WRITE ;] [: MEM-ACCEPT ;] [: MEM-FLUSH ;] [: MEM-CLOSE ;]
   MEM-MARK GENIO:DEVICE ;

: MEM@ ( -- GENIO:device )
   MEM-DEV @ GENIO:>DEVICE ;

: TO-TERMINAL ( -- )
   GENIO:TERMINAL GENIO:OUTPUT!
   GENIO:TERMINAL GENIO:INPUT! ;

: TO-MEMORY ( -- )
   MEM@ GENIO:OUTPUT!
   MEM@ GENIO:INPUT! ;

\ ---- the engine's own text reaches a device ---------------------------------
\ `emit`, `type` and `.` are engine primitives writing through the funnel in
\ src/habu/rt.f, not GENIO words. If the device captures what they produce, the
\ funnel dispatched; if it does not, the bytes went to the process terminal and
\ this assertion sees an empty buffer.

: ENGINE-TEXT ( -- )
   65 emit
   s" bc" type
   42 . ;

: T-ENGINE-ROUTE ( -- )
   s" emit, type and . reach the current output device" T-LABEL
   MEM-RESET
   TO-MEMORY
   ENGINE-TEXT
   TO-TERMINAL
   MEM-OUT$ s\" Abc42\n" T$=
   s" the operation was handed its own state cell" T-LABEL
   MEM-STATE-SEEN @ MEM-MARK T= ;

: T-LIBRARY-ROUTE ( -- )
   s" GENIO:EMIT and GENIO:TYPE reach the same device" T-LABEL
   MEM-RESET
   TO-MEMORY
   $58 GENIO:EMIT
   s" yz" GENIO:TYPE
   GENIO:FLUSH
   TO-TERMINAL
   MEM-OUT$ s" Xyz" T$=
   s" flush dispatched" T-LABEL
   MEM-FLUSHES @ 1 T= ;

: T-INPUT-OPERATIONS ( -- )
   s" key? is false on an empty device" T-LABEL
   MEM-RESET
   TO-MEMORY
   GENIO:KEY? {: empty:bool :}
   s\" hi\nrest" MEM-FEED
   GENIO:KEY? {: loaded:bool :}
   GENIO:KEY {: first:n :}
   LINE-BUF BUF-CAP GENIO:ACCEPT {: linelen:n :}
   READ-BUF BUF-CAP GENIO:READ {: readlen:n :}
   TO-TERMINAL
   empty TFALSE
   s" key? is true once bytes are waiting" T-LABEL
   loaded TTRUE
   s" key answers the first byte" T-LABEL
   first $68 T=
   s" accept answers the rest of the line without its newline" T-LABEL
   LINE-BUF linelen s" i" T$=
   s" read answers the bytes after the line" T-LABEL
   READ-BUF readlen s" rest" T$= ;

\ ---- close reclaims the row, and the handle dies with it --------------------

variable SPARE-DEV
variable ROUNDS

: SPARE@ ( -- GENIO:device )
   SPARE-DEV @ GENIO:>DEVICE ;

: CLOSE-SPARE ( -- )
   SPARE@ GENIO:CLOSE ;

: SELECT-SPARE ( -- )
   SPARE@ GENIO:OUTPUT! ;

: T-CLOSE-DISPATCH ( -- )
   s" close dispatches to the device" T-LABEL
   MEM-RESET
   MEM-BUILD GENIO:DEVICE>N SPARE-DEV !
   CLOSE-SPARE
   MEM-CLOSES @ 1 T=
   s" a handle to a closed device is refused" T-LABEL
   [: CLOSE-SPARE ;] GENIO:E-STATE TTHROWSQ
   s" and it cannot be selected either" T-LABEL
   [: SELECT-SPARE ;] GENIO:E-STATE TTHROWSQ ;

\ The whole table, opened and closed many times over. Without reclamation the
\ ninth device would be refused; the count here is fifty times the table.
: DECLARED-ROWS ( -- n )
   data-base SNAP-RELOC:XTCELL-N-CELL + @ ;

: T-ROWS-RECLAIMED ( -- )
   s" rows come back, so the table does not run out" T-LABEL
   0 ROUNDS !
   DECLARED-ROWS {: declared:n :}
   50 0 ?do
      8 0 ?do
         MEM-BUILD GENIO:DEVICE>N SPARE-DEV !
         CLOSE-SPARE
         ROUNDS @ 1+ ROUNDS !
      loop
   loop
   ROUNDS @ 400 T=
   \ Re-registering a row's write operation re-declares the SAME engine cell,
   \ which the mark path answers from its index instead of appending a row, so
   \ reclaiming a device row does not move the leak into the address table.
   s" and re-registering a row declares no new address cell" T-LABEL
   DECLARED-ROWS declared T=
   s" and a reused row answers a different handle" T-LABEL
   MEM-BUILD GENIO:DEVICE>N SPARE-DEV !
   SPARE-DEV @ MEM-DEV @ T<>
   CLOSE-SPARE ;

\ Eight rows, all taken, and the ninth device has nowhere to go. The failure is
\ E-GENIO-FULL and not E-GENIO-STATE: the table is intact and every handle in
\ it is still good.
$8 constant ALL-ROWS
ALL-ROWS cells constant HELD-BYTES
create HELD HELD-BYTES allot
variable HELD-N

\ Devices are taken until the table refuses, rather than a fixed count, because
\ the suite is already holding one and this case should not have to know that.
: HOLD-ONE ( -- )
   HELD-N @ ALL-ROWS >= if s" held more rows than the table has" T-FAIL-AS exit then
   MEM-BUILD GENIO:DEVICE>N HELD HELD-N @ cells + !
   HELD-N @ 1+ HELD-N ! ;

: FILL-TABLE ( -- n )
   0 HELD-N !
   begin
      [: HOLD-ONE ;] catch dup 0 <> if
         GENIO:E-FULL T=
         HELD-N @ exit
      then
      drop
   again ;

: RELEASE-HELD ( -- )
   HELD-N @ 0 ?do HELD i cells + @ GENIO:>DEVICE GENIO:CLOSE loop
   0 HELD-N ! ;

: T-TABLE-FULL ( -- )
   s" the device past the last free row is refused with E-GENIO-FULL" T-LABEL
   FILL-TABLE {: held:n :}
   s" and the table did hold devices before it refused" T-LABEL
   held 0 > TTRUE
   s" closing one makes room again" T-LABEL
   HELD-N @ 1- HELD-N !
   HELD-N @ cells HELD + @ GENIO:>DEVICE GENIO:CLOSE
   HOLD-ONE
   RELEASE-HELD
   s" and the table is back to where it started" T-LABEL
   GENIO:OUTPUT@ GENIO:DEVICE>N 0 T= ;

\ The engine's cell still names the closed row; the funnel finds it empty and
\ writes to the terminal, so the one space below is on this suite's stdout by
\ design. What must NOT happen is the byte landing in the closed device.
: T-CLOSED-CURRENT ( -- )
   MEM-RESET
   MEM-BUILD GENIO:DEVICE>N SPARE-DEV !
   SELECT-SPARE
   CLOSE-SPARE
   $20 emit
   s" a closed current device leaves the task on the terminal" T-LABEL
   GENIO:OUTPUT@ GENIO:DEVICE>N 0 T=
   s" and its buffer takes nothing more" T-LABEL
   MEM-OUT-N @ 0 T= ;

\ ---- a close operation that throws still gives the row back -----------------
\ A close that fails is how a device normally dies: TCP-CLOSE answers
\ E-GENIO-IO when the descriptor is already gone. The caller has to hear it,
\ and the row has to come back anyway -- held in CLOSING it would be neither
\ live nor free for the life of the process, and the whole table would bleed a
\ row per failed close.

variable BAD-CLOSES
variable BAD-DEV

: BAD-CLOSE ( -- )
   BAD-CLOSES @ 1+ BAD-CLOSES !
   GENIO:E-IO throw ;

: BAD-BUILD ( -- GENIO:device )
   [: MEM-PUT ;] [: MEM-KEY ;] [: MEM-READY? ;] [: MEM-READ ;]
   [: MEM-WRITE ;] [: MEM-ACCEPT ;] [: MEM-FLUSH ;] [: BAD-CLOSE ;]
   MEM-MARK GENIO:DEVICE ;

: CLOSE-BAD ( -- )
   BAD-DEV @ GENIO:>DEVICE GENIO:CLOSE ;

: T-CLOSE-THROWS ( -- )
   MEM-RESET
   0 BAD-CLOSES !
   FILL-TABLE {: before:n :}
   RELEASE-HELD
   BAD-BUILD GENIO:DEVICE>N BAD-DEV !
   s" a close operation's error reaches the caller" T-LABEL
   [: CLOSE-BAD ;] GENIO:E-IO TTHROWSQ
   s" and the close operation did run" T-LABEL
   BAD-CLOSES @ 1 T=
   s" the handle is dead afterwards" T-LABEL
   [: CLOSE-BAD ;] GENIO:E-STATE TTHROWSQ
   FILL-TABLE {: after:n :}
   s" and the row came back: the whole table is available again" T-LABEL
   after before T=
   RELEASE-HELD ;

\ ---- WITH-IO ----------------------------------------------------------------

: BOOM ( -- )
   GENIO:E-OPERAND throw ;

: WITH-IO-THROWS ( -- )
   MEM@ MEM@ [: BOOM ;] GENIO:WITH-IO ;

: T-WITH-IO ( -- )
   s" WITH-IO restores the caller's devices on a normal return" T-LABEL
   MEM-RESET
   TO-TERMINAL
   MEM@ MEM@ [: ENGINE-TEXT ;] GENIO:WITH-IO
   GENIO:OUTPUT@ GENIO:DEVICE>N 0 T=
   s" and the quotation's output went to the device" T-LABEL
   MEM-OUT$ s\" Abc42\n" T$=
   s" WITH-IO restores them on a throw, and the throw survives" T-LABEL
   [: WITH-IO-THROWS ;] GENIO:E-OPERAND TTHROWSQ
   s" the caller's output device is the terminal again" T-LABEL
   GENIO:OUTPUT@ GENIO:DEVICE>N 0 T=
   s" the caller's input device is the terminal again" T-LABEL
   GENIO:INPUT@ GENIO:DEVICE>N 0 T= ;

\ ---- a task starts on its creator's devices ---------------------------------
\ The funnel reads the RUNNING task's DATA, so this also shows that a worker's
\ own `type` reaches the device rather than the process terminal.

TASK:MIN-STACK TASK:TASK WORKER

: WORKER-BODY ( -- )
   s" worker" type ;

: T-TASK-INHERITS ( -- )
   MEM-RESET
   TO-MEMORY
   WORKER TASK:PREPARE
   ['] WORKER-BODY WORKER TASK:ACTIVATE
   begin WORKER TASK:DONE? until
   WORKER TASK:KILL
   TO-TERMINAL
   s" a task inherits its creator's output device" T-LABEL
   MEM-OUT$ s" worker" T$= ;

\ ---- the registry under concurrent tasks ------------------------------------
\ Building a device on the connection a task just accepted is the normal case,
\ so two tasks claiming rows at the same time is the normal case too. The
\ invariant is that no two LIVE handles are ever equal: a handle carries its
\ row and that row's generation, and the generation only moves on release, so
\ two equal handles could only mean two tasks were handed one row at once.
\
\ Nothing here compiles: every word, task and device operation is defined
\ before the first ACTIVATE, because dictionary mutation while a task is live
\ ends the process (docs/threads.md).

3 constant WORKERS
200 constant WORK-ROUNDS

TASK:MIN-STACK TASK:TASK WORKER-A
TASK:MIN-STACK TASK:TASK WORKER-B
TASK:MIN-STACK TASK:TASK WORKER-C

\ Atomic cells need native alignment (docs/threads.md).
here data-base - negate 7 and allot
WORKERS cells constant HOLDING-BYTES
create HOLDING HOLDING-BYTES allot     \ each worker's handle while it holds one
variable COLLISIONS
variable BUILDS
variable REFUSALS

: HOLDING-CELL ( n -- ptr n ) {: slot:n :}
   slot cells HOLDING + ;

\ A worker's own device: eight operations that touch nothing shared, so this
\ case measures the registry and not the device.
: W-EMIT ( n -- ) drop ;
: W-KEY ( -- n ) 0 ;
: W-READY? ( -- bool ) false ;
: W-READ ( ptr u8 n -- n ) drop drop 0 ;
: W-WRITE ( ptr u8 n -- ) drop drop ;
: W-ACCEPT ( ptr u8 n -- n ) drop drop 0 ;
: W-NOTHING ( -- ) ;

: W-BUILD ( -- GENIO:device )
   [: W-EMIT ;] [: W-KEY ;] [: W-READY? ;] [: W-READ ;]
   [: W-WRITE ;] [: W-ACCEPT ;] [: W-NOTHING ;] [: W-NOTHING ;]
   MEM-MARK GENIO:DEVICE ;

\ Every other worker's published handle, compared against mine. Equal means one
\ row was handed out twice, which is the thing this case exists to catch.
: W-CHECK ( n n -- ) {: slot:n mine:n :}
   WORKERS 0 ?do
      i slot <> if
         i HOLDING-CELL atomic@ mine = if 1 COLLISIONS atomic-add drop then
      then
   loop ;

: W-ROUND ( n -- ) {: slot:n :}
   W-BUILD GENIO:DEVICE>N {: handle:n :}
   handle slot HOLDING-CELL atomic!
   1 BUILDS atomic-add drop
   slot handle W-CHECK
   0 slot HOLDING-CELL atomic!
   handle GENIO:>DEVICE GENIO:CLOSE ;

\ A worker carries its own slot in a task-local cell rather than a local,
\ because a quotation cannot reference one (docs/forth.md) and the round has to
\ run under `catch`.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $8 TASK:+USER MY-SLOT drop

: W-ROUND-SELF ( -- )
   MY-SLOT @ W-ROUND ;

\ A full table is an ordinary outcome when three tasks share eight rows: it is
\ counted, not a failure, and the round is simply skipped.
: W-AFTER ( n n -- ) {: code:n slot:n :}
   code 0 = if exit then
   code GENIO:E-FULL = if
      1 REFUSALS atomic-add drop
      0 slot HOLDING-CELL atomic!
      exit
   then
   code throw ;

: W-LOOP ( n -- ) {: slot:n :}
   slot MY-SLOT !
   WORK-ROUNDS 0 ?do
      [: W-ROUND-SELF ;] catch MY-SLOT @ W-AFTER
      TASK:PAUSE
   loop ;

: W-BODY-A ( -- ) 0 W-LOOP ;
: W-BODY-B ( -- ) 1 W-LOOP ;
: W-BODY-C ( -- ) 2 W-LOOP ;

: START-WORKERS ( -- )
   WORKER-A TASK:PREPARE  WORKER-B TASK:PREPARE  WORKER-C TASK:PREPARE
   ['] W-BODY-A WORKER-A TASK:ACTIVATE
   ['] W-BODY-B WORKER-B TASK:ACTIVATE
   ['] W-BODY-C WORKER-C TASK:ACTIVATE ;

: JOIN-WORKERS ( -- )
   begin WORKER-A TASK:DONE? until
   begin WORKER-B TASK:DONE? until
   begin WORKER-C TASK:DONE? until
   WORKER-A TASK:KILL  WORKER-B TASK:KILL  WORKER-C TASK:KILL ;

: T-CONCURRENT-REGISTRY ( -- )
   0 COLLISIONS !  0 BUILDS !  0 REFUSALS !
   WORKERS 0 ?do 0 i HOLDING-CELL atomic! loop
   FILL-TABLE {: before:n :}
   RELEASE-HELD
   START-WORKERS
   JOIN-WORKERS
   s" concurrent tasks never share a row" T-LABEL
   COLLISIONS @ 0 T=
   s" and they really did build devices" T-LABEL
   BUILDS @ 0 > TTRUE
   FILL-TABLE {: after:n :}
   s" and the table is back to the size it started at" T-LABEL
   after before T=
   RELEASE-HELD ;

\ ---- a REPL over a loopback TCP connection ----------------------------------

: NEW-LISTENER ( -- TCP4:listener )
   0 TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF ENDOF
      failed OF drop s" bind failed" T-FAIL-AS 0 TCP4:>LISTENER ENDOF
   ;MATCH {: lis:TCP4:listener :}
   lis BACKLOG TCP4:LISTEN
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop s" listen failed" T-FAIL-AS ENDOF
   ;MATCH
   lis ;

: LISTENER-PORT ( TCP4:listener -- n )
   TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N nip ENDOF
      failed OF drop s" local failed" T-FAIL-AS 0 ENDOF
   ;MATCH ;

: CONNECT-TO ( n -- TCP4:connection )
   LOOPBACK TCP4:ADDRESS swap TCP4:PORT TCP4:CONNECT
   MATCH TCP4:connect-result
      connected OF ENDOF
      failed OF drop s" connect failed" T-FAIL-AS 0 TCP4:>CONNECTION ENDOF
   ;MATCH ;

: ACCEPT-ONE ( TCP4:listener -- TCP4:connection )
   TCP4:ACCEPT
   MATCH TCP4:accept-result
      accepted OF drop drop ENDOF
      failed OF drop s" accept failed" T-FAIL-AS 0 TCP4:>CONNECTION ENDOF
   ;MATCH ;

: SEND$ ( TCP4:connection ptr u8 n -- ) {: con:TCP4:connection src:ptr len:n :}
   con src len TCP4:TRANSFER-BYTES TCP4:WRITE
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop s" write failed" T-FAIL-AS ENDOF
   ;MATCH ;

: HALF-CLOSE ( TCP4:connection -- )
   TCP4:SENDING TCP4:SHUTDOWN
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop s" shutdown failed" T-FAIL-AS ENDOF
   ;MATCH ;

: SESSION$ ( -- ptr u8 n )
   s\" : GENIO-T-DOUBLE ( n -- n ) 2 * ;\n21 GENIO-T-DOUBLE .\n" ;

\ The REPL loop: the engine's own line-reader vector, the audited evaluate the
\ loader uses, and the ok the engine prints. Every byte of it leaves through
\ the output funnel, so it is on the connection.
: SERVE-LINE ( -- bool )
   REPL-READ {: line:ptr len:n :}
   len 0 = if false exit then
   LINES-SERVED @ 1+ LINES-SERVED !
   line len INCLUDE-EVALUATE
   s" ok" type cr
   true ;

: SERVE ( -- )
   begin SERVE-LINE while repeat ;

: DRAIN ( TCP4:connection -- ) {: con:TCP4:connection :}
   0 WIRE-N !
   begin
      con WIRE-BUF WIRE-N @ + BUF-CAP WIRE-N @ - TCP4:TRANSFER-BYTES TCP4:READ
      MATCH TCP4:read-result
         data OF BLEN>N ENDOF
         closed OF drop 0 ENDOF
         failed OF drop s" drain failed" T-FAIL-AS 0 ENDOF
      ;MATCH
      dup 0 = if drop exit then
      WIRE-N @ + WIRE-N !
      WIRE-N @ BUF-CAP >= if exit then
   again ;

: T-TCP-REPL ( -- )
   s" a REPL runs over a loopback TCP connection" T-LABEL
   0 LINES-SERVED !
   NEW-LISTENER TCP4:LISTENER>N LISTENER !
   LISTENER @ TCP4:>LISTENER LISTENER-PORT CONNECT-TO TCP4:CONNECTION>N CLIENT !
   LISTENER @ TCP4:>LISTENER ACCEPT-ONE TCP4:CONNECTION>N SERVER !
   CLIENT @ TCP4:>CONNECTION SESSION$ SEND$
   CLIENT @ TCP4:>CONNECTION HALF-CLOSE
   SERVER @ TCP4:>CONNECTION GENIO:TCP-DEVICE GENIO:DEVICE>N SERVER-DEV !
   SERVER-DEV @ GENIO:>DEVICE SERVER-DEV @ GENIO:>DEVICE [: SERVE ;] GENIO:WITH-IO
   SERVER-DEV @ GENIO:>DEVICE GENIO:CLOSE
   CLIENT @ TCP4:>CONNECTION DRAIN
   LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop s" close-listener failed" T-FAIL-AS ENDOF
   ;MATCH
   LINES-SERVED @ 2 T=
   s" the client reads the definition's ok, the query's answer and its ok" T-LABEL
   WIRE-BUF WIRE-N @ s\" ok\n42\nok\n" T$=
   s" the client's connection closes cleanly" T-LABEL
   CLIENT @ TCP4:>CONNECTION TCP4:CLOSE
   MATCH TCP4:status
      ok OF 0 ENDOF
      failed OF TCP4:ERRNO>N ENDOF
   ;MATCH 0 T= ;

\ ---- a peer that goes away fails with the named code, and does not spin ------

: READ-THROUGH-DEAD-PEER ( -- )
   SERVER-DEV @ GENIO:>DEVICE GENIO:INPUT!
   GENIO:KEY drop ;

: T-PEER-GONE ( -- )
   s" a read through a closed peer fails with E-GENIO-IO" T-LABEL
   NEW-LISTENER TCP4:LISTENER>N LISTENER !
   LISTENER @ TCP4:>LISTENER LISTENER-PORT CONNECT-TO TCP4:CONNECTION>N CLIENT !
   LISTENER @ TCP4:>LISTENER ACCEPT-ONE TCP4:CONNECTION>N SERVER !
   CLIENT @ TCP4:>CONNECTION TCP4:CLOSE
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop s" client close failed" T-FAIL-AS ENDOF
   ;MATCH
   SERVER @ TCP4:>CONNECTION GENIO:TCP-DEVICE GENIO:DEVICE>N SERVER-DEV !
   [: READ-THROUGH-DEAD-PEER ;] GENIO:E-IO TTHROWSQ
   TO-TERMINAL
   SERVER-DEV @ GENIO:>DEVICE GENIO:CLOSE
   LISTENER @ TCP4:>LISTENER TCP4:CLOSE-LISTENER
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop s" close-listener failed" T-FAIL-AS ENDOF
   ;MATCH ;

: RUN ( -- )
   T-RESET
   MEM-BUILD GENIO:DEVICE>N MEM-DEV !
   T-ENGINE-ROUTE
   T-LIBRARY-ROUTE
   T-INPUT-OPERATIONS
   T-CLOSE-DISPATCH
   T-ROWS-RECLAIMED
   T-TABLE-FULL
   T-CLOSED-CURRENT
   T-CLOSE-THROWS
   T-CONCURRENT-REGISTRY
   T-WITH-IO
   T-TASK-INHERITS
   T-TCP-REPL
   T-PEER-GONE
   T-REPORT
   s" genio-test: ok" type cr ;

RUN

;package
