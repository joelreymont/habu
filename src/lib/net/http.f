\ http.f - the HTTP/1.1 server: a listener task on an IPv4 address, a fixed pool
\ of workers taking connections, and the calls SUBMIT queues, from one queue,
\ one handler per request in a catch boundary (a throw answers 500 with the
\ request id). A kept-alive connection waits for its next request parked with
\ the listener, not a worker, so no kept-alive connection delays a queued job;
\ a newly accepted one waits for its first request on a worker, up to IDLE-MS.
\ A handler runs once the head is read; the body is read when the handler asks
\ for it (BODY$), and one it leaves unread closes the connection after the
\ answer. After LOG, every request read writes one LOG INFO access line.
\ Requires http-arena.f, http-request.f, http-response.f, http-router.f and
\ http-static.f.
\ STORAGE CLASS. PROCESS-WIDE: one server per image; its tables and the LOG
\ flag are written before START, and per-request state is each worker's slot
\ row (http-arena.f).
\ TASK-LOCAL: REPLY's connection and drain room, per-task slots.
require src/lib/net/http-static.f
require src/lib/task.f
require src/lib/net/tcp4.f
require src/lib/span.f
require src/lib/aio.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/log.f
require src/lib/net/http-arena.f
require src/lib/net/http-request.f
require src/lib/net/http-response.f
require src/lib/net/http-router.f
require src/lib/time.f
require src/lib/byte-buffer.f

package HTTP

private

$20 constant BACKLOG
$100000 constant WORKER-STACK
$40 constant JOB-SLOTS            \ connections queued before the listener waits for room
$40 constant PARK-SLOTS           \ kept-alive connections parked at once
$8 constant MAX-HOOKS             \ packages that give a worker a resource of its own
$C8 constant LINGER-MS            \ how long a rejected peer may still be sending
$100 constant LINGER-CAP          \ bytes one drain read takes from a lingering peer
$C8 constant STOP-READ-MS         \ the longest the listener's pause or a waiting worker goes without reading the stop flag
$C8 constant STOP-MARGIN-MS       \ over the longest park: scheduling slack and one window's parse
$7FFFFFFF constant MAX-BODY-TIMEOUT-MS  \ TCP4's largest READABLE-WITHIN? poll

ENUM served keep-open answered silent taken ;ENUM

\ What a worker takes from the queue: a connection to serve, a call SUBMIT
\ queued, the end of its loop, or nothing (WORKER-LOOP).
ENUM job connection call stop none ;ENUM

\ The job queue: JOB-N jobs from JOB-AT on, oldest first, read and written
\ under JOB-LOCK, a job's argument in JOB-ARG and a call's quotation in JOB-RUN.
\ The columns grow to hold every job, so neither SUBMIT nor STOP ever waits for
\ room; ROOM counts the free places of the JOB-SLOTS connections may take,
\ which is all the listener waits for. ITEMS counts the jobs queued, signalled
\ under JOB-LOCK as each is queued.
DYNAMIC-BUFFER JOB job
DYNAMIC-BUFFER JOB-ARG n
DYNAMIC-BUFFER JOB-RUN [ n -- ]
SCRATCH-VARIABLE JOB-AT
SCRATCH-VARIABLE JOB-N
SCRATCH-VARIABLE JOB-CAP          \ the jobs the columns hold room for
TASK:FACILITY JOB-LOCK
TASK:SEMAPHORE ITEMS
TASK:SEMAPHORE ROOM
TASK:SEMAPHORE START-READY

\ A kept-alive connection's life once it has been answered with nothing of a
\ next request in its slot's input (SERVE-UNTIL-CLOSED):
\   served - a worker's SERVING row. PARK-CONNECTION parks it, or closes it
\            plainly when every row is taken or the listener takes none.
\   parked - a row below, the slot's no more: handed, until the wake it pokes
\            has the listener poll it to the row's deadline, IDLE-MS past
\            the answer; polled, from then. Readable, it is queued, waiting
\            for ROOM as an accepted connection does; past its deadline it is
\            closed plainly. The listener closes every parked one as it
\            leaves, and STOP closes those of a listener it killed.
\   queued - a connection job: a worker takes it and it is served again, or
\            a stop closes it unserved.
\   closed - its descriptor given back.
\ A row's phase is written under PARK-LOCK; only the listener writes a row
\ that is not vacant, and only it reads a polled row's ticket.
ENUM parking vacant handed polled ;ENUM

PARK-SLOTS TYPED-BUFFER PARK-PHASE parking SCRATCH-ONLY
PARK-SLOTS TYPED-BUFFER PARK-CONN TCP4:connection SCRATCH-ONLY
PARK-SLOTS TYPED-BUFFER PARK-DUE n SCRATCH-ONLY \ mono-ns past which the row is closed
PARK-SLOTS TYPED-BUFFER PARK-TICKET AIO:ticket SCRATCH-ONLY
TASK:FACILITY PARK-LOCK
SCRATCH-VARIABLE PARK-OPEN        \ set while the listener takes rows; under PARK-LOCK

\ What a worker wakes once it has handed a row, and STOP once it has raised the
\ flag; the listener polls it.
TYPED-VARIABLE WAKER AIO:waker SCRATCH-ONLY
SCRATCH-VARIABLE PIPED            \ set while the waker is open

\ What the listener waits on at once: its accept poll, the waker's poll and
\ each polled row's, tagged with the row. A flag is set while its ticket is in
\ the group, unawaited.
AIO:GROUP LISTENING
-1 constant ACCEPT-TAG
-2 constant WAKE-TAG
TYPED-VARIABLE ACCEPT-TICKET AIO:ticket SCRATCH-ONLY
TYPED-VARIABLE WAKE-TICKET AIO:ticket SCRATCH-ONLY
SCRATCH-VARIABLE ACCEPT-ARMED
SCRATCH-VARIABLE WAKE-ARMED

WORKER-STACK TASK:TASK LISTENER-TASK
WORKER-STACK TASK:TASK WORKER-0
WORKER-STACK TASK:TASK WORKER-1
WORKER-STACK TASK:TASK WORKER-2
WORKER-STACK TASK:TASK WORKER-3
WORKER-STACK TASK:TASK WORKER-4
WORKER-STACK TASK:TASK WORKER-5
WORKER-STACK TASK:TASK WORKER-6
WORKER-STACK TASK:TASK WORKER-7

MAX-WORKERS TYPED-BUFFER WORKER-TCB ^n

\ The job each worker took last: its argument and, for a call, its quotation.
MAX-WORKERS TYPED-BUFFER TAKEN-ARG n SCRATCH-ONLY
MAX-WORKERS TYPED-BUFFER TAKEN-RUN [ n -- ] SCRATCH-ONLY

\ The task that raised STOPPING, in the same row type the pool's tasks live in.
\ Zero is the main thread's own record, which is the thread every caller of STOP
\ is on, so a hint before the first stop still reaches a thread that re-checks.
1 TYPED-BUFFER STOPPER-TCB ^n SCRATCH-ONLY

WORKER-0 0 WORKER-TCB !
WORKER-1 1 WORKER-TCB !
WORKER-2 2 WORKER-TCB !
WORKER-3 3 WORKER-TCB !
WORKER-4 4 WORKER-TCB !
WORKER-5 5 WORKER-TCB !
WORKER-6 6 WORKER-TCB !
WORKER-7 7 WORKER-TCB !

SCRATCH-VARIABLE LISTENER-CELL
SCRATCH-VARIABLE BOUND-PORT
SCRATCH-VARIABLE BOUND-ADDRESS
SCRATCH-VARIABLE WORKER-COUNT
SCRATCH-VARIABLE IDLE-MS
SCRATCH-VARIABLE CLAIMED          \ slots the started workers have taken
SCRATCH-VARIABLE ENDED-COUNT      \ tasks that reached the end of their body
SCRATCH-VARIABLE LEFT-COUNT       \ tasks that left their body, however they left it
SCRATCH-VARIABLE KILLED-COUNT     \ tasks a stop had to kill past its bound
SCRATCH-VARIABLE START-ERROR      \ first failed start hook's actual throw code
SCRATCH-VARIABLE STARTED          \ workers successfully activated this start
SCRATCH-VARIABLE ACCEPTED-COUNT   \ connections the listener has taken since START
variable LOGGING                  \ set by LOG: each request read writes its line

\ REPLY's drain room: its task has no worker slot, whose input buffer a worker
\ drains into. REPLYING is the connection REPLY's close takes on every way out.
TASK:+USER REPLYING n
LINGER-CAP TASK:+USERS LINGER-SCRATCH u8

\ No task is live while this file loads, so the locks are made ready here;
\ they are never destroyed.
JOB-LOCK TASK:FACILITY-INIT
PARK-LOCK TASK:FACILITY-INIT


public

private

: LISTENER@ ( -- TCP4:listener )
   LISTENER-CELL @ TCP4:>LISTENER ;


\ The last thing every task of this server does: count itself out of the server's
\ own work, then hint at the task that raised STOPPING, so a stop parks in the
\ kernel rather than polling. The count moves before the hint, which is what lets
\ the stopper trust one hint - WAKE is a hint and not a message
\ (src/lib/task.f), so it re-reads the count after each one. A hint posted
\ outside a stop - a listener whose accept failed raises the flag itself - only
\ returns the stopper's thread to a check it repeats.
: LEAVING ( -- )
   1 LEFT-COUNT atomic-add drop
   STOPPING? 0= if exit then
   0 STOPPER-TCB @ TASK:WAKE ;


: DROP-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;


\ The fault reaches the operator with the same request id the client is given.
: REPORT-FAULT ( n n -- )
   {: idx:n code:n :}
   LOG:ERROR s" http" LOG:LINE
   s" request " LOG:TEXT+  idx SLOT-ID$ LOG:TEXT+
   s"  failed, throw " LOG:TEXT+  code LOG:CODE+
   LOG:;LINE ;


\ Whatever had been built is dropped: the client is owed one answer and the
\ caller is about to render it.
: DROP-ANSWER ( n -- ) {: idx:n :}
   idx HDR-BUF BUF:CLEAR
   idx DROP-BODY ;


\ The error hook runs outside the handler's boundary for a rejection and for a
\ fault, under a catch of its own. A hook that throws there is reported like a
\ handler's fault, whatever it built is dropped, and the client gets the plain
\ text default under the status it was owed, so the worker still answers and
\ lives on. PLAIN-ERROR is called directly: ERROR! would run the same hook.
: HOOK-FAULT ( n n -- ) {: idx:n code:n :}
   code 0= if exit then
   idx code REPORT-FAULT
   idx DROP-ANSWER
   idx RESPONSE-OF idx SLOT-STATUS@ s" error_hook"
   s" the error hook did not finish this answer" PLAIN-ERROR ;


: FAULT-RESPONSE ( n -- ) {: idx:n :}
   idx DROP-ANSWER
   idx RESPONSE-OF 500 s" internal"
   s" the handler did not finish this request" ERROR! ;


\ Waits for the first byte of a request, nothing of it buffered, reading
\ STOPPING? at least every STOP-READ-MS: false once a stop has been raised,
\ true once the peer speaks or the deadline has passed, which READ-HEAD then
\ reads as it reads any fill.
: AWAIT-REQUEST ( TCP4:connection n -- bool ) {: conn:TCP4:connection deadline:n :}
   begin
      STOPPING? if false exit then
      mono-ns deadline >= if true exit then
      conn STOP-READ-MS DEADLINE-AT deadline min WAIT-READABLE
   until
   true ;


\ Reads the validated head and selects its first route, whose explicit cap or
\ the final server default bounds the body. A body framing the head alone
\ rejects is answered here, before any handler runs; the body itself is read
\ only when the handler asks (BODY$). A stop raised while the connection is
\ quiet before a request's first byte closes it.
: READ-REQUEST ( TCP4:connection n n -- parse-result )
   {: conn:TCP4:connection idx:n ms:n :}
   idx ms READ-DEADLINE {: deadline:n :}
   idx SLOT-RESET
   idx COMPACT
   idx IN-U@ 0= if
      conn deadline AWAIT-REQUEST 0= if CLOSED exit then
   then
   conn idx deadline READ-HEAD {: head:parse-result :}
   head COMPLETE? 0= if head exit then
   idx PARSE-HEAD {: parsed:parse-result :}
   parsed COMPLETE? 0= if parsed exit then
   idx HOST-OK? 0= if BAD exit then
   idx DECIDE-KEEP-ALIVE
   idx FIND-ROUTE {: route:n :}
   route idx ROUTE-AT!
   route ROUTE-BODY-CAP {: cap:n :}
   idx cap CHECK-BODY {: framing:parse-result :}
   framing COMPLETE? 0= if framing exit then
   idx deadline cap BODY-AWAIT
   OK ;


: DISPATCH ( n -- ) {: idx:n :}
   idx ROUTE-AT@ {: route:n :}
   route 0 >= if
      route idx BIND-ROUTE
      idx REQUEST-OF idx RESPONSE-OF route ROUTE-HANDLER @ execute
      exit
   then
   idx PATH-MATCHED? if idx NOT-ALLOWED exit then
   idx SERVE-STATIC if exit then
   idx NOT-FOUND ;


\ The offer ends with the handler. True when the handler took the connection.
: WITHDRAW ( n -- bool ) {: idx:n :}
   idx HOLDER @ TAKEN =
   KEPT idx HOLDER ! ;


\ A body read that did not complete throws E-BODY-READ out of its handler:
\ ANSWER rejects the request, so that throw is no fault.
: FAULT? ( n n -- bool ) {: idx:n code:n :}
   code 0= if false exit then
   code E-BODY-READ <> if true exit then
   idx BODY-REJECTED? 0= ;


\ Every fault answer is a 500, stored before the hook runs so that a hook that
\ throws before setting it still leaves the status the client is owed. True
\ when the handler took the connection, which is then owed no answer at all.
: HANDLE-REQUEST ( n -- bool ) {: idx:n :}
   OFFERED idx HOLDER !
   [: idx DISPATCH ;] catch {: code:n :}
   idx code FAULT? {: fault:bool :}
   fault if idx code REPORT-FAULT then
   idx WITHDRAW if true exit then
   fault 0= if false exit then
   500 idx SLOT-STATUS!
   [: idx FAULT-RESPONSE ;] catch {: failed:n :}
   idx failed HOOK-FAULT
   false ;


: CLOSE-CONNECTION ( TCP4:connection -- )
   TCP4:CLOSE DROP-STATUS ;


: DISCARD ( TCP4:connection SPAN:span<u8> -- bool ) {: conn:TCP4:connection room :}
   conn room SPAN:$ str> TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF drop true ENDOF
      closed OF drop false ENDOF
      failed OF drop false ENDOF
   ;MATCH ;


\ Half-closes, then drains until the peer stops or LINGER-MS passes, so a peer
\ still sending reads the answer instead of a reset. The caller closes after.
: LINGER ( TCP4:connection SPAN:span<u8> -- ) {: conn:TCP4:connection room :}
   conn TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   LINGER-MS DEADLINE-AT {: deadline:n :}
   begin
      mono-ns deadline > if exit then   \ a sending peer is always readable
      conn deadline WAIT-READABLE 0= if exit then
      conn room DISCARD 0= if exit then
   again ;


: CONNECTION-RESET ( n -- ) {: idx:n :}
   0 idx IN-U!
   0 idx IN-AT!
   0 idx HEAD-U! ;


\ A parsed method is never empty: an empty one was not given.
: FIELD+ ( str -- ) {: bytes:str :}
   bytes STR:LEN 0= if s" -" LOG:TEXT+ exit then
   bytes LOG:TEXT+ ;


\ The matched route's pattern, never the path: a path may carry a
\ credential.
: ROUTE-FIELD+ ( n -- ) {: idx:n :}
   idx ROUTE-AT@ {: route:n :}
   route 0 < if s" -" LOG:TEXT+ exit then
   route ROUTE-PATTERN$ LOG:TEXT+ ;


: ACCESS-LINE ( n -- ) {: idx:n :}
   LOG:INFO s" http" LOG:LINE
   s" req " LOG:TEXT+  idx SLOT-ID$ LOG:TEXT+
   s"  " LOG:TEXT+  idx SLOT-METHOD$ FIELD+
   s"  " LOG:TEXT+  idx ROUTE-FIELD+
   s"  " LOG:TEXT+ ;


: ;ACCESS-LINE ( n -- ) {: idx:n :}
   s"  in " LOG:TEXT+
   idx START-NS@ TIME:ELAPSED-MS LOG:INT+
   s"  ms" LOG:TEXT+
   LOG:;LINE ;


\ Writes the line owed to a request given an id and makes the slot unrequested;
\ SENT moves it to undelivered, then delivered, and TAKE-OVER to handed-off.
: ACCESS-LOG ( n -- ) {: idx:n :}
   LOGGING @ 0<> if
      idx ACCESS-STATE@
      MATCH access
         unrequested OF ENDOF
         unanswered OF
            idx ACCESS-LINE  s" unanswered" LOG:TEXT+  idx ;ACCESS-LINE
         ENDOF
         undelivered OF
            idx ACCESS-LINE
            idx SLOT-STATUS@ LOG:INT+  s"  unsent" LOG:TEXT+
            idx ;ACCESS-LINE
         ENDOF
         delivered OF
            idx ACCESS-LINE
            idx SLOT-STATUS@ LOG:INT+  s"  " LOG:TEXT+
            idx NO-BODY? if 0 else idx BODY-LEN@ then LOG:INT+  s"  B" LOG:TEXT+
            idx ;ACCESS-LINE
         ENDOF
         handed-off OF
            idx ACCESS-LINE  s" taken" LOG:TEXT+  idx ;ACCESS-LINE
         ENDOF
      ;MATCH
   then
   construct access unrequested idx ACCESS-STATE! ;


: SENT ( TCP4:connection n -- bool ) {: conn:TCP4:connection idx:n :}
   construct access undelivered idx ACCESS-STATE!
   conn idx RESPONSE-OF SEND {: sent:bool :}
   sent if construct access delivered idx ACCESS-STATE! then
   sent ;


: RENDER-REJECTION ( n n -- ) {: idx:n status:n :}
   status 400 = if idx RESPONSE-OF 400 s" bad_request"
      s" the request line or a header is not valid HTTP" ERROR! exit then
   status 413 = if idx RESPONSE-OF 413 s" body_too_large"
      s" the request body is larger than this server accepts" ERROR! exit then
   status 431 = if idx RESPONSE-OF 431 s" headers_too_large"
      s" the request header block is larger than this server accepts" ERROR! exit then
   idx RESPONSE-OF 501 s" not_implemented"
   s" this server does not implement that transfer coding" ERROR! ;


\ Answers a rejected request with its status and closes after; a status of 0
\ (the peer left, the deadline passed, the connection failed) is no answer.
\ Whatever a handler built is dropped first.
: REJECT ( TCP4:connection n n -- served ) {: conn:TCP4:connection idx:n status:n :}
   status 0= if construct served silent exit then
   idx DROP-ANSWER
   0 idx KEEP-ALIVE!
   status idx SLOT-STATUS!
   [: idx status RENDER-REJECTION ;] catch {: failed:n :}
   idx failed HOOK-FAULT
   conn idx SENT 0= if construct served silent exit then
   construct served answered ;


\ A connection its handler took is the taker's, which closes it. A body read
\ that did not complete is answered with that read's rejection (REJECT), whatever
\ the handler set; a body the handler did not read in full leaves unread bytes
\ on the connection, which is closed after the answer.
: ANSWER ( TCP4:connection n -- served ) {: conn:TCP4:connection idx:n :}
   idx MAKE-ID
   idx HANDLE-REQUEST if
      construct served taken exit
   then
   idx BODY-REJECTED? if conn idx idx READ-RESULT @ REJECTION-STATUS REJECT exit then
   idx BODY-UNREAD? if 0 idx KEEP-ALIVE! then
   conn idx SENT 0= if construct served silent exit then
   idx KEEP-ALIVE@ 0 <> if construct served keep-open exit then
   construct served answered ;


\ SLOT-RESET's mappings can fail; the failure stays at this request, not the
\ worker's task. One while a handler reads its body is that handler's fault.
: READ-CURRENT ( -- )
   SELF-SLOT {: idx:n :}
   idx SERVING @ idx IDLE-MS @ READ-REQUEST idx READ-RESULT ! ;


\ A slot whose answer buffers could not be made again has none to answer in:
\ the fault is reported and the connection closed.
: READ-FAULT ( TCP4:connection n n -- served )
   {: conn:TCP4:connection idx:n code:n :}
   idx MAKE-ID
   0 idx KEEP-ALIVE!
   idx code REPORT-FAULT
   idx ANSWER-MADE? 0= if construct served silent exit then
   500 idx SLOT-STATUS!
   [: idx FAULT-RESPONSE ;] catch {: failed:n :}
   idx failed HOOK-FAULT
   conn idx SENT if construct served answered exit then
   construct served silent ;


: SERVE-ONE-RUN ( TCP4:connection n -- served ) {: conn:TCP4:connection idx:n :}
   [: READ-CURRENT ;] catch {: code:n :}
   code 0 <> if conn idx code READ-FAULT exit then
   idx READ-RESULT @ {: result:parse-result :}
   result COMPLETE? if conn idx ANSWER exit then
   result REJECTION-STATUS {: status:n :}
   status 0= if construct served silent exit then
   idx MAKE-ID
   conn idx status REJECT ;


\ n is the worker's own slot, whose access line is written and which is
\ finished on every way out, a throw or a halt included.
: SERVE-ONE ( TCP4:connection n -- served )
   [: SERVE-ONE-RUN ;] [: SELF-SLOT ACCESS-LOG  SELF-SLOT SLOT-FINISH ;] finally ;


: PARK-UNLOCK ( -- )
   PARK-LOCK TASK:RELEASE ;


: VACANT? ( n -- bool )
   PARK-PHASE @
   MATCH parking
      vacant OF true ENDOF
      handed OF false ENDOF
      polled OF false ENDOF
   ;MATCH ;


: VACATE ( n -- ) {: at:n :}
   construct parking vacant at PARK-PHASE ! ;


\ Under PARK-LOCK: the first vacant row, or -1 when every row is taken.
: FREE-ROW ( -- n )
   PARK-SLOTS 0 ?do i VACANT? if i unloop exit then loop
   -1 ;


\ Under PARK-LOCK: true once the connection is a handed row and no longer the
\ slot's; false while the listener takes no rows or every row is taken.
: PARK-PUT ( TCP4:connection n -- bool ) {: conn:TCP4:connection idx:n :}
   PARK-OPEN @ 0= if false exit then
   FREE-ROW {: at:n :}
   at 0 < if false exit then
   conn at PARK-CONN !
   IDLE-MS @ DEADLINE-AT at PARK-DUE !
   construct parking handed at PARK-PHASE !
   0 idx SERVING-LIVE !
   WAKER AIO:WAKE
   true ;


\ The slot gives the connection up under the lock it is handed over in, and
\ nothing there pauses, so no halt falls between: a worker's exit never closes
\ a connection the listener holds. One that finds no row is closed here through
\ the lingering close, so a next request already sent draws no reset over the
\ answer the peer is still reading.
: PARK-CONNECTION ( TCP4:connection n -- ) {: conn:TCP4:connection idx:n :}
   PARK-LOCK TASK:GET
   conn idx [: PARK-PUT ;] [: PARK-UNLOCK ;] finally
   if exit then
   conn idx IN-BUF LINGER conn CLOSE-CONNECTION ;


\ One connection until a request says otherwise. One kept alive with nothing of
\ its next request in the slot's input is parked (PARK-CONNECTION) and the
\ worker goes to its next job: no worker waits for a connection's next request.
\ A peer that has been answered and not kept leaves through the lingering
\ close, one that is gone or silent through a plain one.
: SERVE-UNTIL-CLOSED ( TCP4:connection n -- ) {: conn:TCP4:connection idx:n :}
   begin
      STOPPING? if conn CLOSE-CONNECTION exit then
      conn idx SERVE-ONE
      MATCH served
         keep-open OF
            idx AVAIL 0= if conn idx PARK-CONNECTION exit then
         ENDOF
         answered OF conn idx IN-BUF LINGER conn CLOSE-CONNECTION exit ENDOF
         silent OF conn CLOSE-CONNECTION exit ENDOF
         taken OF exit ENDOF
      ;MATCH
   again ;


\ The connection is the slot's SERVING row until its close has run or it is
\ parked. Nothing between that close and the clear pauses, so no halt falls
\ between them: the exit of a worker that closed its own connection closes
\ nothing, and the exit of one killed inside a wait before that close closes
\ it.
: SERVE-CONNECTION ( TCP4:connection -- ) {: conn:TCP4:connection :}
   SELF-SLOT {: idx:n :}
   conn idx SERVING !
   1 idx SERVING-LIVE !
   idx CONNECTION-RESET
   conn idx SERVE-UNTIL-CLOSED
   0 idx SERVING-LIVE ! ;


\ The quotations live in rows declared to hold quotations, which is the
\ checker's proven-quotation store (src/lib/task.f); an xt in a cell
\ would lose the effect this table is named by.
MAX-HOOKS TYPED-BUFFER START-HOOK [ -- ]
MAX-HOOKS TYPED-BUFFER EXIT-HOOK [ -- ]
variable START-HOOK-N
variable EXIT-HOOK-N


: HOOK-ROOM ( n -- n ) {: at:n :}
   at MAX-HOOKS >= if E-CAPACITY throw then
   at ;


\ A hook added to a live pool would be missed by every worker already started,
\ so the tables are a process's own setup, taken before START and kept for the
\ servers that follow.
: HOOKS-OPEN ( -- )
   RUNNING @ 0 <> if E-STATE throw then ;


: RUN-START-HOOKS ( -- )
   START-HOOK-N @ 0 ?do i START-HOOK @ execute loop ;


\ In reverse, so a hook registered over an earlier one's resource gives its own
\ back first. Each runs under a catch of its own, so a hook that throws keeps
\ no other from giving its resource back; the first throw is the answer, zero
\ when none threw.
: RUN-EXIT-HOOK-ROWS ( -- n )
   0
   EXIT-HOOK-N @ 0 ?do
      EXIT-HOOK-N @ 1- i - EXIT-HOOK @ catch
      over 0= if nip else drop then
   loop ;


\ Closes the connection the slot still serves: a stop killed the worker inside
\ a wait, or a throw left SERVE-CONNECTION, before its close.
: DROP-SERVING ( -- )
   SELF-SLOT {: idx:n :}
   idx SERVING-LIVE @ 0= if exit then
   0 idx SERVING-LIVE !
   idx SERVING @ CLOSE-CONNECTION ;


\ The connection is closed once every hook has run, so a hook that retires what
\ it kept against that connection has done so before the descriptor can be
\ handed to another connection, whatever another hook threw. The first throw
\ then goes on to the task.
: RUN-EXIT-HOOKS ( -- )
   SELF-SLOT SLOT-FINISH
   RUN-EXIT-HOOK-ROWS {: code:n :}
   DROP-SERVING
   code 0 <> if code throw then ;


: JOB-UNLOCK ( -- )
   JOB-LOCK TASK:RELEASE ;


: MOVE-JOB ( n n -- ) {: from:n to:n :}
   from JOB @ to JOB !
   from JOB-ARG @ to JOB-ARG !
   from JOB-RUN @ to JOB-RUN ! ;


\ A mapping rejected part way throws with JOB-CAP unchanged.
: JOB-GROW ( -- )
   JOB-CAP @ 2 * JOB-SLOTS max {: cap:n :}
   cap JOB-RESERVE
   cap JOB-ARG-RESERVE
   cap JOB-RUN-RESERVE
   cap JOB-CAP ! ;


\ Under JOB-LOCK: a place behind the last job. The jobs are first moved down
\ over the places taken jobs left, and the columns doubled when they still
\ fill them.
: JOB-ROOM ( -- )
   JOB-AT @ JOB-N @ + JOB-CAP @ < if exit then
   JOB-N @ 0 ?do JOB-AT @ i + i MOVE-JOB loop
   0 JOB-AT !
   JOB-N @ JOB-CAP @ < if exit then
   JOB-GROW ;


\ Under JOB-LOCK: the job queued last, with that argument, and counted in
\ ITEMS; its place, where a call's quotation goes.
: JOB-PUT ( n job -- n ) {: arg:n kind :}
   JOB-ROOM
   JOB-AT @ JOB-N @ + {: at:n :}
   kind at JOB !
   arg at JOB-ARG !
   1 JOB-N +!
   ITEMS TASK:SIGNAL-SEM
   at ;


\ Under JOB-LOCK: the oldest job, its argument and quotation copied to this
\ worker's row, or none when the count the worker took was a connection a stop
\ dropped.
: NEXT-JOB ( -- job )
   JOB-N @ 0= if construct job none exit then
   JOB-AT @ {: at:n :}
   SELF-SLOT {: w:n :}
   at JOB-ARG @ w TAKEN-ARG !
   at JOB-RUN @ w TAKEN-RUN !
   1 JOB-AT +!
   -1 JOB-N +!
   at JOB @ ;


: TAKE-JOB ( -- job )
   ITEMS TASK:WAIT-SEM
   JOB-LOCK TASK:GET
   [: NEXT-JOB ;] [: JOB-UNLOCK ;] finally ;


\ The connection leaves the queue, so its place goes back to ROOM. A throw
\ ends only the connection, so the worker goes on to its next job.
: SERVE-TAKEN ( -- )
   ROOM TASK:SIGNAL-SEM
   [: SELF-SLOT TAKEN-ARG @ TCP4:>CONNECTION SERVE-CONNECTION ;] catch {: code:n :}
   code 0= if exit then
   LOG:ERROR s" http" LOG:LINE
   s" connection failed, throw " LOG:TEXT+  code LOG:CODE+
   LOG:;LINE
   DROP-SERVING ;


: REPORT-CALL ( n -- ) {: code:n :}
   LOG:ERROR s" http" LOG:LINE
   s" call failed, throw " LOG:TEXT+  code LOG:CODE+
   LOG:;LINE ;


: RUN-CALL ( -- )
   SELF-SLOT {: w:n :}
   [: w TAKEN-ARG @ w TAKEN-RUN @ execute ;] catch {: code:n :}
   code 0 <> if code REPORT-CALL then ;


: WORKER-LOOP ( -- )
   begin
      TAKE-JOB
      MATCH job
         connection OF SERVE-TAKEN ENDOF
         call OF RUN-CALL ENDOF
         stop OF exit ENDOF
         none OF ENDOF
      ;MATCH
   again ;


: SERVE-JOBS ( -- )
   [: RUN-START-HOOKS ;] catch {: code:n :}
   code 0 <> if 0 code START-ERROR atomic-cas drop then
   START-READY TASK:SIGNAL-SEM
   code 0 <> if code throw then
   WORKER-LOOP
   1 ENDED-COUNT atomic-add drop ;


\ A worker takes the next free slot itself: the fetch-and-add hands each one a
\ different number, so no worker waits for the starter and no two share a slot.
\ Its resources are opened after that, because a hook names them by the slot,
\ and a hook that throws reports startup failure before ending this worker.
\
\ However this worker leaves - the stop job, or a throw from a start hook or
\ from outside a connection's or a call's own work - it counts itself out before
\ the throw goes on ending it, so a stop waits for the workers that are still
\ serving and for no others.
: WORKER-BODY ( -- )
   1 CLAIMED atomic-add SELF-SLOT!
   [: SERVE-JOBS ;] catch {: code:n :}
   LEAVING
   code 0 <> if code throw then ;


\ The cleanup is registered on the task before the task runs, so it is already
\ there when a start hook throws, and it runs in the worker's own thread, which
\ is the only thread that may give a worker's resources back.
: START-WORKER ( n -- ) {: at:n :}
   [: RUN-EXIT-HOOKS ;] at WORKER-TCB @ TASK:AT-EXIT
   [: WORKER-BODY ;] at WORKER-TCB @ TASK:ACTIVATE ;


: START-NEXT ( -- )
   STARTED @ START-WORKER ;


\ Count only activated TCBs. A failed activation leaves its attempted TCB to
\ KILL, while the previously activated workers are rolled back by START.
: START-WORKERS ( -- n )
   WORKER-COUNT @ 0 ?do
      [: START-NEXT ;] catch {: code:n :}
      code 0 <> if
         i WORKER-TCB @ TASK:KILL
         code unloop exit
      then
      1 STARTED +!
   loop
   0 ;


: WAIT-START ( -- n )
   STARTED @ 0 ?do START-READY TASK:WAIT-SEM loop
   START-ERROR atomic@ ;


\ Under JOB-LOCK, once ROOM has given the connection a place: queued, or
\ closed when a stop has begun, its place given back.
: QUEUE-CONNECTION ( TCP4:connection -- ) {: conn:TCP4:connection :}
   STOPPING? if ROOM TASK:SIGNAL-SEM conn CLOSE-CONNECTION exit then
   conn TCP4:CONNECTION>N construct job connection JOB-PUT drop ;


\ A connection for a worker, accepted or parked and now readable, queued once
\ ROOM has given it a place.
: ENQUEUE ( TCP4:connection -- ) {: conn:TCP4:connection :}
   STOPPING? if conn CLOSE-CONNECTION exit then
   ROOM TASK:WAIT-SEM
   JOB-LOCK TASK:GET
   conn [: QUEUE-CONNECTION ;] [: JOB-UNLOCK ;] finally ;


: ACCEPTED ( TCP4:connection TCP4:address TCP4:port -- )
   {: conn:TCP4:connection peer:TCP4:address peer-port:TCP4:port :}
   1 ACCEPTED-COUNT atomic-add drop
   \ With Nagle on, a reply's small later write waits for the peer's delayed ACK.
   conn true TCP4:NODELAY!
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop conn CLOSE-CONNECTION exit ENDOF
   ;MATCH
   conn ENQUEUE ;


\ A warning, not an error: the listener pauses and retries.
: REPORT-STARVED ( TCP4:errno -- )
   {: cause:TCP4:errno :}
   LOG:WARN s" http" LOG:LINE
   s" accept failed, errno " LOG:TEXT+  cause TCP4:ERRNO>N LOG:INT+
   LOG:;LINE ;


\ The record awaited and released, whatever its outcome.
: SETTLE ( AIO:ticket -- )
   AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      rejected OF drop ENDOF
   ;MATCH ;


\ A sleep, not an AIO timer: the pause also waits out an AIO that rejects
\ records.
: ACCEPT-PAUSE ( -- )
   STOP-READ-MS >MS TASK:SLEEP ;


\ A failed accept stops the server only when the listener itself is broken. A
\ pending connection that failed before accept took it (`aborted`) costs nothing:
\ the listener polls again at once. Running out of descriptors or memory
\ (`starved`) leaves the connection queued on Linux and drops it on macOS; the
\ listener says so on stderr and pauses, so it does not spin, and accepts again
\ once something has been freed.
: ACCEPT-FAILED ( TCP4:errno -- ) {: cause:TCP4:errno :}
   cause TCP4:>ACCEPT-FAULT
   MATCH TCP4:accept-fault
      aborted OF ENDOF
      starved OF cause REPORT-STARVED ACCEPT-PAUSE ENDOF
      broken OF 1 STOPPING atomic-add drop ENDOF
   ;MATCH ;


: ACCEPT-ONE ( -- )
   LISTENER@ TCP4:ACCEPT
   MATCH TCP4:accept-result
      accepted OF ACCEPTED ENDOF
      failed OF ACCEPT-FAILED ENDOF
   ;MATCH ;


\ The poll stands until a connection is pending: a stop reaches the listener
\ through its wake poll.
: ARM-ACCEPT ( -- )
   LISTENER@ TCP4:LISTENER-FD AIO:READABLE -1 >MS AIO:POLL
   {: t:AIO:ticket :}
   t ACCEPT-TAG LISTENING AIO:GROUP+
   t ACCEPT-TICKET !
   1 ACCEPT-ARMED ! ;


: ARM-WAKE ( -- )
   WAKER AIO:WAKER-POLL {: t:AIO:ticket :}
   t WAKE-TAG LISTENING AIO:GROUP+
   t WAKE-TICKET !
   1 WAKE-ARMED ! ;


\ One arm of the listener's own polls. A poll rejected at AIO's ceiling waits
\ for records to be freed, so it is a warning and a pause.
: ARM-OR-PAUSE ( [ -- ] -- ) {: arm :}
   arm catch {: code:n :}
   code 0= if exit then
   code E-AIO-FULL <> code E-AIO-POLL-BOUND <> and if code throw then
   LOG:WARN s" http" LOG:LINE
   s" poll rejected, throw " LOG:TEXT+  code LOG:CODE+
   LOG:;LINE
   ACCEPT-PAUSE ;


: HANDED? ( n -- bool )
   PARK-PHASE @
   MATCH parking
      vacant OF false ENDOF
      handed OF true ENDOF
      polled OF false ENDOF
   ;MATCH ;


\ The row's poll, its ticket kept in the row.
: ROW-POLL ( n -- n ) {: at:n :}
   at PARK-CONN @ TCP4:CONNECTION-FD AIO:READABLE
   at PARK-DUE @ REMAINING AIO:POLL at PARK-TICKET !
   at ;


\ Under PARK-LOCK: the row polled until it is readable or its deadline passes.
\ A poll AIO rejects (E-AIO-FULL at Linux's record ceiling, E-AIO-POLL-BOUND on
\ Darwin) closes the row's connection and vacates the row, as ROW-HEARD does for
\ a rejected poll, and the listener goes on.
: ARM-ROW ( n -- ) {: at:n :}
   at [: ROW-POLL ;] catch {: code:n :} drop
   code 0 <> if at PARK-CONN @ CLOSE-CONNECTION at VACATE exit then
   at PARK-TICKET @ at LISTENING AIO:GROUP+
   construct parking polled at PARK-PHASE ! ;


\ Under PARK-LOCK, after AIO:WOKE: every row handed so far is polled, or closed
\ when its poll is rejected.
: ARM-HANDED ( -- )
   PARK-SLOTS 0 ?do i HANDED? if i ARM-ROW then loop ;


: READY? ( AIO:outcome -- bool )
   MATCH AIO:outcome
      ready OF drop true ENDOF
      timed-out OF false ENDOF
      cancelled OF false ENDOF
      rejected OF drop false ENDOF
   ;MATCH ;


\ Any end of the wake poll but ready would leave the listener deaf to the rows
\ handed to it, so it stops as a broken listener does.
: WOKEN ( AIO:outcome -- )
   READY? 0= if 1 STOPPING atomic-add drop exit then
   WAKER AIO:WOKE
   PARK-LOCK TASK:GET
   [: ARM-HANDED ;] [: PARK-UNLOCK ;] finally ;


\ The row vacated, its connection read out first: a worker may take the row
\ at once.
: UNPARK ( n -- TCP4:connection ) {: at:n :}
   at PARK-CONN @ {: conn:TCP4:connection :}
   PARK-LOCK TASK:GET
   at [: VACATE ;] [: PARK-UNLOCK ;] finally
   conn ;


\ Readable, the connection goes to a worker; past its deadline, or with its
\ poll rejected, it is closed plainly.
: ROW-HEARD ( n AIO:outcome -- ) {: at:n out :}
   at UNPARK {: conn:TCP4:connection :}
   out READY? if conn ENQUEUE exit then
   conn CLOSE-CONNECTION ;


\ A pending connection is accepted, and a poll that failed stops the server,
\ as a broken listener does. The poll has no deadline, and only UNWAIT
\ cancels, which awaits its own.
: ACCEPT-HEARD ( AIO:outcome -- )
   MATCH AIO:outcome
      ready OF drop ACCEPT-ONE ENDOF
      timed-out OF ENDOF
      cancelled OF 1 STOPPING atomic-add drop ENDOF
      rejected OF drop 1 STOPPING atomic-add drop ENDOF
   ;MATCH ;


\ The wait AWAIT-ANY answered has left the group, so its flag is cleared first.
: HEARD ( n AIO:outcome -- ) {: tag:n out :}
   tag ACCEPT-TAG = if 0 ACCEPT-ARMED ! out ACCEPT-HEARD exit then
   tag WAKE-TAG = if 0 WAKE-ARMED ! out WOKEN exit then
   tag out ROW-HEARD ;


\ The listener waits only with its accept and wake polls both armed, so the
\ wake STOP sends always ends the wait.
: LISTEN-LOOP ( -- )
   begin
      STOPPING? if exit then
      ACCEPT-ARMED @ 0= if ['] ARM-ACCEPT ARM-OR-PAUSE else
      WAKE-ARMED @ 0= if ['] ARM-WAKE ARM-OR-PAUSE else
      LISTENING AIO:AWAIT-ANY HEARD then then
   again ;


: ACCEPT-UNTIL-STOPPED ( -- )
   LISTEN-LOOP
   1 ENDED-COUNT atomic-add drop ;


\ A wait the listener no longer wants leaves the group and is awaited, so a
\ descriptor is closed only once no poll is on it.
: UNWAIT ( AIO:ticket -- ) {: t:AIO:ticket :}
   t LISTENING AIO:GROUP-
   t AIO:CANCEL
   t SETTLE ;


: CLOSE-ROW ( n -- ) {: at:n :}
   at PARK-PHASE @
   MATCH parking
      vacant OF exit ENDOF
      handed OF ENDOF
      polled OF at PARK-TICKET @ UNWAIT ENDOF
   ;MATCH
   at PARK-CONN @ CLOSE-CONNECTION
   at VACATE ;


\ The listener's last act, however its loop ended short of a halt (STOP's
\ DRAIN-PARKED covers that): it takes no more rows, its waits are awaited and
\ every parked connection is closed.
: UNLISTEN ( -- )
   PARK-LOCK TASK:GET
   0 PARK-OPEN !
   PARK-UNLOCK
   ACCEPT-ARMED @ 0 <> if 0 ACCEPT-ARMED ! ACCEPT-TICKET @ UNWAIT then
   WAKE-ARMED @ 0 <> if 0 WAKE-ARMED ! WAKE-TICKET @ UNWAIT then
   PARK-SLOTS 0 ?do i CLOSE-ROW loop ;


\ The loop's throw is the answer, else UNLISTEN's.
: LISTENER-BODY ( -- )
   [: ACCEPT-UNTIL-STOPPED ;] catch {: code:n :}
   [: UNLISTEN ;] catch {: closing:n :}
   LEAVING
   code 0 <> if code throw then
   closing 0 <> if closing throw then ;


: START-LISTENER ( -- )
   [: LISTENER-BODY ;] LISTENER-TASK TASK:ACTIVATE ;


\ The address's octet at that shift, in decimal.
: OCTET+ ( n n -- )
   rshift $FF and LOG:INT+ ;


\ The step of BIND-LISTENER the OS rejected, logged as
\ `<step> <a.b.c.d>:<port> failed, errno <n>` before E-SOCKET.
: REPORT-UNBOUND ( TCP4:errno n n str -- )
   {: cause:TCP4:errno address:n port:n step:str :}
   LOG:ERROR s" http" LOG:LINE
   step LOG:TEXT+  s"  " LOG:TEXT+
   address 24 OCTET+  s" ." LOG:TEXT+
   address 16 OCTET+  s" ." LOG:TEXT+
   address 8 OCTET+  s" ." LOG:TEXT+
   address 0 OCTET+
   s" :" LOG:TEXT+  port LOG:INT+
   s"  failed, errno " LOG:TEXT+  cause TCP4:ERRNO>N LOG:INT+
   LOG:;LINE
   E-SOCKET throw ;


: DROP-LISTENER ( -- )
   LISTENER@ TCP4:CLOSE-LISTENER DROP-STATUS
   0 LISTENER-CELL ! ;


: BIND-LISTENER ( n n -- ) {: address:n port:n :}
   address TCP4:ADDRESS port TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF TCP4:LISTENER>N LISTENER-CELL ! ENDOF
      failed OF address port s" bind to" REPORT-UNBOUND ENDOF
   ;MATCH
   LISTENER@ BACKLOG TCP4:LISTEN
   MATCH TCP4:status
      ok OF ENDOF
      failed OF DROP-LISTENER address port s" listen on" REPORT-UNBOUND ENDOF
   ;MATCH
   LISTENER@ TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N BOUND-PORT ! TCP4:ADDRESS>N BOUND-ADDRESS ! ENDOF
      failed OF
         DROP-LISTENER
         address port s" reading the bound address of" REPORT-UNBOUND
      ENDOF
   ;MATCH ;


\ The listener and the workers: the tasks a stop waits for.
: TASK-COUNT ( -- n )
   WORKER-COUNT @ 1+ ;


\ The longest bound whose nanoseconds a cell holds.
MEM-MAX-N NS-PER-MS / constant MAX-DEADLINE-MS

: BOUND+ ( n n -- n ) {: left:n right:n :}
   right MAX-DEADLINE-MS left - > if MAX-DEADLINE-MS exit then
   left right + ;

\ The longest a task can stay parked once the flag rises - a worker's head, body
\ and linger parks in turn - plus STOP-MARGIN-MS. It counts no handler work.
: STOP-BOUND-MS ( -- n )
   IDLE-MS @ BODY-TIMEOUT-MS @ BOUND+ LINGER-MS BOUND+
   STOP-READ-MS max
   STOP-MARGIN-MS BOUND+ ;


\ True once the listener and every worker have left their body, which is when
\ nothing of this server is serving any more. The tasks count themselves out
\ rather than the stop reading TASK:DONE?: a task's DONE state is stored after
\ its cleanup has run, so it arrives after the hint the task posted on its way
\ out, and a stop waiting for DONE? took its whole bound on a server with nothing
\ to do (measured: over 800 ms of the 900 ms http-test bound). A task still in
\ its cleanup has left its body and is one TASK:KILL joins rather than kills.
: TASKS-LEFT? ( -- bool )
   LEFT-COUNT @ TASK-COUNT = ;


\ The bound's timer is cancelled and then awaited: a cancel does not wait, and a
\ record nobody awaits is one the ring still holds (src/lib/aio.f), which
\ would reject the LOOP-STOP that follows this server.
: DROP-TIMER ( AIO:ticket -- ) {: timer:AIO:ticket :}
   timer AIO:CANCEL
   timer SETTLE ;


\ Waits for the tasks to leave their own bodies, to the bound. The only wake-ups
\ are each task's own LEAVING and one AIO:TIMEOUT for the bound - the loop wakes
\ the record's owner when the timer completes (src/lib/aio.f) - so nothing
\ here polls a clock, and the stopper re-reads the count after every hint, as the
\ hint protocol requires (src/lib/task.f).
: AWAIT-TASKS ( -- )
   STOP-BOUND-MS DEADLINE-AT {: deadline:n :}
   STOP-BOUND-MS >MS AIO:TIMEOUT {: timer:AIO:ticket :}
   begin
      TASKS-LEFT? mono-ns deadline >= or if timer DROP-TIMER exit then
      TASK:WAIT
   again ;


\ The backstop past the bound. A task still inside its body now is one the stop
\ had to kill, with the rest of that body unrun, and KILLED-TASKS says how many;
\ every task is killed either way, which is how its memory is given back and how
\ a task that has left its body is joined.
: KILL-TASKS ( -- )
   TASK-COUNT LEFT-COUNT @ - KILLED-COUNT !
   LISTENER-TASK TASK:KILL
   WORKER-COUNT @ 0 ?do
      i WORKER-TCB @ TASK:KILL
   loop ;


: PIPE-OPEN ( -- )
   WAKER AIO:WAKER-OPEN
   1 PIPED ! ;


: PIPE-CLOSE ( -- )
   PIPED @ 0= if exit then
   0 PIPED !
   WAKER AIO:WAKER-CLOSE ;


\ The rows vacant and taken from once the waker is open.
: OPEN-PARKING ( -- )
   PARK-SLOTS 0 ?do i VACATE loop
   0 ACCEPT-ARMED !
   0 WAKE-ARMED !
   PIPE-OPEN
   1 PARK-OPEN ! ;


\ Everything a running server holds beyond its listener.
: OPEN-RESOURCES ( n -- ) {: workers:n :}
   workers ARENA-OPEN
   0 JOB-AT !
   0 JOB-N !
   0 ITEMS TASK:SEMAPHORE-INIT
   JOB-SLOTS ROOM TASK:SEMAPHORE-INIT
   0 START-READY TASK:SEMAPHORE-INIT
   OPEN-PARKING ;


: RELEASE-RESOURCES ( -- )
   DROP-LISTENER
   PIPE-CLOSE
   ITEMS TASK:SEMAPHORE-DESTROY
   ROOM TASK:SEMAPHORE-DESTROY
   START-READY TASK:SEMAPHORE-DESTROY
   ARENA-CLOSE
   STATIC-CLOSE
   0 RUNNING ! ;


\ JOIN waits through each task's AT-EXIT hook. Its error result is either the
\ original startup throw or E-TASK-NO-RESULT from a worker stopped normally;
\ START retains the published hook error while consuming each outcome.
: JOIN-STARTED ( -- )
   STARTED @ 0 ?do
      i WORKER-TCB @ TASK:JOIN
      MATCH result
         ok OF drop ENDOF
         err OF drop ENDOF
      ;MATCH
   loop ;


\ Under JOB-LOCK: n stop jobs, behind every job queued before them.
: PUSH-STOPS ( n -- )
   0 ?do 0 construct job stop JOB-PUT drop loop ;


: ROLLBACK-START ( n -- ) {: code:n :}
   JOB-LOCK TASK:GET
   STARTED @ [: PUSH-STOPS ;] [: JOB-UNLOCK ;] finally
   JOIN-STARTED
   RELEASE-RESOURCES
   code throw ;


\ Under JOB-LOCK.
: QUEUE-CALL ( n [ n -- ] -- ) {: arg:n q :}
   RUNNING @ 0= STOPPING? or if E-STATE throw then
   q arg construct job call JOB-PUT JOB-RUN ! ;


: CONNECTION? ( job -- bool )
   MATCH job
      connection OF true ENDOF
      call OF false ENDOF
      stop OF false ENDOF
      none OF false ENDOF
   ;MATCH ;


\ The job at that place, a connection closed unserved and any other dropped.
: DROP-JOB ( n -- ) {: at:n :}
   at JOB @ CONNECTION? 0= if exit then
   at JOB-ARG @ TCP4:>CONNECTION CLOSE-CONNECTION ;


\ Under JOB-LOCK: the job at that place dropped when it is a connection, its
\ place given back to ROOM, else moved down behind the jobs kept before it.
\ The count kept so far goes in and out.
: SIFT ( n n -- n ) {: kept:n at:n :}
   at JOB @ CONNECTION? if at DROP-JOB ROOM TASK:SIGNAL-SEM kept exit then
   at JOB-AT @ kept + MOVE-JOB
   kept 1+ ;


\ Under JOB-LOCK: the flag raised and the listener woken to read it, every
\ connection queued closed unserved, and a stop job for each worker queued
\ behind the calls, which keep their order. The counts of the connections
\ dropped stay in ITEMS (NEXT-JOB).
: STOP-QUEUE ( -- )
   1 STOPPING atomic-add drop
   WAKER AIO:WAKE
   0
   JOB-AT @ JOB-N @ + JOB-AT @ ?do i SIFT loop
   JOB-N !
   WORKER-COUNT @ PUSH-STOPS ;


\ Every job left once no task takes one: a connection no worker took is closed
\ unserved, and a call or a stop job is dropped.
: DRAIN-JOBS ( -- )
   JOB-AT @ JOB-N @ + JOB-AT @ ?do i DROP-JOB loop
   0 JOB-AT !
   0 JOB-N ! ;


\ Every row left once no task is: those of a listener the stop killed, whose
\ polls AIO gave back as that task ended.
: DRAIN-PARKED ( -- )
   0 PARK-OPEN !
   PARK-SLOTS 0 ?do
      i VACANT? 0= if i PARK-CONN @ CLOSE-CONNECTION i VACATE then
   loop ;


: LINGER-ROOM ( -- SPAN:span<u8> )
   0 LINGER-SCRATCH LINGER-CAP SPAN:MAKE ;


: REPLY-HEAD ( n str n -- ) {: status:n type:str body-n:n :}
   SB-RESET
   s" HTTP/1.1 " SB-APPEND
   status FMT:SB-U
   SP SB-APPEND-C
   status REASON$ SB-APPEND
   s\" \r\nContent-Type: " SB-APPEND
   type SB-APPEND
   s\" \r\n" SB-APPEND
   status BODYLESS? 0= if
      s" Content-Length: " SB-APPEND
      body-n FMT:SB-U
      s\" \r\n" SB-APPEND
   then
   s\" Connection: close\r\n\r\n" SB-APPEND ;


: CLOSE-REPLYING ( -- )
   REPLYING @ TCP4:>CONNECTION CLOSE-CONNECTION ;


: SEND-REPLY ( n str str -- bool )
   {: status:n type:str body:str :}
   status type body STR:LEN REPLY-HEAD
   REPLYING @ TCP4:>CONNECTION {: conn:TCP4:connection :}
   conn SB$ WRITE-SPAN 0= if false exit then
   status BODYLESS? 0= if
      conn body WRITE-SPAN 0= if false exit then
   then
   conn LINGER-ROOM LINGER
   true ;


public

\ From the next START on, every request the server reads writes one LOG INFO
\ line, tag http: `req <id> <method> <route> <status> <n> B in <ms> ms`, id the
\ REQUEST-ID$, route the pattern of the route the request matched, n the body
\ bytes sent and ms the whole milliseconds from the end of the head's read to
\ the line. A method the request did not give, and the route of a request no
\ route matched, is `-`. In place of `<status> <n> B`, a send that
\ failed writes `<status> unsent`, a connection its handler took `taken`, and
\ a request given no answer `unanswered`. No header value is written. It
\ lasts across STOP; E-STATE while running.
: LOG ( -- )
   RUNNING @ 0 <> if E-STATE throw then
   1 LOGGING ! ;

\ Configure the incoming body bound before START. Zero accepts only empty
\ bodies; the setting persists across STOP and a failed START.
: BODY-CAP! ( n -- ) {: limit:n :}
   RUNNING @ 0 <> if E-STATE throw then
   limit 0 < if E-CAPACITY throw then
   limit BODY-LIMIT ! ;

\ Zero shares START's absolute deadline across head and body. A positive value
\ grants the body a fresh absolute deadline when its read begins, at the
\ handler's first BODY$. Progress renews neither. Past each
\ deadline the worker reads only the bytes the socket holds when its first read
\ finds that deadline passed, however late that read comes, and nothing once a
\ stop is raised. A request those bytes leave incomplete gets no answer: its
\ connection is closed plainly, so a peer still sending reads a reset.
: BODY-TIMEOUT! ( n -- ) {: ms:n :}
   RUNNING @ 0 <> if E-STATE throw then
   ms 0 < ms MAX-BODY-TIMEOUT-MS > or if E-CAPACITY throw then
   ms BODY-TIMEOUT-MS ! ;

: BODY-TIMEOUT ( -- n )
   BODY-TIMEOUT-MS @ ;

\ Opens what a request worker needs of its own (a database connection, say),
\ inside that worker, once it has claimed its slot and before its first request.
\ START waits for every start hook; a throw fails START once every started
\ worker has run its exit hooks and been joined: the listener is closed,
\ RUNNING? is false and the same port can start again. Hooks stay registered
\ for every later server.
: ON-WORKER-START ( [ -- ] -- ) {: q :}
   HOOKS-OPEN
   START-HOOK-N @ HOOK-ROOM {: at:n :}
   q at START-HOOK !
   at 1+ START-HOOK-N ! ;


\ Gives it back, in the worker's own task through TASK:AT-EXIT, however the
\ worker ended.
: ON-WORKER-EXIT ( [ -- ] -- ) {: q :}
   HOOKS-OPEN
   EXIT-HOOK-N @ HOOK-ROOM {: at:n :}
   q at EXIT-HOOK !
   at 1+ EXIT-HOOK-N ! ;


\ How every error answer is rendered: the rejections the server makes itself
\ (400, 404, 405, 413, 431, 500, 501) and every ERROR! a handler calls, given
\ the response, the status, a short code and a message. The hook sets the
\ status and the body through the public response words; REQUEST-ID$ is the id
\ the fault was reported under. It replaces the plain-text default, one line
\ of `code: message (request id)`. For a rejection or a fault the hook runs
\ outside the handler's boundary under the worker's own catch: a hook that
\ throws there is reported on stderr under the request id, and the client gets
\ the plain-text default with code error_hook and the status it was owed, so
\ the worker answers and takes the next connection.
: ON-ERROR ( [ response n str str -- ] -- ) {: q :}
   HOOKS-OPEN
   q 0 ERROR-HOOK ! ;


\ The Cache-Control value a static file is sent with, given its path. It
\ replaces the default, `no-cache` for every file.
: CACHE-RULE! ( [ str -- str ] -- ) {: q :}
   HOOKS-OPEN
   q 0 CACHE-HOOK ! ;


\ The static path that answers a GET or HEAD whose own path names no file,
\ given that path; an empty answer is no fallback, which is the default. A
\ single-page application answers its client routes with its one page this
\ way.
: FALLBACK-RULE! ( [ str -- str ] -- ) {: q :}
   HOOKS-OPEN
   q 0 FALLBACK-HOOK ! ;


\ Queues the call `n q execute` for a request worker, which runs it as it runs
\ a request: WORKER-SLOT holds there, the ON-WORKER-START resources are open,
\ and a throw is one LOG:ERROR line, tag http, `call failed, throw <code>
\ <name>: <text>`, name and text when the code is declared, before the worker
\ goes on. Calls and connections are taken in the order they
\ were queued. It never waits: the queue's columns grow to hold every job, so
\ a caller always finds room for each call it has outstanding, and only a
\ mapping the memory rejects throws. Outside a run, or once STOP has begun, it
\ is E-STATE.
: SUBMIT ( n [ n -- ] -- )
   JOB-LOCK TASK:GET
   [: QUEUE-CALL ;] [: JOB-UNLOCK ;] finally ;


\ The connection and the peer's bytes past what the worker read (valid until the
\ handler ends); the taker closes it, or REPLY does. E-STATE outside a handler
\ or a second time.
: TAKE-OVER ( request -- TCP4:connection str ) {: subject:request :}
   subject SLOT-OF-REQUEST {: idx:n :}
   idx HOLDING
   TAKEN idx HOLDER !
   construct access handed-off idx ACCESS-STATE!
   0 idx SERVING-LIVE !
   idx SERVING @
   idx idx IN-AT@ idx AVAIL SPAN$ ;


\ One whole answer on a connection a handler took, from any task: the status,
\ the content type and the body, with Connection: close; a BODYLESS? status
\ sends no length and no body. Then the lingering close an answered worker
\ connection gets. True when the OS took every byte, false when a write failed
\ (the peer has reset the connection). The head is built in the calling task's
\ string builder, so a content type past its room is E-STR-CAPACITY. REPLY
\ closes the connection on every way out, a throw or a TASK:HALT included.
: REPLY ( TCP4:connection n str str -- bool )
   {: conn:TCP4:connection status:n type:str body:str :}
   conn TCP4:CONNECTION>N REPLYING !
   status type body [: SEND-REPLY ;] [: CLOSE-REPLYING ;] finally ;


\ The address and port to listen on, how many request workers to run, and how
\ long a connection may stay silent before it is closed.
STRUCTURE config DERIVE addr
   FIELD address n
   FIELD port n
   FIELD workers n
   FIELD idle ms
;STRUCTURE


\ Every hook and route is registered before START: a running server makes each
\ hook and route word E-STATE, and a worker reads the route table unordered,
\ while a word reaches a running task only through a channel that orders
\ (src/lib/task.f). Route strings must outlive the server. The AIO loop is a
\ precondition and not this package's to start: every readiness wait is an AIO
\ submission, and one with no loop is E-AIO-STATE. Thrown inside the listener's
\ task that code is a report nobody reads (no connection is accepted and the
\ caller hangs on the idle deadline), so it is re-thrown here.
: START ( ^config -- )
   {: cfg:^config :}
   cfg HTTP-CONFIG:ADDRESS @ {: address:n :}
   cfg HTTP-CONFIG:PORT @ {: port:n :}
   cfg HTTP-CONFIG:WORKERS @ {: workers:n :}
   cfg HTTP-CONFIG:IDLE @ MS>N {: idle:n :}
   RUNNING @ 0 <> if E-STATE throw then
   AIO:RUNNING? 0= if E-AIO-STATE throw then
   workers 1 < if E-WORKERS throw then
   workers MAX-WORKERS > if E-WORKERS throw then
   idle 1 < if E-WORKERS throw then
   idle IDLE-MS !
   workers WORKER-COUNT !
   0 STOPPING !
   0 CLAIMED !
   0 ENDED-COUNT !
   0 LEFT-COUNT !
   0 START-ERROR atomic!
   0 STARTED !
   0 ACCEPTED-COUNT !
   address port BIND-LISTENER
   [: workers OPEN-RESOURCES ;] catch {: code:n :}
   code 0 <> if RELEASE-RESOURCES code throw then
   1 RUNNING !
   START-WORKERS {: launch-code:n :}
   launch-code 0 <> if launch-code ROLLBACK-START then
   WAIT-START {: hook-code:n :}
   hook-code 0 <> if hook-code ROLLBACK-START then
   [: START-LISTENER ;] catch {: listener-code:n :}
   listener-code 0 <> if
      LISTENER-TASK TASK:KILL
      listener-code ROLLBACK-START
   then ;


: PORT ( -- n )
   BOUND-PORT @ ;


\ The address the listener actually bound, which is what a package deciding
\ whom to trust on this server must ask rather than the configuration.
: ADDRESS ( -- n )
   BOUND-ADDRESS @ ;


\ How many of the server's tasks reached the end of their own body, counted by
\ the tasks themselves, and how many there were.
: ENDED-TASKS ( -- n )
   ENDED-COUNT @ ;


: TASK-TOTAL ( -- n )
   TASK-COUNT ;


\ How many of them the last stop had to kill past its bound instead of waiting
\ out. Zero is a server every task of which ended itself.
: KILLED-TASKS ( -- n )
   KILLED-COUNT @ ;


\ Asks the listener and every worker to end - the flag the listener reads, and
\ one stop job for each worker, queued behind every call SUBMIT queued before
\ the flag - waits for them to end their own bodies to STOP-BOUND-MS, and then
\ gives back everything the server took. The flag rises under JOB-LOCK, so a
\ SUBMIT has queued its call before it or is E-STATE. The stopper names itself
\ before it raises the flag, so every task that sees the flag has somebody to
\ hint at. The kill is only the backstop past the bound: a task killed there
\ ends inside its wait with the rest of its body unrun, which is why
\ KILLED-TASKS is public.
\
\ Nothing waits on the queue, which grows to take the stop jobs. The
\ connections queued when the flag rises are closed unserved, and the room
\ they give back lets go a listener waiting for it, which then closes the one
\ it holds. The listener closes every parked connection as it leaves. The
\ calls run in order, within the bound as requests do. What is still queued or
\ parked once every task has been joined - behind a worker or with a listener
\ the stop killed - is dropped then: a connection closed, a call unrun.
: STOP ( -- )
   RUNNING @ 0= if E-STATE throw then
   TASK:SELF 0 STOPPER-TCB !
   JOB-LOCK TASK:GET
   [: STOP-QUEUE ;] [: JOB-UNLOCK ;] finally
   AWAIT-TASKS
   KILL-TASKS
   DRAIN-JOBS
   DRAIN-PARKED
   RELEASE-RESOURCES ;


: RUNNING? ( -- bool )
   RUNNING @ 0 <> ;


;package
