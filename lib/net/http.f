\ http.f - the HTTP/1.1 server: one listener task accepting on an IPv4
\ address, a fixed pool of request workers taking connections from a queue, and
\ one handler per request inside a catch boundary, so a handler that throws
\ answers 500 with the request id and the worker lives to take the next
\ connection. See docs/http.md.
\
\ STORAGE CLASS. PROCESS-WIDE: one server per image. The routes, the static
\ tree, the hooks and the rules are written before START and only read while it
\ runs; each worker's per-request state is its own slot's row
\ (lib/net/http-arena.f).
\
\ Every word a task runs is defined here, before START activates anything:
\ Habu forbids dictionary mutation while a task is live (docs/threads.md).
\
\ A package whose per-worker resource belongs to the worker task itself - a
\ database connection each worker holds, say - registers it with
\ ON-WORKER-START and ON-WORKER-EXIT. The start hooks run inside each worker
\ once it has claimed its slot and before it takes its first connection; the
\ exit hooks run through TASK:AT-EXIT, so a worker that throws gives its
\ resources back as surely as one that is stopped. Registration belongs to a
\ process's setup and is refused once its server is running.
require lib/net/http-static.f
require lib/task.f
require lib/queue.f
require lib/net/tcp4.f
require lib/span.f
require lib/aio.f

package HTTP

private

$20 constant BACKLOG
$100000 constant WORKER-STACK
$40 constant JOB-SLOTS
$8 constant MAX-HOOKS             \ packages that give a worker a resource of its own
-1 constant STOP-JOB
$C8 constant LINGER-MS            \ how long a refused peer may still be sending
$C8 constant ACCEPT-WAIT-MS       \ how long the listener parks before reading the stop flag
$C8 constant STOP-MARGIN-MS       \ scheduling slack over the longest park a stop waits out

ENUM served keep-open answered silent ;ENUM
2 constant STDERR-FD

JOB-SLOTS QUEUE:QUEUE JOBS
TASK:SEMAPHORE START-READY

WORKER-STACK TASK:TASK LISTENER-TASK
WORKER-STACK TASK:TASK WORKER-0
WORKER-STACK TASK:TASK WORKER-1
WORKER-STACK TASK:TASK WORKER-2
WORKER-STACK TASK:TASK WORKER-3
WORKER-STACK TASK:TASK WORKER-4
WORKER-STACK TASK:TASK WORKER-5
WORKER-STACK TASK:TASK WORKER-6
WORKER-STACK TASK:TASK WORKER-7

MAX-WORKERS TYPED-BUFFER WORKER-TCB ptr n

\ The task that raised STOPPING, in the same row type the pool's tasks live in.
\ Zero is the main thread's own record, which is the thread every caller of STOP
\ is on, so a hint before the first stop still reaches a thread that re-checks.
1 TYPED-BUFFER STOPPER-TCB ptr n

WORKER-0 0 WORKER-TCB !
WORKER-1 1 WORKER-TCB !
WORKER-2 2 WORKER-TCB !
WORKER-3 3 WORKER-TCB !
WORKER-4 4 WORKER-TCB !
WORKER-5 5 WORKER-TCB !
WORKER-6 6 WORKER-TCB !
WORKER-7 7 WORKER-TCB !

variable LISTENER-CELL
variable BOUND-PORT
variable BOUND-ADDRESS
variable STOPPING
variable WORKER-COUNT
variable RUNNING
variable IDLE-MS
variable CLAIMED                  \ slots the started workers have taken
variable ENDED-COUNT              \ tasks that reached the end of their body
variable LEFT-COUNT               \ tasks that left their body, however they left it
variable KILLED-COUNT             \ tasks a stop had to kill past its bound
variable START-ERROR              \ first failed start hook's actual throw code
variable STARTED                  \ workers successfully activated this start


: LISTENER@ ( -- TCP4:listener )
   LISTENER-CELL @ TCP4:>LISTENER ;


\ The last thing every task of this server does: count itself out of the server's
\ own work, then hint at the task that raised STOPPING, so a stop parks in the
\ kernel rather than polling. The count moves before the hint, which is what lets
\ the stopper trust one hint - WAKE is a hint and not a message (habu
\ docs/threads.md), so it re-reads the count after each one. A hint posted
\ outside a stop - a listener whose accept failed raises the flag itself - only
\ returns the stopper's thread to a check it repeats.
: LEAVING ( -- )
   1 LEFT-COUNT atomic-add drop
   STOPPING atomic@ 0= if exit then
   0 STOPPER-TCB @ TASK:WAKE ;


: DROP-STATUS ( TCP4:status -- )
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;


\ ---- one request ------------------------------------------------------------

: MAGNITUDE ( n -- n ) {: value:n :}
   value 0 < if 0 value - exit then
   value ;


\ The fault reaches the operator with the same request id the client is given.
: REPORT-FAULT ( n n -- ) {: idx:n code:n :}
   0 idx OUT-U!
   s" http: request " idx OUT+
   idx SLOT-ID$ idx OUT+
   s"  failed, throw " idx OUT+
   code 0 < if DASH-BYTE idx OUT+C then
   code MAGNITUDE idx NUM$ idx OUT+
   LF idx OUT+C
   STDERR-FD idx OUT-BUF idx OUT-U@ SPAN:TAKE SPAN:$ write drop
   0 idx OUT-U! ;


\ Whatever had been built is dropped: the client is owed one answer and the
\ caller is about to render it.
: DROP-ANSWER ( n -- ) {: idx:n :}
   0 idx HDR-U!
   idx IN-BUF 0 SPAN:TAKE SPAN:$ idx RESPONSE-BODY! ;


\ The error hook runs outside the handler's boundary for a refusal and for a
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


: DISPATCH ( n -- ) {: idx:n :}
   idx FIND-ROUTE {: route:n :}
   route 0 >= if
      route idx ROUTE-AT!
      idx REQUEST-OF idx RESPONSE-OF route ROUTE-HANDLER @ execute
      exit
   then
   idx PATH-MATCHED? if idx NOT-ALLOWED exit then
   idx SERVE-STATIC if exit then
   idx NOT-FOUND ;


\ The quotation cannot read a local, so the slot travels through the task.
: RUN-HANDLER ( -- )
   SELF-SLOT DISPATCH ;


\ A body the slot's JSON buffer does not hold is refused by name: the client
\ learns the answer exists and is too large, not that the handler broke.
: LARGE-RESPONSE ( n -- ) {: idx:n :}
   idx DROP-ANSWER
   idx RESPONSE-OF 500 s" response_too_large"
   s" the answer is larger than this server sends in one response" ERROR! ;


\ The handler's throw, kept for RENDER-FAULT: a quotation cannot read a local.
MAX-WORKERS TYPED-BUFFER FAULT-CODE n


: RENDER-FAULT ( -- )
   SELF-SLOT {: idx:n :}
   idx FAULT-CODE @ E-RESPONSE = if idx LARGE-RESPONSE exit then
   idx FAULT-RESPONSE ;


\ Every fault answer is a 500, stored before the hook runs so that a hook that
\ throws before setting it still leaves the status the client is owed.
: HANDLE-REQUEST ( n -- ) {: idx:n :}
   [: RUN-HANDLER ;] catch {: code:n :}
   code 0= if exit then
   idx code REPORT-FAULT
   code idx FAULT-CODE !
   500 idx SLOT-STATUS!
   [: RENDER-FAULT ;] catch {: failed:n :}
   idx failed HOOK-FAULT ;


\ ---- one connection ---------------------------------------------------------

: CLOSE-CONNECTION ( TCP4:connection -- )
   TCP4:CLOSE DROP-STATUS ;


: DISCARD ( TCP4:connection n -- bool ) {: conn:TCP4:connection idx:n :}
   conn idx IN-BUF SPAN:$ TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF drop true ENDOF
      closed OF drop false ENDOF
      failed OF drop false ENDOF
   ;MATCH ;


\ A peer still sending when we answer would be reset by a plain close and would
\ lose the answer with it, which is exactly how a refused oversize request ends.
\ Half-close instead, read what is still in flight until the peer stops or the
\ linger deadline passes, and only then close.
: LINGER-CLOSE ( TCP4:connection n -- ) {: conn:TCP4:connection idx:n :}
   conn TCP4:SENDING TCP4:SHUTDOWN DROP-STATUS
   LINGER-MS DEADLINE-AT {: deadline:n :}
   begin
      conn deadline WAIT-READABLE 0= if conn CLOSE-CONNECTION exit then
      conn idx DISCARD 0= if conn CLOSE-CONNECTION exit then
   again ;


: CONNECTION-RESET ( n -- ) {: idx:n :}
   0 idx IN-U!
   0 idx IN-AT!
   0 idx HEAD-U! ;


\ The refusal REFUSE stored as the slot's status: a quotation cannot read a
\ local.
: RENDER-REFUSAL ( -- )
   SELF-SLOT {: idx:n :}
   idx SLOT-STATUS@ {: status:n :}
   status 400 = if idx RESPONSE-OF 400 s" bad_request"
      s" the request line or a header is not valid HTTP" ERROR! exit then
   status 413 = if idx RESPONSE-OF 413 s" body_too_large"
      s" the request body is larger than this server accepts" ERROR! exit then
   status 431 = if idx RESPONSE-OF 431 s" headers_too_large"
      s" the request header block is larger than this server accepts" ERROR! exit then
   idx RESPONSE-OF 501 s" not_implemented"
   s" this server does not implement that transfer coding" ERROR! ;


: REFUSE ( n n -- ) {: idx:n status:n :}
   idx MAKE-ID
   0 idx KEEP-ALIVE!
   status idx SLOT-STATUS!
   [: RENDER-REFUSAL ;] catch {: failed:n :}
   idx failed HOOK-FAULT ;


: ANSWER ( TCP4:connection n -- served ) {: conn:TCP4:connection idx:n :}
   idx MAKE-ID
   idx HANDLE-REQUEST
   conn idx RESPONSE-OF SEND 0= if construct served silent exit then
   idx KEEP-ALIVE@ 0 <> if construct served keep-open exit then
   construct served answered ;


: SERVE-ONE ( TCP4:connection n -- served ) {: conn:TCP4:connection idx:n :}
   conn idx IDLE-MS @ READ-REQUEST {: result:parse-result :}
   result COMPLETE? if conn idx ANSWER exit then
   result REFUSAL-STATUS {: status:n :}
   status 0= if construct served silent exit then
   idx status REFUSE
   conn idx RESPONSE-OF SEND 0= if construct served silent exit then
   construct served answered ;


\ One connection until a request says otherwise: a peer that has been answered
\ leaves through the lingering close, one that is gone or silent through a
\ plain one.
: SERVE-CONNECTION ( TCP4:connection -- ) {: conn:TCP4:connection :}
   SELF-SLOT {: idx:n :}
   idx CONNECTION-RESET
   begin
      STOPPING atomic@ 0 <> if conn CLOSE-CONNECTION exit then
      conn idx SERVE-ONE
      MATCH served
         keep-open OF ENDOF
         answered OF conn idx LINGER-CLOSE exit ENDOF
         silent OF conn CLOSE-CONNECTION exit ENDOF
      ;MATCH
   again ;


\ ---- the worker lifecycle hooks ---------------------------------------------

\ The quotations live in rows declared to hold quotations, which is the
\ checker's proven-quotation store (docs/threads.md); an xt in a cell
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
\ back first.
: RUN-EXIT-HOOKS ( -- )
   SELF-SLOT CLOSE-FILE
   EXIT-HOOK-N @ 0 ?do EXIT-HOOK-N @ 1- i - EXIT-HOOK @ execute loop ;


\ ---- the worker pool --------------------------------------------------------

: WORKER-LOOP ( -- )
   begin
      JOBS QUEUE:POP {: job:n :}
      job 0 < if exit then
      job TCP4:>CONNECTION SERVE-CONNECTION
   again ;


: SERVE-JOBS ( -- )
   [: RUN-START-HOOKS ;] catch {: code:n :}
   code 0 <> if 0 code START-ERROR atomic-cas drop then
   START-READY TASK:SIGNAL
   code 0 <> if code throw then
   WORKER-LOOP
   1 ENDED-COUNT atomic-add drop ;


\ A worker takes the next free slot itself: the fetch-and-add hands each one a
\ different number, so no worker waits for the starter and no two share a slot.
\ Its resources are opened after that, because a hook names them by the slot,
\ and a hook that throws reports startup failure before ending this worker.
\
\ However this worker leaves - the stop job, or a hook or a word that threw - it
\ counts itself out before the throw goes on ending it, so a stop waits for the
\ workers that are still serving and for no others.
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
   STARTED @ 0 ?do START-READY TASK:WAIT loop
   START-ERROR atomic@ ;


\ ---- the listener -----------------------------------------------------------

: ACCEPTED ( TCP4:connection TCP4:address TCP4:port -- )
   {: conn:TCP4:connection peer:TCP4:address peer-port:TCP4:port :}
   STOPPING atomic@ 0 <> if conn CLOSE-CONNECTION exit then
   conn TCP4:CONNECTION>N JOBS QUEUE:PUSH ;


: ACCEPT-ONE ( -- )
   LISTENER@ TCP4:ACCEPT
   MATCH TCP4:accept-result
      accepted OF ACCEPTED ENDOF
      failed OF drop 1 STOPPING atomic-add drop ENDOF
   ;MATCH ;


\ The listener parks in poll(2) rather than in ACCEPT, so it reads the stop
\ flag between waits and needs nobody to connect to it to be let go.
: ACCEPT-LOOP ( -- )
   begin
      STOPPING atomic@ 0 <> if exit then
      LISTENER@ ACCEPT-WAIT-MS >MS TCP4:PENDING-WITHIN?
      MATCH TCP4:ready-result
         ready OF ACCEPT-ONE ENDOF
         idle OF ENDOF
         failed OF drop 1 STOPPING atomic-add drop ENDOF
      ;MATCH
   again ;


: ACCEPT-UNTIL-STOPPED ( -- )
   ACCEPT-LOOP
   1 ENDED-COUNT atomic-add drop ;


: LISTENER-BODY ( -- )
   [: ACCEPT-UNTIL-STOPPED ;] catch {: code:n :}
   LEAVING
   code 0 <> if code throw then ;


: START-LISTENER ( -- )
   [: LISTENER-BODY ;] LISTENER-TASK TASK:ACTIVATE ;


\ ---- binding ----------------------------------------------------------------

: BIND-LISTENER ( n n -- ) {: address:n port:n :}
   address TCP4:ADDRESS port TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF TCP4:LISTENER>N LISTENER-CELL ! ENDOF
      failed OF drop E-SOCKET throw ENDOF
   ;MATCH
   LISTENER@ BACKLOG TCP4:LISTEN
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop E-SOCKET throw ENDOF
   ;MATCH
   LISTENER@ TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N BOUND-PORT ! TCP4:ADDRESS>N BOUND-ADDRESS ! ENDOF
      failed OF drop E-SOCKET throw ENDOF
   ;MATCH ;


\ ---- ending the tasks -------------------------------------------------------

\ The listener and the workers: the tasks a stop waits for.
: TASK-COUNT ( -- n )
   WORKER-COUNT @ 1+ ;


\ How long a stop waits for the tasks to end themselves. The listener re-reads
\ STOPPING every ACCEPT-WAIT-MS; a worker inside a request is bounded by that
\ request's own deadline (IDLE-MS, SERVE-ONE into lib/net/http-request.f
\ WAIT-READABLE) and after it by LINGER-MS in LINGER-CLOSE, so the longer of the
\ two parks is what a stop waits out, plus the scheduling slack.
: STOP-BOUND-MS ( -- n )
   IDLE-MS @ LINGER-MS + {: request:n :}
   request ACCEPT-WAIT-MS < if ACCEPT-WAIT-MS STOP-MARGIN-MS + exit then
   request STOP-MARGIN-MS + ;


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
\ record nobody awaits is one the ring still holds (docs/aio.md), which
\ would refuse the LOOP-STOP that follows this server.
: DROP-TIMER ( AIO:ticket -- ) {: timer:AIO:ticket :}
   timer AIO:CANCEL
   timer AIO:AWAIT
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;


\ Waits for the tasks to leave their own bodies, to the bound. The only wake-ups
\ are each task's own LEAVING and one AIO:TIMEOUT for the bound - the loop wakes
\ the record's owner when the timer completes (docs/aio.md) - so nothing
\ here polls a clock, and the stopper re-reads the count after every hint, as the
\ hint protocol requires (docs/threads.md).
: AWAIT-TASKS ( -- )
   STOP-BOUND-MS >MS AIO:TIMEOUT {: timer:AIO:ticket :}
   STOP-BOUND-MS DEADLINE-AT {: deadline:n :}
   begin
      TASKS-LEFT? mono-ns deadline > or if timer DROP-TIMER exit then
      TASK:STOP
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


\ Everything a running server holds beyond its listener. The count is read from
\ the cell because a quotation cannot read a local.
: OPEN-RESOURCES ( -- )
   WORKER-COUNT @ ARENA-OPEN
   JOBS QUEUE:INIT
   0 START-READY TASK:SEMAPHORE-INIT ;


: DROP-LISTENER ( -- )
   LISTENER@ TCP4:CLOSE-LISTENER DROP-STATUS ;


: RELEASE-RESOURCES ( -- )
   DROP-LISTENER
   JOBS QUEUE:DESTROY
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


: ROLLBACK-START ( n -- ) {: code:n :}
   STARTED @ 0 ?do STOP-JOB JOBS QUEUE:PUSH loop
   JOIN-STARTED
   RELEASE-RESOURCES
   code throw ;


public

\ Opens what a request worker needs of its own, inside that worker, once per
\ worker and before its first request. START waits for every start hook; a
\ throw fails startup and ends the whole pool after every worker exits.
: ON-WORKER-START ( [ -- ] -- ) {: q :}
   HOOKS-OPEN
   START-HOOK-N @ HOOK-ROOM {: at:n :}
   q at START-HOOK !
   at 1+ START-HOOK-N ! ;


\ Gives it back, in the worker's own task, however the worker ended.
: ON-WORKER-EXIT ( [ -- ] -- ) {: q :}
   HOOKS-OPEN
   EXIT-HOOK-N @ HOOK-ROOM {: at:n :}
   q at EXIT-HOOK !
   at 1+ EXIT-HOOK-N ! ;


\ How every error answer is rendered: the refusals the server makes itself
\ (400, 404, 405, 413, 431, 500, 501) and every ERROR! a handler calls, given
\ the response, the status, a short code and a message. The hook sets the
\ status and the body through the public response words; REQUEST-ID$ is the id
\ the fault was reported under. It replaces the plain-text default, one line
\ of `code: message (request id)`. For a refusal or a fault the hook runs
\ outside the handler's boundary under the worker's own catch: a hook that
\ throws there is reported on stderr under the request id, and the client gets
\ the plain-text default with code error_hook and the status it was owed, so
\ the worker answers and takes the next connection.
: ON-ERROR ( [ response n ptr u8 n ptr u8 n -- ] -- ) {: q :}
   HOOKS-OPEN
   q 0 ERROR-HOOK ! ;


\ The Cache-Control value a static file is sent with, given its path. It
\ replaces the default, `no-cache` for every file.
: CACHE-RULE! ( [ ptr u8 n -- ptr u8 n ] -- ) {: q :}
   HOOKS-OPEN
   q 0 CACHE-HOOK ! ;


\ The static path that answers a GET or HEAD whose own path names no file,
\ given that path; an empty answer is no fallback, which is the default. A
\ single-page application answers its client routes with its one page this
\ way.
: FALLBACK-RULE! ( [ ptr u8 n -- ptr u8 n ] -- ) {: q :}
   HOOKS-OPEN
   q 0 FALLBACK-HOOK ! ;


\ The address and port to listen on, how many request workers to run, and how
\ long a connection may stay silent before it is closed. The AIO loop is a
\ precondition and not this package's to start: every readiness wait the
\ listener and the workers make is an AIO submission, and a submission with no
\ loop running is E-AIO-STATE. Thrown inside the listener's own task that code
\ is a report nobody reads - the task dies, no connection is ever accepted and
\ the caller waiting for its first answer hangs on the idle deadline instead -
\ so the same code is re-thrown here, to the caller that can still fix it.
: START ( n n n n -- ) {: address:n port:n workers:n idle:n :}
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
   address port BIND-LISTENER
   [: OPEN-RESOURCES ;] catch {: code:n :}
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
\ one stop job for each worker parked on the queue - waits for them to end their
\ own bodies to STOP-BOUND-MS, and then gives back everything the server took.
\ The stopper names itself before it raises the flag, so every task that sees
\ the flag has somebody to hint at. The kill is only the backstop past the
\ bound: a task killed there ends inside its wait with the rest of its body
\ unrun, which is why KILLED-TASKS is public.
: STOP ( -- )
   RUNNING @ 0= if E-STATE throw then
   TASK:SELF 0 STOPPER-TCB !
   1 STOPPING atomic-add drop
   WORKER-COUNT @ 0 ?do STOP-JOB JOBS QUEUE:PUSH loop
   AWAIT-TASKS
   KILL-TASKS
   RELEASE-RESOURCES ;


: RUNNING? ( -- bool )
   RUNNING @ 0 <> ;


;package
