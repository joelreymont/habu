\ signal-test.f - package SIGNAL: the self-pipe, the caught set, the WAIT
\ window and the refusals, against real deliveries this process sends itself.
\
\ Only SIGUSR1 and SIGUSR2 are ever raised here. SIGTERM and SIGINT would end
\ the run on any path that reaches them before their disposition is installed.
\
\ Run: bin/hb --load lib/signal-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/task.f
require lib/ffi-abi.f
require lib/aio.f
require lib/signal.f
require src/habu/layout.f

package SIGNAL-TEST
private

\ THE EXPECTATIONS FOLLOW THE HOST, AND THIS FILE SPELLS THEM ITSELF. lib/signal.f
\ selects its record layout, its SA_RESTART and its SIGUSR numbers per target;
\ asserting one host's values would make this suite - and the process-signals
\ gate that runs it - red by construction on the other supported target. The
\ arms below are a second, independent spelling of the same platform facts, so a
\ wrong arm in the library still fails here. The macOS arm is never executed:
\ no macOS host runs this suite, and a third target is refused by name.
: EXPECT-SIGUSR1 ( -- n )
   HB-TARGET-LINUX? if 10 exit then
   HB-TARGET-MACOS? if 30 exit then
   E-PROC-HOST throw ;

: EXPECT-SIGUSR2 ( -- n )
   HB-TARGET-LINUX? if 12 exit then
   HB-TARGET-MACOS? if 31 exit then
   E-PROC-HOST throw ;

: SA-FLAGS-OFF ( -- n )
   HB-TARGET-LINUX? if $88 exit then
   HB-TARGET-MACOS? if $0C exit then
   E-PROC-HOST throw ;

: SA-RESTART ( -- n )
   HB-TARGET-LINUX? if $10000000 exit then
   HB-TARGET-MACOS? if 2 exit then
   E-PROC-HOST throw ;

\ One buffer of the larger record ($98 on glibc/aarch64, $10 on macOS) serves
\ both, exactly as the library's does, so only the offset above needs an arm.
$98 constant SA-BUF-BYTES
0 constant SA-NO-FLAGS
0 constant SIG-DFL                 \ the handler an unarmed signal carries
$FFFFFFFF constant INT-MASK        \ sa_flags is an int; the cell read carries its padding
1 constant F-GETFD                 \ lib/process.f names only F-SETFD

0 constant SIG-BELOW               \ one below the lowest signal sigaction installs
65 constant SIG-ABOVE              \ one above the highest

25 constant RAISE-WAIT-MS          \ the stub has already written by the time kill returns
40 constant QUIET-MS               \ the window a WAIT with nothing raised waits out
1000000 constant NS-PER-MS
10 constant TASK-RAISE-MS          \ the raiser waits this long, inside the main task's sleep
120 constant TASK-SLEEP-MS
2000 constant TASK-JOIN-MS

400 constant RACE-WINDOW-MS        \ the window both racing tasks give their WAIT
250 constant RACE-SLACK-MS         \ what a loaded host may add to it
80 constant RACE-ARM-MS            \ both tasks are inside poll before the raise
1 constant RACE-SIGNAL             \ the kinds a racing task records
2 constant RACE-TIMEOUT
1 constant NOT-RUN                 \ no throw code and no kind, so an idle task fails

create SA-ACT SA-BUF-BYTES allot
create SA-OLD SA-BUF-BYTES allot

PTR-VARIABLE ABI-KEPT              \ it holds a pointer, so it is declared as one
variable MAIN-FD-WORD
variable TASK-FD-WORD
variable TASK-RAISED
variable RELEASED-FD

variable A-KIND                    \ what the first racing task's WAIT answered
variable A-SIGNO
variable A-MS
variable B-KIND
variable B-SIGNO
variable B-MS

variable OUTSIDER-CATCH            \ what CATCH, RELEASE and FD threw off-owner
variable OUTSIDER-RELEASE
variable OUTSIDER-FD

TASK:MIN-STACK TASK:TASK SIG-RAISER
TASK:MIN-STACK TASK:TASK RACER-A
TASK:MIN-STACK TASK:TASK RACER-B
TASK:MIN-STACK TASK:TASK OUTSIDER

PROCESS-SYMBOLS

FUNCTION: SIGACTION-CALL sigaction ( n ptr u8 ptr u8 -- n )
   2 SA-BUF-BYTES WRITES-BYTES
;FUNCTION

: ZERO-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      0 over a + c!
      1+
   repeat drop ;

: ABI-SLOT ( n -- ptr n ) {: off:n :}
   data-base off + ;

: STUB@ ( -- n )
   SIGNAL-ABI:STUB-CELL ABI-SLOT @ ;

\ Read in the MAIN task, where the boot wrote it: the fd word's absolute
\ address. A spawned task's region carries neither published cell, so this
\ pointer is the only way that task reaches the one word the stub writes
\ through.
: KEEP-FD-WORD ( -- )
   SIGNAL-ABI:FD-PTR-CELL ABI-SLOT 0 ptr-field @ ABI-KEPT ! ;

: FD-WORD ( -- ptr n )
   ABI-KEPT @ ;

: RAISE ( n -- ) {: sig:n :}
   getpid sig kill 0 <> if E-PROC-OUTPUT throw then ;

: FD-OPEN? ( n -- bool ) {: fd:n :}
   fd F-GETFD 0 fcntl 0 >= ;

\ F_GETFL reads the flags back, so "INIT asked for non-blocking" and "the
\ descriptor IS non-blocking" are different claims and this is the second one.
: NONBLOCK? ( n -- bool ) {: fd:n :}
   fd F-GETFL 0 fcntl {: flags :}
   flags 0 < if false exit then
   flags O-NONBLOCK and 0 <> ;

: ELAPSED-MS ( n -- n ) {: started:n :}
   mono-ns started - NS-PER-MS / ;

\ Hands back the disposition a signal is carrying, by installing the one it is
\ expected to hold over it: the record sigaction returns in oldact is the
\ assertion, and the disposition is where it was when this returns.
: SET-DISPOSITION ( n n n -- ) {: sig:n handler:n flags:n :}
   SA-ACT SA-BUF-BYTES ZERO-BYTES
   SA-OLD SA-BUF-BYTES ZERO-BYTES
   handler SA-ACT cell-view !
   flags SA-ACT SA-FLAGS-OFF + cell-view !
   sig SA-ACT SA-OLD SIGACTION-CALL 0 <> if E-PROC-OUTPUT throw then ;

: REINSTALL ( n -- ) {: sig:n :}
   sig STUB@ SA-RESTART SET-DISPOSITION ;

: RE-DEFAULT ( n -- ) {: sig:n :}
   sig SIG-DFL SA-NO-FLAGS SET-DISPOSITION ;

: OLD-HANDLER ( -- n )
   SA-OLD cell-view @ ;

: OLD-FLAGS ( -- n )
   SA-OLD SA-FLAGS-OFF + cell-view @ INT-MASK and ;

: WANT-SIGNAL ( SIGNAL:signal-result n -- ) {: want:n :}
   MATCH SIGNAL:signal-result
      signal OF want T= ENDOF
      timeout OF -1 want T= ENDOF
   ;MATCH ;

: WANT-TIMEOUT ( SIGNAL:signal-result -- )
   MATCH SIGNAL:signal-result
      signal OF -1 T= ENDOF
      timeout OF 0 0 T= ENDOF
   ;MATCH ;

\ ---- case zero: the target arms picked this host's values --------------------
\ 10/12 on Linux and 30/31 on macOS, against this file's own arm rather than the
\ library's: a library that spelled one host's number everywhere would agree
\ with itself on both and fail here on one.

: HOST-CASE ( -- )
   s" SIGUSR1 carries this host's number, not the other host's" T-LABEL
   SIGNAL:SIGUSR1 EXPECT-SIGUSR1 T=

   s" ... and so does SIGUSR2" T-LABEL
   SIGNAL:SIGUSR2 EXPECT-SIGUSR2 T= ;

\ ---- case one: every word refuses before INIT --------------------------------

: COLD-CASE ( -- )
   s" FD before INIT is refused by name" T-LABEL
   [: SIGNAL:FD drop ;] E-SIGNAL-STATE TTHROWSQ

   s" PENDING? before INIT is refused by name" T-LABEL
   [: SIGNAL:PENDING? drop ;] E-SIGNAL-STATE TTHROWSQ

   s" WAIT before INIT is refused by name" T-LABEL
   [: 0 >MS SIGNAL:WAIT drop ;] E-SIGNAL-STATE TTHROWSQ

   s" CATCH before INIT is refused by name" T-LABEL
   [: SIGNAL:SIGUSR1 SIGNAL:CATCH ;] E-SIGNAL-STATE TTHROWSQ

   s" RELEASE before INIT is refused by name" T-LABEL
   [: SIGNAL:RELEASE ;] E-SIGNAL-STATE TTHROWSQ ;

\ ---- case two: INIT arms the one word, and refuses a second one --------------

: ARMED-CASE ( -- )
   s" INIT armed the fd word with a descriptor" T-LABEL
   FD-WORD @ 0 <> TTRUE

   s" ... which is the write end, not the read end a program polls" T-LABEL
   FD-WORD @ SIGNAL:FD FD>N <> TTRUE

   s" the write end the stub writes really is non-blocking" T-LABEL
   FD-WORD @ NONBLOCK? TTRUE

   s" ... and so is the read end WAIT reads, so a lost race is refused" T-LABEL
   SIGNAL:FD FD>N NONBLOCK? TTRUE

   s" a second INIT without RELEASE is refused by name" T-LABEL
   [: SIGNAL:INIT ;] E-SIGNAL-STATE TTHROWSQ ;

\ ---- case two and a half: the window waits on the loop -----------------------
\ The facility is armed and the loop is not: WAIT and PENDING? say so by name
\ instead of falling back to a thread parked in poll(2). A COLD facility still
\ names E-SIGNAL-STATE (case one), because NEED-READY runs before the wait.

: NO-LOOP-CASE ( -- )
   AIO:STOP

   s" WAIT with the loop stopped is refused by name" T-LABEL
   [: QUIET-MS >MS SIGNAL:WAIT drop ;] E-AIO-STATE TTHROWSQ

   s" ... and so is PENDING?, which asks the same question with a zero window" T-LABEL
   [: SIGNAL:PENDING? drop ;] E-AIO-STATE TTHROWSQ

   AIO:START ;

\ ---- case three: what CATCH installed ----------------------------------------

: CAUGHT-CASE ( -- )
   SIGNAL:SIGUSR1 REINSTALL

   s" CATCH installed the engine's published stub, not a Forth word" T-LABEL
   OLD-HANDLER STUB@ T=

   s" ... with SA_RESTART, so a blocked read or write resumes" T-LABEL
   OLD-FLAGS SA-RESTART T=

   s" a signal number below the range sigaction installs is refused" T-LABEL
   [: SIG-BELOW SIGNAL:CATCH ;] E-SIGNAL-NUMBER TTHROWSQ

   s" ... and one above it" T-LABEL
   [: SIG-ABOVE SIGNAL:CATCH ;] E-SIGNAL-NUMBER TTHROWSQ ;

\ ---- case four: a raised signal comes back as its number ---------------------

: DELIVERED-CASE ( -- )
   SIGNAL:SIGUSR1 RAISE

   s" a raised signal is answered by WAIT as its own number" T-LABEL
   RAISE-WAIT-MS >MS SIGNAL:WAIT SIGNAL:SIGUSR1 WANT-SIGNAL

   s" ... once, not once per WAIT" T-LABEL
   0 >MS SIGNAL:WAIT WANT-TIMEOUT ;

\ ---- case five: two before one WAIT are answered in order --------------------

: ORDER-CASE ( -- )
   SIGNAL:SIGUSR1 RAISE
   SIGNAL:SIGUSR2 RAISE

   s" two signals raised before one WAIT answer in the order raised" T-LABEL
   RAISE-WAIT-MS >MS SIGNAL:WAIT SIGNAL:SIGUSR1 WANT-SIGNAL

   s" ... and the second WAIT answers the second" T-LABEL
   RAISE-WAIT-MS >MS SIGNAL:WAIT SIGNAL:SIGUSR2 WANT-SIGNAL ;

\ ---- case six: an empty window closes as a timeout ---------------------------

: TIMEOUT-CASE ( -- )
   mono-ns {: started :}
   QUIET-MS >MS SIGNAL:WAIT {: answer :}

   s" WAIT with nothing raised answers a timeout" T-LABEL
   answer WANT-TIMEOUT

   s" ... after waiting out the window it was given" T-LABEL
   started ELAPSED-MS QUIET-MS >= TTRUE ;

\ ---- case seven: PENDING? around one delivery --------------------------------

: PENDING-CASE ( -- )
   s" PENDING? is false with nothing on the descriptor" T-LABEL
   SIGNAL:PENDING? 0= TTRUE

   SIGNAL:SIGUSR2 RAISE

   s" ... true once a signal has been written to it" T-LABEL
   SIGNAL:PENDING? TTRUE

   s" ... and still true, because PENDING? consumes nothing" T-LABEL
   SIGNAL:PENDING? TTRUE

   s" the WAIT that answers it is what consumes it" T-LABEL
   RAISE-WAIT-MS >MS SIGNAL:WAIT SIGNAL:SIGUSR2 WANT-SIGNAL

   s" ... after which PENDING? is false again" T-LABEL
   SIGNAL:PENDING? 0= TTRUE ;

\ ---- case eight: a delivery in a spawned task's company ----------------------
\ The handler runs on whichever thread the kernel hands the signal to, and the
\ main task is inside TASK:SLEEP for the whole of it. The raiser reads the fd
\ word through the pointer kept in the main task, because its own region
\ carries zero at both published cells.

: RAISER-WORK ( -- )
   FD-WORD @ TASK-FD-WORD !
   TASK-RAISE-MS >MS TASK:SLEEP
   SIGNAL:SIGUSR1 RAISE
   1 TASK-RAISED ! ;

\ TASK:JOIN would park the main task forever on a worker that never ends, and a
\ worker that never ends is exactly what some of these cases are watching for,
\ so the wait owns a deadline and the case asserts TASK:DONE? afterwards.
: JOIN-TASK ( ptr n -- ) {: t:ptr :}
   TASK-JOIN-MS >MS PROC-DEADLINE-AT {: deadline :}
   begin
      t TASK:DONE? if exit then
      deadline PROC-LEFT-MS MS>N 0= if exit then
      TASK:PAUSE
   again ;

: TASK-CASE ( -- )
   0 TASK-FD-WORD !
   0 TASK-RAISED !
   ['] RAISER-WORK SIG-RAISER TASK:ACTIVATE
   TASK-SLEEP-MS >MS TASK:SLEEP
   SIG-RAISER JOIN-TASK

   s" the spawned task ran and raised the signal" T-LABEL
   TASK-RAISED @ 1 T=

   s" a spawned task reaches the armed fd word by its absolute address" T-LABEL
   TASK-FD-WORD @ FD-WORD @ T=

   s" ... and that word is the armed descriptor, not a zero it read locally" T-LABEL
   TASK-FD-WORD @ 0 <> TTRUE

   s" a signal raised while the main task slept is still answered" T-LABEL
   RAISE-WAIT-MS >MS SIGNAL:WAIT SIGNAL:SIGUSR1 WANT-SIGNAL ;

\ ---- case nine: two tasks WAIT, one signal ------------------------------------
\ Both tasks are inside one WAIT window when the signal lands, so both waits are
\ answered with POLLIN and only one of them finds four bytes left to read. Both
\ wait on the one loop, which costs neither task a thread parked in poll(2).
\ The loser's read is refused, it waits again against the deadline it had, and its own
\ window closes on a timeout. With a BLOCKING read end the loser parks in read
\ until the NEXT signal instead, which turns a race into a task that never
\ finishes the window it was given - so DONE? is asserted, not assumed.

: RACE-RUN ( ptr n ptr n ptr n -- ) {: kind:ptr signo:ptr elapsed:ptr :}
   mono-ns {: started :}
   RACE-WINDOW-MS >MS SIGNAL:WAIT {: answer :}
   started ELAPSED-MS elapsed !
   answer MATCH SIGNAL:signal-result
      signal OF signo ! RACE-SIGNAL kind ! ENDOF
      timeout OF RACE-TIMEOUT kind ! ENDOF
   ;MATCH ;

: RACE-A-WORK ( -- )
   A-KIND A-SIGNO A-MS RACE-RUN ;

: RACE-B-WORK ( -- )
   B-KIND B-SIGNO B-MS RACE-RUN ;

: RACE-CASE ( -- )
   0 A-KIND !  0 A-SIGNO !  0 A-MS !
   0 B-KIND !  0 B-SIGNO !  0 B-MS !
   ['] RACE-A-WORK RACER-A TASK:ACTIVATE
   ['] RACE-B-WORK RACER-B TASK:ACTIVATE
   RACE-ARM-MS >MS TASK:SLEEP
   SIGNAL:SIGUSR1 RAISE
   RACER-A JOIN-TASK
   RACER-B JOIN-TASK

   s" the first racing task finished the window it was given" T-LABEL
   RACER-A TASK:DONE? TTRUE

   s" ... and so did the second, rather than parking in read" T-LABEL
   RACER-B TASK:DONE? TTRUE

   s" one of the two answered the signal and the other a timeout" T-LABEL
   A-KIND @ B-KIND @ + RACE-SIGNAL RACE-TIMEOUT + T=

   s" ... and the one that answered carried the number raised" T-LABEL
   A-SIGNO @ B-SIGNO @ + SIGNAL:SIGUSR1 T=

   s" the loser closed on its own deadline, not on the next signal" T-LABEL
   A-MS @ B-MS @ max RACE-WINDOW-MS RACE-SLACK-MS + <= TTRUE ;

\ ---- case ten: CATCH and RELEASE answer to the task that ran INIT -------------
\ They install through the one SA-ACT and read back the one SA-OLD, so a second
\ task in either would be overwriting a record the owner is using. FD shares
\ none of that and stays callable.

: OUTSIDER-WORK ( -- )
   [: SIGNAL:SIGUSR1 SIGNAL:CATCH ;] catch OUTSIDER-CATCH !
   [: SIGNAL:RELEASE ;] catch OUTSIDER-RELEASE !
   [: SIGNAL:FD drop ;] catch OUTSIDER-FD ! ;

: OWNER-CASE ( -- )
   NOT-RUN OUTSIDER-CATCH !
   NOT-RUN OUTSIDER-RELEASE !
   NOT-RUN OUTSIDER-FD !
   ['] OUTSIDER-WORK OUTSIDER TASK:ACTIVATE
   OUTSIDER JOIN-TASK

   s" CATCH from a task that did not run INIT is refused by name" T-LABEL
   OUTSIDER-CATCH @ E-SIGNAL-STATE T=

   s" ... and so is RELEASE, which writes the same two records" T-LABEL
   OUTSIDER-RELEASE @ E-SIGNAL-STATE T=

   s" ... while FD, which writes neither, answers that task too" T-LABEL
   OUTSIDER-FD @ 0 T= ;

\ ---- case eleven: RELEASE disarms the word and closes both ends --------------

: RELEASE-CASE ( -- )
   SIGNAL:FD FD>N RELEASED-FD !
   FD-WORD @ MAIN-FD-WORD !
   SIGNAL:RELEASE

   s" RELEASE cleared the fd word, so a late signal writes nothing" T-LABEL
   FD-WORD @ 0 T=

   s" ... and the read end a program polled is closed" T-LABEL
   RELEASED-FD @ FD-OPEN? 0= TTRUE

   s" ... as is the write end it had armed" T-LABEL
   MAIN-FD-WORD @ FD-OPEN? 0= TTRUE

   SIGNAL:SIGUSR1 RE-DEFAULT

   s" RELEASE restored the default disposition of a caught signal" T-LABEL
   OLD-HANDLER SIG-DFL T=

   s" ... and took the stub's flags off with it" T-LABEL
   OLD-FLAGS SA-NO-FLAGS T=

   SIGNAL:SIGUSR2 RE-DEFAULT

   s" ... for every signal in the caught set, not just the first" T-LABEL
   OLD-HANDLER SIG-DFL T=

   s" the facility refuses its words again once released" T-LABEL
   [: SIGNAL:FD drop ;] E-SIGNAL-STATE TTHROWSQ ;

\ ---- case twelve: a read end closed behind the facility's back ---------------
\ Not a state a caller reaches politely - it is the one thing a wait on that
\ read end must never do, which is re-ask a descriptor the kernel will not poll
\ until the deadline runs out. The loop answers it on the REFUSED arm: the
\ descriptor number no longer names anything, so io_uring refuses the POLL
\ with EBADF rather than completing it with POLLNVAL, and READABLE-WITHIN? names
\ the poll for both. A wait that only counted an outcome would spin here and a
\ PENDING? that only counted one would answer true.

: BROKEN-CASE ( -- )
   SIGNAL:INIT
   SIGNAL:FD FD>N close-rc drop

   s" PENDING? on a read end poll refuses is not true, it is refused" T-LABEL
   [: SIGNAL:PENDING? drop ;] E-SIGNAL-POLL TTHROWSQ

   s" WAIT on a read end poll refuses names the poll, not a timeout" T-LABEL
   [: QUIET-MS >MS SIGNAL:WAIT drop ;] E-SIGNAL-POLL TTHROWSQ

   s" ... and RELEASE names the close it could not make" T-LABEL
   [: SIGNAL:RELEASE ;] E-SIGNAL-CLOSE TTHROWSQ ;

public

\ The loop is started after the last definition (a live task forbids
\ compilation) and stopped at the end; every WAIT and PENDING? below runs on it.
: RUN ( -- )
   T-RESET
   AIO:START
   HOST-CASE
   COLD-CASE
   SIGNAL:INIT
   KEEP-FD-WORD
   ARMED-CASE
   NO-LOOP-CASE
   SIGNAL:SIGUSR1 SIGNAL:CATCH
   SIGNAL:SIGUSR2 SIGNAL:CATCH
   CAUGHT-CASE
   DELIVERED-CASE
   ORDER-CASE
   TIMEOUT-CASE
   PENDING-CASE
   TASK-CASE
   RACE-CASE
   OWNER-CASE
   RELEASE-CASE
   BROKEN-CASE
   AIO:STOP
   T-REPORT ;

;package

SIGNAL-TEST:RUN
