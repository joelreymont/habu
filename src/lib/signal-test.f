\ signal-test.f - package SIGNAL against real deliveries. This process raises
\ only SIGUSR1 and SIGUSR2: SIGTERM and SIGINT would end the run on any path
\ that reaches them before their disposition is installed. SIGSTOP and SIGCONT
\ go to a child, and a forked child raises SIGTERM at itself.

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/test/wait.f
require src/lib/fd.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-fork.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/task.f
require src/lib/ffi-abi.f
require src/lib/aio.f
require src/lib/signal.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/at-exit.f
require src/habu/layout.f

package SIGNAL-TEST
private

\ src/lib/signal.f selects its record layout, SA_RESTART and the SIGUSR, SIGSTOP
\ and SIGCONT numbers per target. These arms spell the same platform facts
\ independently, so a wrong library arm fails here; asserting one host's values
\ would make this suite red on the other target. A third target is rejected by
\ name.
: EXPECT-SIGUSR1 ( -- n )
   HB-TARGET-LINUX? if 10 else 30 then ;

: EXPECT-SIGUSR2 ( -- n )
   HB-TARGET-LINUX? if 12 else 31 then ;

: EXPECT-SIGSTOP ( -- n )
   HB-TARGET-LINUX? if 19 else 17 then ;

: EXPECT-SIGCONT ( -- n )
   HB-TARGET-LINUX? if 18 else 19 then ;

: SA-FLAGS-OFF ( -- n )
   HB-TARGET-LINUX? if $88 else $0C then ;

: SA-RESTART ( -- n )
   HB-TARGET-LINUX? if $10000000 else 2 then ;

: SA-RESETHAND ( -- n )
   HB-TARGET-LINUX? if $80000000 else 4 then ;

\ glibc supplies the x86-64 restorer when it installs a sigaction, including
\ the default disposition restored by RELEASE. It is part of the observed
\ action flags, while ARM Linux and macOS leave this bit clear.
: EXPECT-FLAGS ( n -- n )
   HB-TARGET-LINUX-X86-64? if $04000000 or then ;

\ One buffer of the larger record ($98 on glibc/aarch64, $10 on macOS) serves
\ both, exactly as the library's does, so only the offset above needs an arm.
$98 constant SA-BUF-BYTES
0 constant SA-NO-FLAGS
0 constant SIG-DFL                 \ the handler an unarmed signal carries
$FFFFFFFF constant INT-MASK        \ sa_flags is an int; the cell read carries its padding
1 constant F-GETFD                 \ src/lib/fd.f names only FD:F-SETFD

0 constant SIG-BELOW               \ one below the lowest signal sigaction installs
65 constant SIG-ABOVE              \ one above the highest

40 constant QUIET-MS               \ the window a WAIT with nothing raised waits out
1000000 constant NS-PER-MS
10 constant TASK-RAISE-MS          \ the raiser waits this long, inside the main task's sleep
120 constant TASK-SLEEP-MS

400 constant RACE-WINDOW-MS        \ the window both racing tasks give their WAIT
250 constant RACE-SLACK-MS         \ what a loaded host may add to it
80 constant RACE-ARM-MS            \ both tasks are inside poll before the raise
1 constant RACE-SIGNAL             \ the kinds a racing task records
2 constant RACE-TIMEOUT
1 constant NOT-RUN                 \ no throw code and no kind, so an idle task fails

2 constant WUNTRACED               \ waitpid's flag on Linux and macOS
$7F constant STOP-MARK             \ a stop report's low status byte

create SA-ACT SA-BUF-BYTES allot
create SA-OLD SA-BUF-BYTES allot
create SIGNO-OUT 1 cells allot    \ the bytes a stub writes for one signal

PTR-VARIABLE ABI-KEPT
variable MAIN-FD-WORD
variable TASK-FD-WORD
variable TASK-RAISED
variable RELEASED-FD

variable A-KIND                    \ what the first racing task's WAIT answered
variable A-SIGNO
variable A-MS
variable A-START                   \ mono-ns as its WAIT began
variable B-KIND
variable B-SIGNO
variable B-MS
variable B-START

variable OUTSIDER-CATCH            \ what CATCH, RELEASE and FD threw off-owner
variable OUTSIDER-RELEASE
variable OUTSIDER-FD

variable STOP-PID                  \ the stop case's child until reaped, else 0
variable STOP-R                    \ its stdin's read end until closed, else -1
variable STOP-W                    \ that pipe's write end until closed, else -1
variable STOP-STATUS               \ the status waitpid wrote for the stop wait

create DIE-ROOT PATH-CAP allot     \ the stopped child's scratch, removed at exit
variable DIE-ROOT-U
create DIE-PATH PATH-CAP allot     \ the path the stopped child is judged by
variable DIE-PATH-U
$1000 constant DIE-CAP
create DIE-OUT DIE-CAP allot       \ the stopped child's captured streams
create DIE-ERR DIE-CAP allot

TASK:MIN-STACK TASK:TASK SIG-RAISER
TASK:MIN-STACK TASK:TASK RACER-A
TASK:MIN-STACK TASK:TASK RACER-B
TASK:MIN-STACK TASK:TASK OUTSIDER

PROCESS-SYMBOLS

FUNCTION: SIGACTION-CALL sigaction ( n ^u8 ^u8 -- i32 )
;FUNCTION

FUNCTION: WAITPID-CALL waitpid ( n ^u8 n -- i32 )
;FUNCTION

: ZERO-BYTES ( ^u8 n -- ) {: a:^ u :}
   0 begin dup u < while
      0 over a + c!
      1+
   repeat drop ;

\ Read in the MAIN task, where the boot wrote it: the fd word's absolute
\ address. A spawned task's region carries neither published cell, so this
\ pointer is the only way that task reaches the one word the stub writes
\ through.
: KEEP-FD-WORD ( -- )
   SIGNAL-ABI:FD ABI-KEPT ! ;

: FD-WORD ( -- ^n )
   ABI-KEPT @ ;

: RAISE ( n -- ) {: sig:n :}
   getpid sig kill 0 <> if E-PROC-OUTPUT throw then ;

: FD-OPEN? ( n -- bool ) {: fd:n :}
   fd F-GETFD 0 fcntl 0 >= ;

\ F_GETFL reads the flags back, so "INIT asked for non-blocking" and "the
\ descriptor IS non-blocking" are different claims and this is the second one.
: NONBLOCK? ( n -- bool ) {: fd:n :}
   fd FD:F-GETFL 0 fcntl {: flags :}
   flags 0 < if false exit then
   flags FD:O-NONBLOCK and 0 <> ;

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
   sig SIGNAL-ABI:STUB SA-RESTART SET-DISPOSITION ;

: RE-DEFAULT ( n -- ) {: sig:n :}
   sig SIG-DFL SA-NO-FLAGS SET-DISPOSITION ;

: OLD-HANDLER ( -- n )
   SA-OLD cell-view @ ;

: OLD-FLAGS ( -- n )
   SA-OLD SA-FLAGS-OFF + cell-view @ INT-MASK and ;

: WANT-SIGNAL ( SIGNAL:signal-result n -- ) {: want:n :}
   MATCH SIGNAL:signal-result
      signal OF want T= ENDOF
      timeout OF 0 want T= ENDOF
   ;MATCH ;

: WANT-TIMEOUT ( SIGNAL:signal-result -- )
   MATCH SIGNAL:signal-result
      signal OF -1 T= ENDOF
      timeout OF 0 0 T= ENDOF
   ;MATCH ;

\ SIGUSR1/2 10/12 on Linux and 30/31 on macOS, SIGSTOP/CONT 19/18 and 17/19,
\ against this file's own arm rather than the library's: a library that spelled
\ one host's number everywhere would agree with itself on both and fail here on
\ one.

: HOST-CASE ( -- )
   s" SIGUSR1 carries this host's number, not the other host's" T-LABEL
   SIGNAL:SIGUSR1 EXPECT-SIGUSR1 T=

   s" ... and so does SIGUSR2" T-LABEL
   SIGNAL:SIGUSR2 EXPECT-SIGUSR2 T=

   s" ... and so does SIGSTOP" T-LABEL
   SIGNAL:SIGSTOP EXPECT-SIGSTOP T=

   s" ... and so does SIGCONT" T-LABEL
   SIGNAL:SIGCONT EXPECT-SIGCONT T= ;

: READER-PATH ( -- str )
   s" /bin/cat" ;

: CLOSE-END ( ^n -- )
   {: v:^ :}
   v @ 0 < if exit then
   v @ close
   -1 v ! ;

\ The spawn gives the child only fds 0-2 (src/lib/process.f PROC-RAW), so
\ this process holds the only write end; closing it ends the child's read.
: SPAWN-READER ( -- )
   FD:PIPE {: r:fd w:fd :}
   r FD>N STOP-R !
   w FD>N STOP-W !
   PROC-ARGV-RESET
   READER-PATH r -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PID>N STOP-PID !
   STOP-R CLOSE-END ;

: SEND ( n -- ) {: sig:n :}
   STOP-PID @ >PID sig PROCESS:KILL-RAW RC>N 0 <> if E-PROC-OUTPUT throw then ;

\ A wait that saw the child end reaped it, so its pid is no longer ours.
: REAPED? ( PROCESS:ending -- bool )
   MATCH PROCESS:ending
     exited OF drop 0 STOP-PID ! 0 0= ENDOF
     signaled OF drop 0 STOP-PID ! 0 0= ENDOF
     timeout OF 0 0= 0= ENDOF
   ;MATCH ;

: STOP-REAP ( -- )
   STOP-R CLOSE-END
   STOP-W CLOSE-END
   STOP-PID @ 0= if exit then
   PROCESS:SIGKILL SEND
   STOP-PID @ >PID PROCESS:WAIT drop
   0 STOP-PID ! ;

\ Waits for the child to stop or end and answers the raw status; an ending
\ reaps it, so its pid is no longer ours. It needs no deadline: the reader
\ execs once and the spawn returns after that exec, so the stop is never lost.
: STOP-WAIT ( -- n )
   0 STOP-STATUS !
   begin
      STOP-PID @ STOP-STATUS BYTE-VIEW WUNTRACED WAITPID-CALL
      dup -1 = FFI:ERRNO FD:EINTR# = and
   while drop repeat
   STOP-PID @ <> if E-PROC-WAIT throw then
   STOP-STATUS @ {: status:n :}
   status $FF and STOP-MARK <> if 0 STOP-PID ! then
   status ;

\ The write end is closed, ending the child's read, only once the stop is
\ reported, so the child cannot exit before SIGCONT.
: STOP-BODY ( -- )
   SPAWN-READER
   SIGNAL:SIGSTOP SEND
   STOP-WAIT {: status:n :}
   SIGNAL:SIGSTOP 8 lshift STOP-MARK or {: stopped:n :}
   status stopped = {: held:bool :}

   s" a child sent SIGSTOP is reported stopped by that signal" T-LABEL
   status stopped T=
   held 0= if exit then

   STOP-W CLOSE-END
   SIGNAL:SIGCONT SEND
   STOP-PID @ >PID PROCESS:WAIT PROCESS:STATUS>ENDING {: done :}
   done REAPED? drop

   s" ... and once sent SIGCONT it exits 0" T-LABEL
   READER-PATH s" " s" " done 0 T-ENDING-EXITED= ;

: STOP-CASE ( -- )
   0 STOP-PID !
   -1 STOP-R !
   -1 STOP-W !
   [: STOP-BODY ;] [: STOP-REAP ;] finally ;

: COLD-CASE ( -- )
   s" FD before INIT is rejected by name" T-LABEL
   [: SIGNAL:FD drop ;] E-SIGNAL-STATE TTHROWSQ

   s" PENDING? before INIT is rejected by name" T-LABEL
   [: SIGNAL:PENDING? drop ;] E-SIGNAL-STATE TTHROWSQ

   s" WAIT before INIT is rejected by name" T-LABEL
   [: 0 >MS SIGNAL:WAIT drop ;] E-SIGNAL-STATE TTHROWSQ

   s" CATCH before INIT is rejected by name" T-LABEL
   [: SIGNAL:SIGUSR1 SIGNAL:CATCH ;] E-SIGNAL-STATE TTHROWSQ

   s" IGNORE before INIT is rejected by name" T-LABEL
   [: SIGNAL:SIGUSR1 SIGNAL:IGNORE ;] E-SIGNAL-STATE TTHROWSQ

   s" RELEASE before INIT is rejected by name" T-LABEL
   [: SIGNAL:RELEASE drop ;] E-SIGNAL-STATE TTHROWSQ ;

: ARMED-CASE ( -- )
   s" INIT armed the fd word with a descriptor" T-LABEL
   FD-WORD @ 0 <> TTRUE

   s" ... which is the write end, not the read end a program polls" T-LABEL
   FD-WORD @ SIGNAL:FD FD>N <> TTRUE

   s" the write end the stub writes really is non-blocking" T-LABEL
   FD-WORD @ NONBLOCK? TTRUE

   s" ... and so is the read end WAIT reads, so a lost race is rejected" T-LABEL
   SIGNAL:FD FD>N NONBLOCK? TTRUE

   s" a second INIT without RELEASE is rejected by name" T-LABEL
   [: SIGNAL:INIT ;] E-SIGNAL-STATE TTHROWSQ ;

\ The facility is armed and the loop is not: WAIT and PENDING? say so by name
\ instead of falling back to a thread parked in poll(2). A COLD facility still
\ names E-SIGNAL-STATE (case one), because NEED-READY runs before the wait.

: NO-LOOP-CASE ( -- )
   AIO:STOP

   s" WAIT with the loop stopped is rejected by name" T-LABEL
   [: QUIET-MS >MS SIGNAL:WAIT drop ;] E-AIO-STATE TTHROWSQ

   s" ... and so is PENDING?, which asks the same question with a zero window" T-LABEL
   [: SIGNAL:PENDING? drop ;] E-AIO-STATE TTHROWSQ

   AIO:START ;

: CAUGHT-CASE ( -- )
   SIGNAL:SIGUSR1 REINSTALL

   s" CATCH installed the engine's published stub, not a Forth word" T-LABEL
   OLD-HANDLER SIGNAL-ABI:STUB T=

   s" ... with SA_RESTART, so a blocked read or write resumes" T-LABEL
   OLD-FLAGS SA-RESTART EXPECT-FLAGS T=

   s" a signal number below the range sigaction installs is rejected" T-LABEL
   [: SIG-BELOW SIGNAL:CATCH ;] E-SIGNAL-NUMBER TTHROWSQ

   s" ... and one above it" T-LABEL
   [: SIG-ABOVE SIGNAL:CATCH ;] E-SIGNAL-NUMBER TTHROWSQ ;

: DELIVERED-CASE ( -- )
   SIGNAL:SIGUSR1 RAISE

   s" a raised signal is answered by WAIT as its own number" T-LABEL
   -1 >MS SIGNAL:WAIT SIGNAL:SIGUSR1 WANT-SIGNAL

   s" ... once, not once per WAIT" T-LABEL
   0 >MS SIGNAL:WAIT WANT-TIMEOUT ;

: ORDER-CASE ( -- )
   SIGNAL:SIGUSR1 RAISE
   SIGNAL:SIGUSR2 RAISE

   s" two signals raised before one WAIT answer in the order raised" T-LABEL
   -1 >MS SIGNAL:WAIT SIGNAL:SIGUSR1 WANT-SIGNAL

   s" ... and the second WAIT answers the second" T-LABEL
   -1 >MS SIGNAL:WAIT SIGNAL:SIGUSR2 WANT-SIGNAL ;

: TIMEOUT-CASE ( -- )
   mono-ns {: started :}
   QUIET-MS >MS SIGNAL:WAIT {: answer :}

   s" WAIT with nothing raised answers a timeout" T-LABEL
   answer WANT-TIMEOUT

   s" ... after waiting out the window it was given" T-LABEL
   started ELAPSED-MS QUIET-MS >= TTRUE ;

: PENDING-CASE ( -- )
   s" PENDING? is false with nothing on the descriptor" T-LABEL
   SIGNAL:PENDING? 0= TTRUE

   SIGNAL:SIGUSR2 RAISE

   s" ... true once a signal has been written to it" T-LABEL
   SIGNAL:PENDING? TTRUE

   s" ... and still true, because PENDING? consumes nothing" T-LABEL
   SIGNAL:PENDING? TTRUE

   s" the WAIT that answers it is what consumes it" T-LABEL
   -1 >MS SIGNAL:WAIT SIGNAL:SIGUSR2 WANT-SIGNAL

   s" ... after which PENDING? is false again" T-LABEL
   SIGNAL:PENDING? 0= TTRUE ;

\ The handler runs on whichever thread the kernel hands the signal to, and the
\ main task is inside TASK:SLEEP for the whole of it. The raiser reads the fd
\ word through the pointer kept in the main task, because its own region
\ carries zero at both published cells.

: RAISER-WORK ( -- )
   FD-WORD @ TASK-FD-WORD !
   TASK-RAISE-MS >MS TASK:SLEEP
   SIGNAL:SIGUSR1 RAISE
   1 TASK-RAISED ! ;

: JOIN-TASK ( ^n -- )
   [: dup TASK:DONE? ;] TEST-WAIT:POLL drop ;

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
   -1 >MS SIGNAL:WAIT SIGNAL:SIGUSR1 WANT-SIGNAL ;

\ Both tasks are inside one WAIT window when the signal lands, so both waits are
\ answered with POLLIN and only one of them finds four bytes left to read. Both
\ wait on the one loop, which costs neither task a thread parked in poll(2).
\ The loser's read is rejected, it waits again against the deadline it had, and its own
\ window closes on a timeout. With a BLOCKING read end the loser parks in read
\ until the NEXT signal instead, which turns a race into a task that never
\ finishes the window it was given.

: RACE-RUN ( ^n ^n ^n ^n -- )
   {: kind:^ signo:^ elapsed:^ start:^ :}
   mono-ns {: started :}
   started start !
   RACE-WINDOW-MS >MS SIGNAL:WAIT {: answer :}
   started ELAPSED-MS elapsed !
   answer MATCH SIGNAL:signal-result
      signal OF signo ! RACE-SIGNAL kind ! ENDOF
      timeout OF RACE-TIMEOUT kind ! ENDOF
   ;MATCH ;

: RACE-A-WORK ( -- )
   A-KIND A-SIGNO A-MS A-START RACE-RUN ;

: RACE-B-WORK ( -- )
   B-KIND B-SIGNO B-MS B-START RACE-RUN ;

: RACE-CASE ( -- )
   0 A-KIND !  0 A-SIGNO !  0 A-MS !  0 A-START !
   0 B-KIND !  0 B-SIGNO !  0 B-MS !  0 B-START !
   ['] RACE-A-WORK RACER-A TASK:ACTIVATE
   ['] RACE-B-WORK RACER-B TASK:ACTIVATE
   RACE-ARM-MS >MS TASK:SLEEP
   SIGNAL:SIGUSR1 RAISE
   mono-ns {: raised:n :}
   RACER-A JOIN-TASK
   RACER-B JOIN-TASK
   \ Both timing out on a signal raised after both windows had closed means
   \ main was starved past them: the row's deadline. A signal raised inside a
   \ window that neither answers was lost, and fails below.
   A-KIND @ RACE-TIMEOUT =  B-KIND @ RACE-TIMEOUT =  and
   raised A-START @ B-START @ max RACE-WINDOW-MS NS-PER-MS * + >=  and
   if s" signal-test RACE-RUN" TEST-WAIT:EXPIRED then

   s" one of the two answered the signal and the other a timeout" T-LABEL
   A-KIND @ B-KIND @ + RACE-SIGNAL RACE-TIMEOUT + T=

   s" ... and the one that answered carried the number raised" T-LABEL
   A-SIGNO @ B-SIGNO @ + SIGNAL:SIGUSR1 T=

   s" the loser closed on its own deadline, not on the next signal" T-LABEL
   A-MS @ B-MS @ max RACE-WINDOW-MS RACE-SLACK-MS + <= TTRUE ;

\ They install through the one SA-ACT and read back the one SA-OLD, so a second
\ task in either would be overwriting a record the owner is using. FD shares
\ none of that and stays callable.

: OUTSIDER-WORK ( -- )
   [: SIGNAL:SIGUSR1 SIGNAL:CATCH ;] catch OUTSIDER-CATCH !
   [: SIGNAL:RELEASE drop ;] catch OUTSIDER-RELEASE !
   [: SIGNAL:FD drop ;] catch OUTSIDER-FD ! ;

: OWNER-CASE ( -- )
   NOT-RUN OUTSIDER-CATCH !
   NOT-RUN OUTSIDER-RELEASE !
   NOT-RUN OUTSIDER-FD !
   ['] OUTSIDER-WORK OUTSIDER TASK:ACTIVATE
   OUTSIDER JOIN-TASK

   s" CATCH from a task that did not run INIT is rejected by name" T-LABEL
   OUTSIDER-CATCH @ E-SIGNAL-STATE T=

   s" ... and so is RELEASE, which writes the same two records" T-LABEL
   OUTSIDER-RELEASE @ E-SIGNAL-STATE T=

   s" ... while FD, which writes neither, answers that task too" T-LABEL
   OUTSIDER-FD @ 0 T= ;

: RELEASE-CASE ( -- )
   SIGNAL:FD FD>N RELEASED-FD !
   FD-WORD @ MAIN-FD-WORD !
   SIGNAL:SIGUSR1 RAISE
   [: SIGNAL:PENDING? ;] TEST-WAIT:POLL
   SIGNAL:SIGUSR2 SIGNO-OUT cell-view !
   MAIN-FD-WORD @ SIGNO-OUT 4 >str write 4 <> if E-PROC-OUTPUT throw then
   SIGNAL:RELEASE {: last:n :}

   s" RELEASE answers the last of two signals never taken" T-LABEL
   last SIGNAL:SIGUSR2 T=

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
   OLD-FLAGS SA-NO-FLAGS EXPECT-FLAGS T=

   SIGNAL:SIGUSR2 RE-DEFAULT

   s" ... for every signal in the caught set, not just the first" T-LABEL
   OLD-HANDLER SIG-DFL T=

   s" the facility rejects its words again once released" T-LABEL
   [: SIGNAL:FD drop ;] E-SIGNAL-STATE TTHROWSQ ;

\ Not a state a caller reaches politely - it is the one thing a wait on that
\ read end must never do, which is re-ask a descriptor the kernel will not poll
\ until the deadline runs out. The loop answers it on the REJECTED arm: the
\ descriptor number no longer names anything, so io_uring rejects the POLL
\ with EBADF rather than completing it with POLLNVAL, and READABLE-WITHIN? names
\ the poll for both. A wait that only counted an outcome would spin here and a
\ PENDING? that only counted one would answer true.

: BROKEN-CASE ( -- )
   SIGNAL:INIT
   SIGNAL:FD FD>N close-rc drop

   s" PENDING? on a read end poll rejects is not true, it is rejected" T-LABEL
   [: SIGNAL:PENDING? drop ;] E-SIGNAL-POLL TTHROWSQ

   s" WAIT on a read end poll rejects names the poll, not a timeout" T-LABEL
   [: QUIET-MS >MS SIGNAL:WAIT drop ;] E-SIGNAL-POLL TTHROWSQ

   s" ... and RELEASE names the close it could not make" T-LABEL
   [: SIGNAL:RELEASE drop ;] E-SIGNAL-CLOSE TTHROWSQ ;

\ SIGUSR1 carries SIG_DFL here (case eleven), and its default action ends the
\ process: a raise the ignore did not take ends this run with that signal.

: IGNORE-CASE ( -- )
   SIGNAL:INIT
   SIGNAL:SIGUSR1 SIGNAL:IGNORE

   s" IGNORE leaves the signal ignored" T-LABEL
   SIGNAL:SIGUSR1 SIGNAL:IGNORED? TTRUE

   SIGNAL:SIGUSR1 RAISE

   s" ... so a raise leaves the process running and the pipe empty" T-LABEL
   SIGNAL:PENDING? 0= TTRUE

   \ On SIGUSR2, so that SIGUSR1 reaches RELEASE ignored and never caught.
   s" a later CATCH replaces the ignore, so a raise reaches the pipe" T-LABEL
   SIGNAL:SIGUSR2 SIGNAL:IGNORE
   SIGNAL:SIGUSR2 SIGNAL:CATCH
   SIGNAL:SIGUSR2 RAISE
   -1 >MS SIGNAL:WAIT SIGNAL:SIGUSR2 WANT-SIGNAL

   s" IGNORE rejects a signal number outside the range by name" T-LABEL
   [: SIG-ABOVE SIGNAL:IGNORE ;] E-SIGNAL-NUMBER TTHROWSQ

   s" RELEASE answers 0 when nothing arrived after the last take" T-LABEL
   SIGNAL:RELEASE 0 T=

   SIGNAL:INIT

   s" RELEASE restored the default disposition of an ignored signal" T-LABEL
   SIGNAL:SIGUSR1 SIGNAL:IGNORED? 0= TTRUE

   SIGNAL:RELEASE drop ;

\ The child raises both: the ignored SIGUSR1 leaves it running, and the caught
\ SIGUSR2, back under the default action, ends it.
: FORKED-RAISES ( -- )
   SIGNAL:SIGUSR1 RAISE
   SIGNAL:SIGUSR2 RAISE ;

: FORK-CASE ( -- )
   SIGNAL:INIT
   SIGNAL:SIGUSR1 SIGNAL:IGNORE
   SIGNAL:SIGUSR2 SIGNAL:CATCH
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if [: FORKED-RAISES ;] ERROR:CHILD-TOP then
   pid PROCESS:WAIT PROCESS:STATUS>ENDING {: done :}
   SIGNAL:RELEASE drop

   s" a forked child keeps an ignore and drops the stub of a catch" T-LABEL
   s" FORKED-RAISES" s" " s" " done SIGNAL:SIGUSR2 T-ENDING-SIGNALED= ;

\ A file under the scratch root, made by this process, so the child only
\ registers it.
: DIE-FILE ( str ^u8 -- n )
   {: name:str dst:^ :}
   DIE-ROOT DIE-ROOT-U @ >str name dst FS:JOIN-PATH {: u :}
   dst u >str s" made" FS:ATOMIC-WRITE-FILE
   u ;

\ Raises SIGTERM at itself and answers it as a program that caught stops does.
: STOP-SELF ( -- )
   SIGNAL:SIGTERM RAISE
   SIGNAL:TAKE MATCH SIGNAL:signal-result
      signal OF s" signal-test: stopped" swap SIGNAL:DIE-OF ENDOF
      timeout OF s" signal-test: no SIGTERM taken" 3 die ENDOF
   ;MATCH ;

\ The throwing action runs before the registry's, which was added first.
: DIE-CHILD ( -- )
   SIGNAL:INIT
   SIGNAL:SIGTERM SIGNAL:CATCH
   DIE-PATH DIE-PATH-U @ >str FS:CLEANUP+
   [: E-FS-IO throw ;] AT-EXIT:ADD
   STOP-SELF ;

\ SUBJECT:RUN evaluates text with no package open, so the text reopens this
\ package to name DIE-CHILD.
: DIE-WAIT ( -- len len PROCESS:ending )
   s" package SIGNAL-TEST DIE-CHILD ;package"
   DIE-OUT DIE-CAP >LEN DIE-ERR DIE-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

: DIE-OF-CASE ( -- )
   s" signal-test" FS:MKTEMP-DIR {: root:str :}
   root STR:DATA DIE-ROOT root STR:LEN BYTE-COPY
   root STR:LEN DIE-ROOT-U !
   DIE-ROOT DIE-ROOT-U @ >str FS:CLEANUP-TREE+

   s" stopped" DIE-PATH DIE-FILE DIE-PATH-U !
   DIE-WAIT {: outu:len erru:len done :}
   DIE-ERR erru LEN>N >str {: err:str :}

   s" a child whose exit action throws still dies of the SIGTERM it caught" T-LABEL
   s" DIE-CHILD" DIE-OUT outu LEN>N >str err done SIGNAL:SIGTERM T-ENDING-SIGNALED=

   s" ... after its exit action's throw reached its stderr" T-LABEL
   err s" E-FS-IO" STR:CONTAINS? TTRUE

   s" ... once its exit actions removed the path it registered" T-LABEL
   DIE-PATH DIE-PATH-U @ >str FS:EXISTS? 0= TTRUE ;

\ RELEASE cleared the stub's bit, so a stub still installed sends the signal
\ on. SA_RESETHAND puts SIG_DFL back as the stub enters, so the child dies of
\ it: a stub that wrote it to the re-armed pipe would leave the child running.
: RESEND-CHILD ( -- )
   SIGNAL:INIT
   SIGNAL:SIGUSR1 SIGNAL:CATCH
   SIGNAL:RELEASE drop
   SIGNAL:INIT
   SIGNAL:SIGUSR1 SIGNAL-ABI:STUB SA-RESETHAND SET-DISPOSITION
   SIGNAL:SIGUSR1 RAISE ;

: RESEND-WAIT ( -- len len PROCESS:ending )
   s" package SIGNAL-TEST RESEND-CHILD ;package"
   DIE-OUT DIE-CAP >LEN DIE-ERR DIE-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

: RESEND-CASE ( -- )
   RESEND-WAIT {: outu:len erru:len done :}
   s" a stub RELEASE replaced sends its signal on to the default" T-LABEL
   s" RESEND-CHILD" DIE-OUT outu LEN>N >str DIE-ERR erru LEN>N >str done
   SIGNAL:SIGUSR1 T-ENDING-SIGNALED= ;

public

\ The loop is started after the last definition and stopped at the end; every
\ WAIT and PENDING? below runs on it.
: RUN ( -- )
   T-RESET
   AIO:START
   HOST-CASE
   STOP-CASE
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
   IGNORE-CASE
   FORK-CASE
   DIE-OF-CASE
   RESEND-CASE
   AIO:STOP
   T-REPORT ;

;package

SIGNAL-TEST:RUN
