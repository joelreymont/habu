\ signal.f - process signals delivered to a program on a self-pipe.
\ No Forth word is async-signal-safe: the handler is the engine's stub
\ (src/habu/crash.f EMIT-SIGNAL-HANDLER), which writes the four-byte signal
\ number to the descriptor the process-wide fd word holds.
\ PROCESS-WIDE: one pipe, one caught set and one armed fd word, owned by the task
\ that ran INIT (NEED-OWNER); the staging span WAIT and TAKE read into is per task.
\ The facility's cells are scratch: a built program starts unarmed and runs INIT.
\
\ THE MACHINE. The facility is OFF (READY 0: no pipe, the fd word 0, both sets
\ empty) or ON (READY 1). Within ON each signal is DEFAULT (no bit), STUBBED
\ (its CAUGHT and STUBBED bits) or IGNORED (its CAUGHT bit only).
\   OFF  INIT      -> ON         the pipe opened, then the fd word armed
\   ON   CATCH n   -> n STUBBED  the STUBBED bit set, then the stub installed
\   ON   IGNORE n  -> n IGNORED  SIG_IGN installed, then the STUBBED bit cleared
\   ON   RELEASE   -> OFF        defaults back, fd word 0, stubs waited out, pipe
\                                drained and closed
\   ON   fork      -> child OFF  CHILD-RESET: in-flight count 0, stubs back to
\                                default, fd word 0, pipe closed; ignores kept
\   OFF  fork      -> child OFF  CHILD-RESET: in-flight count 0
\ Any other input is E-SIGNAL-STATE: INIT when ON, every other word when OFF.
\ The stub runs in either state, on any thread, between a +1 and a -1 of the
\ in-flight count. Each transition keeps one invariant: while the stub is a
\ signal's disposition, its STUBBED bit is set and the fd word is nonzero, since
\ a stub that finds either clear sends the signal on, back to itself.

s" src/lib/error.f" required
s" src/lib/process.f" required                \ PROCESS:KILL-RAW, PROCESS:NO-FD, PROCESS:DEADLINE-AT and PROCESS:LEFT-MS
s" src/lib/fd.f" required                     \ FD:PIPE, the close-on-exec self-pipe, FD:NONBLOCK! and FD:POLLIN
s" src/lib/aio.f" required                    \ the WAIT window's POLL and AWAIT
s" src/lib/task.f" required                   \ TASK:+USER carries the staging span, TASK:SELF-N the owner
s" src/lib/ffi-abi.f" required                \ sigaction is declared here, not assumed
require src/habu/layout.f                 \ package SIGNAL-ABI publishes the two cells
require src/lib/fork-child.f                 \ CHILD-RESET, registered below
require src/lib/at-exit.f                    \ AT-EXIT:RUN-NOW, which DIE-OF runs

package SIGNAL
public

\ What one WAIT window answered: the number of a signal the stub delivered, or
\ that the window closed with nothing on the descriptor.
SUMTYPE signal-result
   VARIANT signal n ;VARIANT
   VARIANT timeout ;VARIANT
;SUMTYPE

\ SIGHUP, SIGINT and SIGTERM carry the same number on both hosts. SIGUSR1,
\ SIGUSR2, SIGSTOP and SIGCONT do not, so they are words with an arm per target:
\ one constant would name another signal on the other host, as Linux's SIGUSR1
\ is macOS's SIGBUS and Linux's SIGSTOP is macOS's SIGCONT.
1 constant SIGHUP
2 constant SIGINT
15 constant SIGTERM

: SIGUSR1 ( -- n )
   HB-TARGET-LINUX? if 10 else 30 then ;

: SIGUSR2 ( -- n )
   HB-TARGET-LINUX? if 12 else 31 then ;

: SIGSTOP ( -- n )
   HB-TARGET-LINUX? if 19 else 17 then ;

: SIGCONT ( -- n )
   HB-TARGET-LINUX? if 18 else 19 then ;

private

\ struct sigaction is not one record. glibc on both Linux ABIs lays out sa_handler at 0,
\ a 128-byte mask at 8, sa_flags at $88 and sa_restorer at $90, $98 bytes in
\ all; macOS lays out the handler at 0, a four-byte mask at 8 and sa_flags at
\ $0C, $10 bytes in all. Only the handler and the flags are ever spelled, so the
\ one value that has to follow the host is the flags offset. The buffers are the
\ larger record, which makes the cell write at either offset land inside them:
\ on glibc it covers the int and the padding after it and stops short of the
\ restorer, on macOS it covers the int and four bytes past a record the kernel
\ reads only $10 of.
$98 constant SA-LINUX-BYTES
$88 constant SA-LINUX-FLAGS-OFF
$10 constant SA-MACOS-BYTES
$0C constant SA-MACOS-FLAGS-OFF
SA-LINUX-BYTES SA-MACOS-BYTES max constant SA-BUF-BYTES

: SA-FLAGS-OFF ( -- n )
   HB-TARGET-LINUX? if
      SA-LINUX-FLAGS-OFF
   else
      SA-MACOS-FLAGS-OFF
   then ;

\ The same flag, spelled differently: a Linux $10000000 installed on macOS is
\ SA_SIGINFO|SA_NOCLDWAIT|SA_NODEFER and not SA_RESTART at all.
: SA-RESTART ( -- n )
   HB-TARGET-LINUX? if $10000000 else 2 then ;

0 constant SA-NO-FLAGS
0 constant SIG-DFL
1 constant SIG-IGN

4 constant SIGNO-BYTES             \ the width the stub writes, and under PIPE_BUF
1 constant SIG-MIN

\ The range sigaction installs on the wider of the two hosts. macOS stops at 31,
\ where a number between 32 and 64 passes this check and is rejected by sigaction
\ itself: E-SIGNAL-INSTALL rather than E-SIGNAL-NUMBER, named either way.
64 constant SIG-MAX

create SA-ACT SA-BUF-BYTES allot
create SA-OLD SA-BUF-BYTES allot   \ sigaction fills this; its prior content is dead

\ The two values kept from the main task's region, and the pipe they arm.
\ FD-WORD-A holds a POINTER and is declared as one: a raw `variable` read through
\ `0 ptr-field` is a raw base the pointer rule rejects, and PTR-VARIABLE gives
\ the slot the pointer type outright, for the cost of a bare cell load.
SCRATCH-VARIABLE STUB-A            \ the stub's runtime address, as sa_handler
PTR-VARIABLE FD-WORD-A             \ the ADDRESS of the process-wide fd word
SCRATCH-VARIABLE CAUGHT            \ bit (sig-1) per signal RELEASE restores
SCRATCH-VARIABLE READ-FD-CELL
SCRATCH-VARIABLE WRITE-FD-CELL
SCRATCH-VARIABLE OWNER             \ TASK:SELF-N at INIT; read only while READY
SCRATCH-VARIABLE READY             \ 1 from INIT to RELEASE, else 0

\ One staging span per task: WAIT reads the number into its own row.
8 TASK:+USERS SIGNO-STORAGE u8

PROCESS-SYMBOLS

FUNCTION: SIGACTION-CALL sigaction ( n ^u8 ^u8 -- i32 )
;FUNCTION

: ZERO-BYTES ( ^u8 n -- ) {: a:^ u :}
   0 begin dup u < while
      0 over a + c!
      1+
   repeat drop ;

: SIGNO-BUF ( -- ^u8 )
   0 SIGNO-STORAGE ;

: FD-WORD ( -- ^n )
   FD-WORD-A @ ;

\ Bit (sig-1) per signal CHILD-RESET resets, set before the stub goes in and
\ cleared after it is replaced: a stub that finds its bit clear is no longer the
\ disposition, and sends the signal on to the one in force. A rejected CATCH's
\ bit stays: sigaction rejects its reset too.
: STUBBED ( -- ^n )
   FD-WORD 1 cells + ;

\ The count of stubs running, on any thread.
: FLIGHT ( -- ^n )
   FD-WORD 2 cells + ;

: ARM ( n -- )
   FD-WORD ! ;

: DISARM ( -- )
   0 ARM ;

\ Both cells are read before either is dereferenced: a spawned task, and an
\ engine that bakes no stub, read zero in both.
: KEEP-ABI ( -- )
   SIGNAL-ABI:STUB 0= if E-SIGNAL-ABI throw then
   SIGNAL-ABI:FD NULL-PTR = if E-SIGNAL-ABI throw then
   SIGNAL-ABI:STUB STUB-A !
   SIGNAL-ABI:FD FD-WORD-A ! ;

: SIG-BIT ( n -- n ) {: sig:n :}
   1 sig 1 - lshift ;

: IN-SET? ( n n -- bool ) {: sig:n set:n :}
   sig SIG-BIT set and 0 <> ;

: CAUGHT+ ( n -- ) {: sig:n :}
   sig SIG-BIT CAUGHT @ or CAUGHT ! ;

: STUBBED+ ( n -- ) {: sig:n :}
   sig SIG-BIT STUBBED @ or STUBBED ! ;

: STUBBED- ( n -- ) {: sig:n :}
   sig SIG-BIT invert STUBBED @ and STUBBED ! ;

: SIG-CHECK ( n -- ) {: sig:n :}
   sig SIG-MIN < if E-SIGNAL-NUMBER throw then
   sig SIG-MAX > if E-SIGNAL-NUMBER throw then ;

: SA-HANDLER! ( n -- )
   SA-ACT cell-view ! ;

: SA-FLAGS! ( n -- )
   SA-ACT SA-FLAGS-OFF + cell-view ! ;

: SIGACTION-RC ( n -- n ) {: sig:n :}
   sig SA-ACT SA-OLD SIGACTION-CALL ;

: SIGACTION! ( n -- )
   SIGACTION-RC 0 <> if E-SIGNAL-INSTALL throw then ;

\ SA_RESTART so a read, write or wait a program is blocked in resumes after a
\ caught signal. The WAIT window does not need it: a signal reaches no thread
\ parked in poll(2). WAIT keeps a deadline for the lost race, not for -EINTR.
: INSTALL-STUB ( n -- ) {: sig:n :}
   SA-ACT SA-BUF-BYTES ZERO-BYTES
   STUB-A @ SA-HANDLER!
   SA-RESTART SA-FLAGS!
   sig SIGACTION! ;

\ SIG-DFL or SIG-IGN: an action the kernel takes itself, so no flags.
: ACTION! ( n -- )
   SA-ACT SA-BUF-BYTES ZERO-BYTES
   SA-HANDLER!
   SA-NO-FLAGS SA-FLAGS! ;

: INSTALL-ACTION ( n n -- ) {: sig:n action:n :}
   action ACTION!
   sig SIGACTION! ;

: EACH-IN ( n [ n -- ] -- ) {: set:n q :}
   SIG-MAX 1+ SIG-MIN ?do
      i set IN-SET? if i q execute then
   loop ;

: RESTORE-ALL ( -- )
   CAUGHT @ [: SIG-DFL INSTALL-ACTION ;] EACH-IN
   0 CAUGHT !
   0 STUBBED ! ;

: READ-FD ( -- fd )
   READ-FD-CELL @ >FD ;

: MARK-PIPE ( fd fd -- fd fd )
   over FD:NONBLOCK! dup FD:NONBLOCK! ;

\ Both ends are non-blocking: a full pipe rejects the stub's write rather than park
\ the signalled thread, and a read that lost the bytes to another task is rejected.
: OPEN-PIPE ( -- )
   FD:PIPE {: r:fd w:fd :}
   r w [: MARK-PIPE ;] catch {: code:n :}
   2drop
   code 0<> if r FD>N close w FD>N close code throw then
   r FD>N READ-FD-CELL !
   w FD>N WRITE-FD-CELL ! ;

: FORGET-FDS ( -- )
   PROCESS:NO-FD READ-FD-CELL !
   PROCESS:NO-FD WRITE-FD-CELL ! ;

\ A forked child keeps the stubs, the three words and both pipe ends, so it
\ would write a stop signal into the parent's pipe and live on through it. Its
\ count can hold a stub another thread was running at the fork, released or not.
\ The stubs go before DISARM: a signal between them would find its bit set and
\ the fd word 0 and send itself back to the stub.
: CHILD-RESET ( -- )
   FD-WORD-A @ NULL-PTR <> if 0 FLIGHT ! then
   READY @ 0= if exit then
   STUBBED @ [: SIG-DFL ACTION! SIGACTION-RC drop ;] EACH-IN
   0 STUBBED !
   DISARM
   READ-FD-CELL @ close
   WRITE-FD-CELL @ close
   FORGET-FDS
   0 CAUGHT !
   0 READY ! ;

: REGISTER-RESET ( -- )
   [: CHILD-RESET ;] FORK-CHILD:REGISTER ;
REGISTER-RESET

\ Both ends are closed before either failure is named, so a rejected close never
\ leaks the other end.
: CLOSE-PIPE ( n n -- ) {: r:n w:n :}
   r close-rc {: rrc:n :}
   w close-rc {: wrc:n :}
   rrc 0 <> wrc 0 <> or if E-SIGNAL-CLOSE throw then ;

\ One window of the read end, waited out in the task's own AIO queue: a POLL
\ with ms as its own deadline, and an AWAIT. A signal does not cut the window
\ short, so there is no -EINTR to restart.
\
\ A rejected read is only ever the lost race when POLLIN put the bytes there in
\ the first place, so the answered bits are read and not just counted: a ready
\ answer without POLLIN (POLLHUP, every write end closed) and a rejected POLL
\ (EBADF, the read end closed) would otherwise be re-read against the deadline
\ forever, so both name the poll.
: READABLE-WITHIN? ( ms -- bool ) {: window:ms :}
   READ-FD AIO:READABLE window AIO:WAIT-READY
   MATCH AIO:readiness
      ready OF FD:POLLIN and 0= if E-SIGNAL-POLL throw then true ENDOF
      timed-out OF false ENDOF
      rejected OF drop E-SIGNAL-POLL throw ENDOF
   ;MATCH ;

\ Four bytes is under PIPE_BUF, so the pipe takes the number whole or not at
\ all: a short read is a torn number the stub guarantees against, not a partial
\ delivery to be resumed.
\
\ A REJECTED read is an empty pipe. The engine collapses every failed syscall but
\ poll to a bare -1 (src/habu/habu1.f, "THE ERRNO RULE FOR THIS FILE'S SYSCALL
\ WRAPPERS"), so there is no errno here to branch on - and on a non-blocking
\ descriptor this facility owns, reading four bytes into its own span with a
\ write end held open for the facility's life, EAGAIN is the only rejection the
\ kernel has left. Under WAIT, the bytes poll saw went to another task. EINTR
\ cannot appear: a read of a non-blocking pipe never sleeps.
-1 constant SIGNO-REJECTED

: SIGNO@ ( -- n )
   SIGNO-BUF CELL ZERO-BYTES
   READ-FD FD>N SIGNO-BUF SIGNO-BYTES SPAN:MAKE read {: got:n :}
   got 0 < if SIGNO-REJECTED exit then
   got SIGNO-BYTES <> if E-SIGNAL-READ throw then
   SIGNO-BUF cell-view @ ;

\ Every number left on the read end, keeping the last; 0 when it holds none.
\ The read end is non-blocking, so the read that finds it empty is rejected.
: DRAIN ( -- n )
   0 begin SIGNO@ dup SIGNO-REJECTED <> while nip repeat drop ;

: NEED-READY ( -- )
   READY @ 0= if E-SIGNAL-STATE throw then ;

\ CATCH, IGNORE, IGNORED? and RELEASE share SA-ACT, SA-OLD and the caught set,
\ so the facility has one owner, the task that ran INIT. FD, PENDING?, WAIT and
\ TAKE touch none of the three and are callable from any task.
: NEED-OWNER ( -- )
   TASK:SELF-N OWNER @ <> if E-SIGNAL-STATE throw then ;

public

\ From the MAIN task: the two published cells are read once here and kept.
\ The fd word is armed last, once the pipe it names is whole.
: INIT ( -- )
   READY @ 0<> if E-SIGNAL-STATE throw then
   KEEP-ABI
   OPEN-PIPE
   0 CAUGHT !
   TASK:SELF-N OWNER !
   1 READY !
   WRITE-FD-CELL @ ARM ;

\ Deliver signal n to the descriptor instead of its default disposition.
: CATCH ( n -- ) {: sig:n :}
   NEED-READY
   NEED-OWNER
   sig SIG-CHECK
   sig STUBBED+
   sig INSTALL-STUB
   sig CAUGHT+ ;

\ Discard signal n: the kernel drops it and nothing reaches the descriptor. A
\ later CATCH replaces the ignore, and RELEASE restores the default as it does
\ for a caught signal. Unlike a caught signal, which exec resets to its
\ default, an ignored one stays ignored in a child the process starts.
: IGNORE ( n -- ) {: sig:n :}
   NEED-READY
   NEED-OWNER
   sig SIG-CHECK
   sig SIG-IGN INSTALL-ACTION
   sig STUBBED-
   sig CAUGHT+ ;

\ TRUE when signal n is ignored. A process starts with its caller's ignored
\ signals still ignored (nohup's SIGHUP, the SIGINT of a background job under a
\ shell without job control), and a CATCH over one undoes that choice, so a
\ program that means to honour it asks first.
: IGNORED? ( n -- bool ) {: sig:n :}
   NEED-READY
   NEED-OWNER
   sig SIG-CHECK
   sig NULL-PTR SA-OLD SIGACTION-CALL 0 <> if E-SIGNAL-INSTALL throw then
   SA-OLD cell-view @ SIG-IGN = ;

\ The read end, for a program that polls it beside its sockets.
: FD ( -- fd )
   NEED-READY
   READ-FD ;

\ The next delivered signal, or `timeout` at once when none is waiting. This
\ is the read for a program that waits in a loop of its own - FD in its poll
\ set, or a poll that a signal interrupts - and so needs no AIO.
: TAKE ( -- signal-result )
   NEED-READY
   SIGNO@ dup SIGNO-REJECTED = if
      drop SIGNAL-SIGNAL--RESULT:TIMEOUT exit
   then
   SIGNAL-SIGNAL--RESULT:SIGNAL ;

\ Readable now, without consuming: a zero-length window, which the kernel serves
\ inline for a descriptor that is already ready. "Ready" is not "readable": a
\ read end that is gone is the rejection READABLE-WITHIN? names, never a true that
\ would send a caller into the WAIT that names it.
: PENDING? ( -- bool )
   NEED-READY
   0 >MS READABLE-WITHIN? ;

\ The next signal number, or `timeout` once ms has passed; a negative ms waits
\ unbounded.
: WAIT ( ms -- signal-result ) {: window:ms :}
   NEED-READY
   window PROCESS:DEADLINE-AT {: deadline :}
   begin
      window MS>N 0 < if window else deadline PROCESS:LEFT-MS then
      READABLE-WITHIN? 0= if SIGNAL-SIGNAL--RESULT:TIMEOUT exit then
      SIGNO@ dup SIGNO-REJECTED <> if SIGNAL-SIGNAL--RESULT:SIGNAL exit then
      drop
   again ;

\ No stub that may write runs once this returns. The fence orders DISARM before the reads of
\ the count, as the stub's locked add orders its reads after its count.
: AWAIT-STUBS ( -- )
   fence
   begin FLIGHT atomic@ 0<> while TASK:PAUSE repeat ;

\ The last caught signal not yet taken, or 0. Restoring before DISARM and
\ waiting out the stubs after it sends each signal to the pipe before DRAIN or
\ to its default, and leaves no stub to write after the close.
: RELEASE ( -- n )
   NEED-READY
   NEED-OWNER
   RESTORE-ALL
   DISARM
   AWAIT-STUBS
   DRAIN {: last:n :}
   READ-FD-CELL @ WRITE-FD-CELL @ {: r:n w:n :}
   FORGET-FDS
   0 READY !
   r w CLOSE-PIPE
   last ;

\ SIGTERM, SIGINT and SIGHUP ask a process to stop. Under the default action a
\ program that runs children or makes scratch ends where it stands, and what it
\ started and made outlives it: a spawned child that is not REAPED leads a
\ process group of its own (src/lib/process.f) and runs on under init, and
\ nothing removes the scratch. Such a program calls CATCH-STOPS before it
\ starts either, asks TAKE wherever it waits, and answers a signal by ending
\ what it started, then DIE-OF, which runs the exit actions and so removes the
\ paths registered for removal (src/lib/fs-mutate.f). The gate root
\ (test/gate-pool.f), the pg cluster (test/db/pg-cluster.f) and check.f
\ (tools/check-core.f) answer so.
\
\ CATCH-STOPS leaves a signal the process was started with ignored as it is
\ (IGNORED?).

private

: CATCH-STOP ( n -- ) {: sig:n :}
   sig IGNORED? if exit then
   sig CATCH ;

public

: CATCH-STOPS ( -- )
   INIT
   SIGTERM CATCH-STOP
   SIGINT CATCH-STOP
   SIGHUP CATCH-STOP ;

\ The end of an answer: the exit actions run while the stub still absorbs a
\ second signal, then the process dies OF the signal, as an uncaught one ends it.
: DIE-OF ( str n -- ) {: msg:str sig:n :}
   AT-EXIT:RUN-NOW
   RELEASE drop
   getpid >PID sig PROCESS:KILL-RAW drop
   \ Reached only when another thread took the signal: a shell's status for it.
   msg 128 sig + die ;

;package
