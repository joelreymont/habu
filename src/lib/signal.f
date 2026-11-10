\ signal.f - process signals delivered to a program on a self-pipe.
\ No Forth word is async-signal-safe: the handler is the engine's stub
\ (src/habu/crash.f EMIT-SIGNAL-HANDLER), which writes the four-byte signal
\ number to the descriptor the process-wide fd word holds.
\ PROCESS-WIDE: one pipe, one caught set and one armed fd word, owned by the task
\ that ran INIT (NEED-OWNER); the staging span WAIT and TAKE read into is per task.
\ The facility's cells are scratch: a built program starts unarmed and runs INIT.

s" src/lib/error.f" required
s" src/lib/process.f" required                \ PROCESS:KILL-RAW, PROCESS:NO-FD, PROCESS:DEADLINE-AT and PROCESS:LEFT-MS
s" src/lib/fd.f" required                     \ FD:PIPE, the close-on-exec self-pipe, FD:NONBLOCK! and FD:POLLIN
s" src/lib/aio.f" required                    \ the loop the WAIT window runs on
s" src/lib/task.f" required                   \ TASK:+USER carries the staging span, TASK:SELF-N the owner
s" src/lib/ffi-abi.f" required                \ sigaction is declared here, not assumed
require src/habu/layout.f                 \ package SIGNAL-ABI publishes the two cells

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
   HB-TARGET-LINUX-KERNEL? if 10 exit then
   HB-TARGET-MACOS? if 30 exit then
   E-PROC-HOST throw ;

: SIGUSR2 ( -- n )
   HB-TARGET-LINUX-KERNEL? if 12 exit then
   HB-TARGET-MACOS? if 31 exit then
   E-PROC-HOST throw ;

: SIGSTOP ( -- n )
   HB-TARGET-LINUX-KERNEL? if 19 exit then
   HB-TARGET-MACOS? if 17 exit then
   E-PROC-HOST throw ;

: SIGCONT ( -- n )
   HB-TARGET-LINUX-KERNEL? if 18 exit then
   HB-TARGET-MACOS? if 19 exit then
   E-PROC-HOST throw ;

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
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if
      SA-LINUX-FLAGS-OFF exit
   then
   HB-TARGET-MACOS? if SA-MACOS-FLAGS-OFF exit then
   E-PROC-HOST throw ;

\ The same flag, spelled differently: a Linux $10000000 installed on macOS is
\ SA_SIGINFO|SA_NOCLDWAIT|SA_NODEFER and not SA_RESTART at all.
: SA-RESTART ( -- n )
   HB-TARGET-LINUX-KERNEL? if $10000000 exit then
   HB-TARGET-MACOS? if 2 exit then
   E-PROC-HOST throw ;

0 constant SA-NO-FLAGS
0 constant SIG-DFL
1 constant SIG-IGN

4 constant SIGNO-BYTES             \ the width the stub writes, and under PIPE_BUF
1 constant SIG-MIN

\ The range sigaction installs on the wider of the two hosts. macOS stops at 31,
\ where a number between 32 and 64 passes this check and is refused by sigaction
\ itself: E-SIGNAL-INSTALL rather than E-SIGNAL-NUMBER, named either way.
64 constant SIG-MAX

create SA-ACT SA-BUF-BYTES allot
create SA-OLD SA-BUF-BYTES allot   \ sigaction fills this; its prior content is dead

\ The two values kept from the main task's region, and the pipe they arm.
\ FD-WORD-A holds a POINTER and is declared as one: a raw `variable` read through
\ `0 ptr-field` is a raw base the pointer rule refuses, and PTR-VARIABLE gives
\ the slot the pointer type outright, for the cost of a bare cell load.
SCRATCH-VARIABLE STUB-A            \ the stub's runtime address, as sa_handler
PTR-VARIABLE FD-WORD-A             \ the ADDRESS of the process-wide fd word
SCRATCH-VARIABLE CAUGHT            \ bit (sig-1) per signal RELEASE restores
SCRATCH-VARIABLE READ-FD-CELL
SCRATCH-VARIABLE WRITE-FD-CELL
SCRATCH-VARIABLE OWNER             \ TASK:SELF-N at INIT; read only while READY
SCRATCH-VARIABLE READY             \ 1 from INIT to RELEASE, else 0

\ One staging span per task: WAIT reads the number into its own row.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and CELL TASK:+USER SIGNO-STORAGE drop

PROCESS-SYMBOLS

FUNCTION: SIGACTION-CALL sigaction ( n ptr u8 ptr u8 -- i32 )
   2 SA-BUF-BYTES WRITES-BYTES
;FUNCTION

: ZERO-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      0 over a + c!
      1+
   repeat drop ;

: SIGNO-BUF ( -- ptr u8 )
   SIGNO-STORAGE BYTE-VIEW ;

: ABI-SLOT ( n -- ptr n ) {: off:n :}
   data-base off + ;

: STUB-CELL@ ( -- n )
   SIGNAL-ABI:STUB-CELL ABI-SLOT @ ;

: FD-PTR-CELL@ ( -- n )
   SIGNAL-ABI:FD-PTR-CELL ABI-SLOT @ ;

\ The published pointer read AS a pointer. Reaching the fd word by an offset of
\ this task's own region instead would name a different word in every task and
\ a zero one in a spawned task; the address is what makes it one word.
: FD-WORD-PUBLISHED ( -- ptr n )
   SIGNAL-ABI:FD-PTR-CELL ABI-SLOT 0 ptr-field @ ;

: FD-WORD ( -- ptr n )
   FD-WORD-A @ ;

: ARM ( n -- )
   FD-WORD ! ;

: DISARM ( -- )
   0 ARM ;

\ Both cells are read before either is dereferenced: a spawned task, and an
\ engine that bakes no stub, read zero in both.
: KEEP-ABI ( -- )
   STUB-CELL@ 0= if E-SIGNAL-ABI throw then
   FD-PTR-CELL@ 0= if E-SIGNAL-ABI throw then
   STUB-CELL@ STUB-A !
   FD-WORD-PUBLISHED FD-WORD-A ! ;

: SIG-BIT ( n -- n ) {: sig:n :}
   1 sig 1 - lshift ;

: CAUGHT? ( n -- bool ) {: sig:n :}
   sig SIG-BIT CAUGHT @ and 0 <> ;

: CAUGHT+ ( n -- ) {: sig:n :}
   sig SIG-BIT CAUGHT @ or CAUGHT ! ;

: SIG-CHECK ( n -- ) {: sig:n :}
   sig SIG-MIN < if E-SIGNAL-NUMBER throw then
   sig SIG-MAX > if E-SIGNAL-NUMBER throw then ;

: SA-HANDLER! ( n -- )
   SA-ACT cell-view ! ;

: SA-FLAGS! ( n -- )
   SA-ACT SA-FLAGS-OFF + cell-view ! ;

: SIGACTION! ( n -- ) {: sig:n :}
   sig SA-ACT SA-OLD SIGACTION-CALL 0 <> if E-SIGNAL-INSTALL throw then ;

\ SA_RESTART so a read, write or wait a program is blocked in resumes after a
\ caught signal. The WAIT window does not need it: a signal reaches no thread
\ parked in poll(2). WAIT keeps a deadline for the lost race, not for -EINTR.
: INSTALL-STUB ( n -- ) {: sig:n :}
   SA-ACT SA-BUF-BYTES ZERO-BYTES
   STUB-A @ SA-HANDLER!
   SA-RESTART SA-FLAGS!
   sig SIGACTION! ;

\ SIG-DFL or SIG-IGN: an action the kernel takes itself, so no flags.
: INSTALL-ACTION ( n n -- ) {: sig:n action:n :}
   SA-ACT SA-BUF-BYTES ZERO-BYTES
   action SA-HANDLER!
   SA-NO-FLAGS SA-FLAGS!
   sig SIGACTION! ;

: RESTORE-ONE ( n -- ) {: sig:n :}
   sig CAUGHT? if sig SIG-DFL INSTALL-ACTION then ;

: RESTORE-ALL ( -- )
   SIG-MAX 1+ SIG-MIN ?do i RESTORE-ONE loop
   0 CAUGHT ! ;

: READ-FD ( -- fd )
   READ-FD-CELL @ >FD ;

: MARK-PIPE ( fd fd -- fd fd )
   over FD:NONBLOCK! dup FD:NONBLOCK! ;

\ Both ends are non-blocking: a full pipe refuses the stub's write rather than park
\ the signalled thread, and a read that lost the bytes to another task is refused.
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

\ Both ends are closed before either failure is named, so a refused close never
\ leaks the other end.
: CLOSE-PIPE ( n n -- ) {: r:n w:n :}
   r close-rc {: rrc:n :}
   w close-rc {: wrc:n :}
   rrc 0 <> wrc 0 <> or if E-SIGNAL-CLOSE throw then ;

\ One window of the read end, waited out on the AIO loop: a POLL for ms with
\ the deadline as the poll's own linked timeout, and an AWAIT. A signal reaches
\ no thread parked here, so there is no -EINTR to restart.
\
\ A refused read is only ever the lost race when POLLIN put the bytes there in
\ the first place, so the answered bits are read and not just counted: POLLNVAL,
\ and a bare POLLERR, are a read end gone behind the facility's back and would
\ otherwise be re-read against the deadline forever. The kernel answers that
\ state two ways - as those bits when it still has the descriptor, and as a
\ refused poll (EBADF) when the number no longer names one - and both name the
\ poll. `cancelled` cannot arrive: nothing here cancels, the ticket never leaves
\ this word, and the cleanup AIO registers on a submitting task runs only after
\ that task has ended.
: READABLE-WITHIN? ( ms -- bool ) {: window:ms :}
   READ-FD AIO:READABLE window AIO:POLL AIO:AWAIT
   MATCH AIO:outcome
      ready OF FD:POLLIN and 0= if E-SIGNAL-POLL throw then true ENDOF
      timed-out OF false ENDOF
      cancelled OF E-SIGNAL-POLL throw ENDOF
      refused OF drop E-SIGNAL-POLL throw ENDOF
   ;MATCH ;

\ Four bytes is under PIPE_BUF, so the pipe takes the number whole or not at
\ all: a short read is a torn number the stub guarantees against, not a partial
\ delivery to be resumed.
\
\ A REFUSED read is the lost race. The engine collapses every failed syscall but
\ poll to a bare -1 (src/habu/habu1.f, "THE ERRNO RULE FOR THIS FILE'S SYSCALL
\ WRAPPERS"), so there is no errno here to branch on - and on a non-blocking
\ descriptor this facility owns, reading four bytes into its own span with a
\ write end held open for the facility's life, EAGAIN is the only refusal the
\ kernel has left: the bytes poll saw went to another task in between. EINTR
\ cannot appear, because read is one of the calls SA_RESTART restarts.
-1 constant SIGNO-REFUSED

: SIGNO@ ( -- n )
   SIGNO-BUF CELL ZERO-BYTES
   READ-FD FD>N SIGNO-BUF SIGNO-BYTES read {: got:n :}
   got 0 < if SIGNO-REFUSED exit then
   got SIGNO-BYTES <> if E-SIGNAL-READ throw then
   SIGNO-BUF cell-view @ ;

: NEED-READY ( -- )
   READY @ 0= if E-SIGNAL-STATE throw then ;

\ CATCH, IGNORE, IGNORED? and RELEASE share SA-ACT, SA-OLD and the caught set,
\ so the facility has one owner, the task that ran INIT. FD, PENDING?, WAIT and
\ TAKE touch none of the three and are callable from any task.
: NEED-OWNER ( -- )
   TASK:SELF-N OWNER @ <> if E-SIGNAL-STATE throw then ;

\ Refused up front, before a pipe is opened or a disposition touched: on a host
\ neither arm above knows, every one of this file's numbers would be somebody
\ else's, and installing the stub for whatever 10 means there is exactly the
\ silent mis-install the arms exist to prevent.
: NEED-HOST ( -- )
   HB-TARGET-LINUX-KERNEL? if exit then
   HB-TARGET-MACOS? if exit then
   E-PROC-HOST throw ;

public

\ From the MAIN task: the two published cells are read once here and kept.
\ The fd word is armed last, so a signal that arrives while INIT is opening the
\ pipe is absorbed rather than written to a half-built facility.
: INIT ( -- )
   READY @ 0<> if E-SIGNAL-STATE throw then
   NEED-HOST
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
\ set, or a poll that a signal interrupts - and so runs no AIO loop.
: TAKE ( -- signal-result )
   NEED-READY
   SIGNO@ dup SIGNO-REFUSED = if
      drop SIGNAL-SIGNAL--RESULT:timeout exit
   then
   SIGNAL-SIGNAL--RESULT:signal ;

\ Readable now, without consuming: a zero-length window, which the kernel serves
\ inline for a descriptor that is already ready. "Ready" is not "readable": a
\ read end that is gone is the refusal READABLE-WITHIN? names, never a true that
\ would send a caller into the WAIT that names it.
: PENDING? ( -- bool )
   NEED-READY
   0 >MS READABLE-WITHIN? ;

\ The next signal number, or `timeout` once ms has passed; a negative ms waits
\ unbounded. Like PENDING?, it needs AIO:START or throws E-AIO-STATE.
: WAIT ( ms -- signal-result ) {: window:ms :}
   NEED-READY
   window PROCESS:DEADLINE-AT {: deadline :}
   begin
      window MS>N 0 < if window else deadline PROCESS:LEFT-MS then
      READABLE-WITHIN? 0= if SIGNAL-SIGNAL--RESULT:timeout exit then
      SIGNO@ dup SIGNO-REFUSED <> if SIGNAL-SIGNAL--RESULT:signal exit then
      drop
   again ;

\ The fd word is cleared FIRST, so a signal delivered during the restore is
\ absorbed instead of written to a descriptor that is about to close.
: RELEASE ( -- )
   NEED-READY
   NEED-OWNER
   DISARM
   RESTORE-ALL
   READ-FD-CELL @ WRITE-FD-CELL @ {: r:n w:n :}
   FORGET-FDS
   0 READY !
   r w CLOSE-PIPE ;

\ SIGTERM, SIGINT and SIGHUP ask a process to stop. Under the default action a
\ program that runs children or makes scratch ends where it stands, and what it
\ started and made outlives it: a spawned child leads a process group of its
\ own (src/lib/process.f) and runs on under init, and nothing removes the
\ scratch. Such a program calls CATCH-STOPS before it starts either, asks TAKE
\ wherever it waits, and answers a signal by ending what it started and
\ removing what it made, then DIE-OF. The gate root (test/gate-pool.f), the pg
\ cluster (test/db/pg-cluster.f) and check.f (tools/check-core.f) answer so.
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

\ The end of an answer: the process dies OF the signal, under the default
\ action again, so its caller reads the status an uncaught one would have left.
\ The stub stays installed until here, so a second signal during the answer is
\ written to the pipe and read by nobody. The die ends a process whose signal
\ another thread took, with the status a shell gives that death; it is not
\ reached when this one took it.
: DIE-OF ( ptr u8 n n -- ) {: msg:ptr msgu:n sig:n :}
   RELEASE
   getpid >PID sig PROCESS:KILL-RAW drop
   msg msgu 128 sig + die ;

;package
