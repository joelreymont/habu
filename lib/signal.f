\ signal.f - process signals delivered to a program on a self-pipe.
\
\ No Forth word is async-signal-safe, so the only code the kernel runs in a
\ handler is the stub the engine bakes (src/habu/crash.f EMIT-SIGNAL-HANDLER):
\ it writes the four-byte signal number to whatever descriptor the process-wide
\ fd word holds, and a program reads it in ordinary checked code.
\
\ STORAGE CLASS. PROCESS-WIDE. One pipe, one caught set and one armed fd word
\ for the whole process. INIT reads the engine's two published SIGNAL-ABI cells
\ in the MAIN task - a spawned task's region carries neither - and keeps them,
\ so every later arm and disarm reaches the same word by its absolute address.
\ INIT also records the task that ran it: CATCH and RELEASE share SA-ACT, SA-OLD
\ and the caught set, so they answer to that task alone. FD, PENDING? and WAIT
\ touch none of the three and are callable from any task; the four-byte staging
\ span WAIT reads into is a TASK:+USER row, so two tasks polling the read end
\ never share it. See docs/signal.md.
\
\ HOSTS. Linux and macOS, on aarch64. The signal numbers, SA_RESTART and the
\ struct sigaction layout all differ between the two and are selected here the
\ way lib/process.f O-NONBLOCK, lib/fs.f and lib/process-pty-io.f select theirs.
\ A third target reaches no arm and INIT refuses it with E-PROC-HOST rather than
\ installing the stub for whatever signal the Linux numbers happen to name
\ there. macOS is selected for but untested: no macOS host runs this suite.

s" lib/errors.f" required
s" lib/process.f" required                \ the pipe words, POLLIN, PROC-NO-FD, close-rc and the two deadline helpers
s" lib/aio.f" required                    \ the loop the WAIT window runs on
s" lib/task.f" required                   \ TASK:+USER carries the staging span, TASK:SELF-N the owner
s" lib/ffi-abi.f" required                \ sigaction is declared here, not assumed
s" lib/image-lifecycle.f" required        \ a restored image is a different process
require src/habu/layout.f                 \ package SIGNAL-ABI publishes the two cells

package SIGNAL
public

\ What one WAIT window answered: the number of a signal the stub delivered, or
\ that the window closed with nothing on the descriptor.
SUMTYPE signal-result 0
   VARIANT signal n ;VARIANT
   VARIANT timeout ;VARIANT
;SUMTYPE

\ SIGHUP, SIGINT and SIGTERM carry the same number on both hosts. SIGUSR1 and
\ SIGUSR2 do not, so they are words with an arm per target rather than one
\ constant that would name SIGBUS and SIGSYS on macOS.
1 constant SIGHUP
2 constant SIGINT
15 constant SIGTERM

: SIGUSR1 ( -- n )
   HB-TARGET-LINUX? if 10 exit then
   HB-TARGET-MACOS? if 30 exit then
   E-PROC-HOST throw ;

: SIGUSR2 ( -- n )
   HB-TARGET-LINUX? if 12 exit then
   HB-TARGET-MACOS? if 31 exit then
   E-PROC-HOST throw ;

private

\ struct sigaction is not one record. glibc on aarch64 lays out sa_handler at 0,
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
   HB-TARGET-LINUX? if SA-LINUX-FLAGS-OFF exit then
   HB-TARGET-MACOS? if SA-MACOS-FLAGS-OFF exit then
   E-PROC-HOST throw ;

\ The same flag, spelled differently: a Linux $10000000 installed on macOS is
\ SA_SIGINFO|SA_NOCLDWAIT|SA_NODEFER and not SA_RESTART at all.
: SA-RESTART ( -- n )
   HB-TARGET-LINUX? if $10000000 exit then
   HB-TARGET-MACOS? if 2 exit then
   E-PROC-HOST throw ;

0 constant SA-NO-FLAGS
0 constant SIG-DFL

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
variable STUB-A                    \ the stub's runtime address, as sa_handler
PTR-VARIABLE FD-WORD-A             \ the ADDRESS of the process-wide fd word
variable CAUGHT                    \ bit (sig-1) for every signal CATCH installed
variable READ-FD-CELL
variable WRITE-FD-CELL
variable OWNER                     \ TASK:SELF-N at INIT; read only while READY

TYPED-VARIABLE READY bool
false READY !

TYPED-VARIABLE REGISTERED bool
false REGISTERED !

\ One staging span per task: WAIT reads the number into its own row.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and CELL TASK:+USER SIGNO-STORAGE drop

PROCESS-SYMBOLS

FUNCTION: SIGACTION-CALL sigaction ( n ptr u8 ptr u8 -- n )
   2 SA-BUF-BYTES WRITES-BYTES
;FUNCTION

: ZERO-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      0 over a + c!
      1+
   repeat drop ;

: SIGNO-BUF ( -- ptr u8 )
   SIGNO-STORAGE BYTE-VIEW ;

\ ---- the engine's two published cells ---------------------------------------

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

\ ---- the caught set ----------------------------------------------------------

: SIG-BIT ( n -- n ) {: sig:n :}
   1 sig 1 - lshift ;

: CAUGHT? ( n -- bool ) {: sig:n :}
   sig SIG-BIT CAUGHT @ and 0 <> ;

: CAUGHT+ ( n -- ) {: sig:n :}
   sig SIG-BIT CAUGHT @ or CAUGHT ! ;

: SIG-CHECK ( n -- ) {: sig:n :}
   sig SIG-MIN < if E-SIGNAL-NUMBER throw then
   sig SIG-MAX > if E-SIGNAL-NUMBER throw then ;

\ ---- installing and restoring a disposition ---------------------------------

: SA-HANDLER! ( n -- )
   SA-ACT cell-view ! ;

: SA-FLAGS! ( n -- )
   SA-ACT SA-FLAGS-OFF + cell-view ! ;

: SIGACTION! ( n -- ) {: sig:n :}
   sig SA-ACT SA-OLD SIGACTION-CALL 0 <> if E-SIGNAL-INSTALL throw then ;

\ SA_RESTART so the read, write and wait calls a program is blocked in resume
\ after a caught signal. The WAIT window no longer needs it: the wait is a
\ POLL-ADD on the AIO loop, and a signal reaches no thread parked in poll(2).
\ WAIT still owns a deadline, for the lost race rather than for -EINTR.
: INSTALL-STUB ( n -- ) {: sig:n :}
   SA-ACT SA-BUF-BYTES ZERO-BYTES
   STUB-A @ SA-HANDLER!
   SA-RESTART SA-FLAGS!
   sig SIGACTION! ;

: INSTALL-DEFAULT ( n -- ) {: sig:n :}
   SA-ACT SA-BUF-BYTES ZERO-BYTES
   SIG-DFL SA-HANDLER!
   SA-NO-FLAGS SA-FLAGS!
   sig SIGACTION! ;

: RESTORE-ONE ( n -- ) {: sig:n :}
   sig CAUGHT? if sig INSTALL-DEFAULT then ;

: RESTORE-ALL ( -- )
   SIG-MAX 1+ SIG-MIN ?do i RESTORE-ONE loop
   0 CAUGHT ! ;

\ ---- the descriptors ---------------------------------------------------------

: READ-FD ( -- fd )
   READ-FD-CELL @ >FD ;

\ BOTH ends are non-blocking. The write end so a full pipe refuses the stub's
\ write instead of parking whichever thread the signal landed on; the read end
\ so a WAIT that loses the four bytes to another task is refused instead of
\ parked in read until the NEXT signal arrives - which is a hang, not a race,
\ for a window that was meant to close on its own.
: OPEN-PIPE ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r FD-CLOEXEC!
   w FD-CLOEXEC!
   r PROC-NONBLOCK!
   w PROC-NONBLOCK!
   r FD>N READ-FD-CELL !
   w FD>N WRITE-FD-CELL ! ;

: FORGET-FDS ( -- )
   PROC-NO-FD READ-FD-CELL !
   PROC-NO-FD WRITE-FD-CELL ! ;

\ Both ends are closed before either failure is named, so a refused close never
\ leaks the other end.
: CLOSE-PIPE ( n n -- ) {: r:n w:n :}
   r close-rc {: rrc:n :}
   w close-rc {: wrc:n :}
   rrc 0 <> wrc 0 <> or if E-SIGNAL-CLOSE throw then ;

\ ---- waiting on the read end -------------------------------------------------

\ One window of the read end, waited out on the AIO loop: a POLL-ADD for ms with
\ the deadline as the poll's own linked timeout, and an AWAIT. A signal reaches
\ no thread parked here, so there is no -EINTR to restart.
\
\ A refused read is only ever the lost race when POLLIN put the bytes there in
\ the first place, so the answered bits are read and not just counted: POLLNVAL,
\ and a bare POLLERR, are a read end gone behind the facility's back and would
\ otherwise be re-read against the deadline forever. The kernel answers that
\ state two ways now - as those bits when it still has the descriptor, and as a
\ refused poll (EBADF) when the number no longer names one - and both name the
\ poll. `cancelled` cannot arrive: nothing here cancels, the ticket never leaves
\ this word, and the cleanup AIO registers on a submitting task runs only after
\ that task has ended.
: READABLE-WITHIN? ( ms -- bool ) {: window:ms :}
   READ-FD AIO:READABLE window AIO:POLL-ADD AIO:AWAIT
   MATCH AIO:outcome
      ready OF POLLIN and 0= if E-SIGNAL-POLL throw then true ENDOF
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

\ CATCH and RELEASE share SA-ACT, SA-OLD and the caught set, so the facility has
\ one owner - the task that ran INIT - rather than a documented convention two
\ tasks could each believe they were keeping.
: NEED-OWNER ( -- )
   TASK:SELF-N OWNER @ <> if E-SIGNAL-STATE throw then ;

\ Refused up front, before a pipe is opened or a disposition touched: on a host
\ neither arm above knows, every one of this file's numbers would be somebody
\ else's, and installing the stub for whatever 10 means there is exactly the
\ silent mis-install the arms exist to prevent.
: NEED-HOST ( -- )
   HB-TARGET-LINUX? if exit then
   HB-TARGET-MACOS? if exit then
   E-PROC-HOST throw ;

\ A restored image is a different process: its descriptors and its sigaction
\ table are gone and the engine's boot has already cleared the fd word, so the
\ facility's own cells are dropped and INIT arms the new process from scratch.
: RESET ( -- )
   0 STUB-A !
   NULL-PTR FD-WORD-A !
   0 CAUGHT !
   0 OWNER !
   FORGET-FDS
   false READY ! ;

: REGISTER-CLEANUP ( -- )
   REGISTERED @ if exit then
   [: RESET ;] IMAGE-LIFECYCLE:REGISTER
   true REGISTERED ! ;

public

\ From the MAIN task: the two published cells are read once here and kept.
\ The fd word is armed last, so a signal that arrives while INIT is opening the
\ pipe is absorbed rather than written to a half-built facility.
: INIT ( -- )
   READY @ if E-SIGNAL-STATE throw then
   NEED-HOST
   REGISTER-CLEANUP
   KEEP-ABI
   OPEN-PIPE
   0 CAUGHT !
   TASK:SELF-N OWNER !
   true READY !
   WRITE-FD-CELL @ ARM ;

\ Deliver signal n to the descriptor instead of its default disposition. From
\ the task that ran INIT: the install writes the shared SA-ACT and SA-OLD.
: CATCH ( n -- ) {: sig:n :}
   NEED-READY
   NEED-OWNER
   sig SIG-CHECK
   sig INSTALL-STUB
   sig CAUGHT+ ;

\ The read end, for a program that polls it beside its sockets.
: FD ( -- fd )
   NEED-READY
   READ-FD ;

\ Readable now, without consuming: a zero-length window, which the kernel serves
\ inline for a descriptor that is already ready. "Ready" is not "readable": a
\ read end that is gone is the refusal READABLE-WITHIN? names, never a true that
\ would send a caller into the WAIT that names it.
: PENDING? ( -- bool )
   NEED-READY
   0 >MS READABLE-WITHIN? ;

\ The next signal number, or the closing of the window. One deadline is set from
\ the ms this call was given and every wait inside it runs against that one
\ deadline: a read another task won re-polls there. So a lost race cannot push
\ one WAIT past the milliseconds it was given, and a window that runs out while
\ the race is being lost answers timeout. The two spellings are not redundant:
\ the first wait is given the window as the caller spelled it, and only a wait
\ that follows something re-derives what is left, because PROC-LEFT-MS floors
\ and would shave a millisecond off a window nothing had yet consumed.
: WAIT ( ms -- signal-result ) {: window:ms :}
   NEED-READY
   window PROC-DEADLINE-AT {: deadline :}
   window                                          \ milliseconds left to wait for
   begin
      dup READABLE-WITHIN? 0= if
         drop SIGNAL-SIGNAL--RESULT:timeout exit
      then
      SIGNO@ dup SIGNO-REFUSED <> if
         nip SIGNAL-SIGNAL--RESULT:signal exit
      then
      2drop deadline PROC-LEFT-MS
   again ;

\ The fd word is cleared FIRST, so a signal delivered during the restore is
\ absorbed instead of written to a descriptor that is about to close. From the
\ task that ran INIT: the restore writes the shared SA-ACT and SA-OLD.
: RELEASE ( -- )
   NEED-READY
   NEED-OWNER
   DISARM
   RESTORE-ALL
   READ-FD-CELL @ WRITE-FD-CELL @ {: r:n w:n :}
   FORGET-FDS
   false READY !
   r w CLOSE-PIPE ;

;package
