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
\ The four-byte staging span WAIT reads into is a TASK:+USER row, so two tasks
\ polling the read end never share it. See docs/signal.md.
\
\ Load after lib/process.f.

s" lib/errors.f" required
s" lib/process.f" required                \ the pipe, the pollfd row and PROC-POLL-RESTART
s" lib/task.f" required                   \ TASK:+USER carries the per-task staging span
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

\ Linux aarch64 numbers. SIGHUP, SIGINT and SIGTERM are these on macOS too;
\ SIGUSR1 and SIGUSR2 are 30 and 31 there, and this file does not switch on the
\ target because nothing in lib/ carries a target switch yet (docs/signal.md).
1 constant SIGHUP
2 constant SIGINT
15 constant SIGTERM
10 constant SIGUSR1
12 constant SIGUSR2

private

\ glibc's struct sigaction on aarch64: sa_handler at 0, the 128-byte mask at 8,
\ sa_flags at $88 and sa_restorer at $90. Only the handler and the flags are
\ spelled; one cell write at $88 covers the int and the padding that follows it,
\ and stops short of the restorer.
$98 constant SA-BYTES
$88 constant SA-FLAGS-OFF
$10000000 constant SA-RESTART      \ Linux; macOS spells the same flag 2
0 constant SA-NO-FLAGS
0 constant SIG-DFL

4 constant SIGNO-BYTES             \ the width the stub writes, and under PIPE_BUF
1 constant SIG-MIN
64 constant SIG-MAX                \ Linux's highest; macOS stops at 31

create SA-ACT SA-BYTES allot
create SA-OLD SA-BYTES allot       \ sigaction fills this; its prior content is dead

\ The two values kept from the main task's region, and the pipe they arm.
variable STUB-A                    \ the stub's runtime address, as sa_handler
variable FD-WORD-A                 \ the ADDRESS of the process-wide fd word
variable CAUGHT                    \ bit (sig-1) for every signal CATCH installed
variable READ-FD-CELL
variable WRITE-FD-CELL

TYPED-VARIABLE READY bool
false READY !

TYPED-VARIABLE REGISTERED bool
false REGISTERED !

\ One staging span per task: WAIT reads the number into its own row.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and CELL TASK:+USER SIGNO-STORAGE drop

PROCESS-SYMBOLS

FUNCTION: SIGACTION-CALL sigaction ( n ptr u8 ptr u8 -- n )
   2 SA-BYTES WRITES-BYTES
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

: FD-WORD-SLOT ( -- ptr ptr n )
   FD-WORD-A 0 ptr-field ;

: FD-WORD ( -- ptr n )
   FD-WORD-SLOT @ ;

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
   FD-WORD-PUBLISHED FD-WORD-SLOT ! ;

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
\ after a caught signal. poll(2) restarts for nobody and reports -EINTR, which
\ is why WAIT owns a deadline of its own.
: INSTALL-STUB ( n -- ) {: sig:n :}
   SA-ACT SA-BYTES ZERO-BYTES
   STUB-A @ SA-HANDLER!
   SA-RESTART SA-FLAGS!
   sig SIGACTION! ;

: INSTALL-DEFAULT ( n -- ) {: sig:n :}
   SA-ACT SA-BYTES ZERO-BYTES
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

: OPEN-PIPE ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r FD-CLOEXEC!
   w FD-CLOEXEC!
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

\ ---- polling the read end ----------------------------------------------------

: PFD-ARM ( -- )
   READ-FD POLLIN PROC-PFD! ;

\ No deadline to restart against, so an interrupted zero-wait poll is simply
\ asked again: the stub has already written by the time the call reports -EINTR.
: POLL-NOW ( -- n )
   begin
      PFD-ARM
      1 0 PROC-POLL-ONCE {: rc :}
      rc EINTR# negate <> if rc exit then
   again ;

: POLL-UNTIL ( ms -- n ) {: ms :}
   ms PROC-DEADLINE-AT {: deadline :}
   PFD-ARM
   1 ms MS>N deadline PROC-POLL-RESTART ;

: POLL-RC ( n -- n ) {: rc :}
   rc 0 < if E-SIGNAL-POLL throw then
   rc ;

\ Four bytes is under PIPE_BUF, so the pipe takes the number whole or not at
\ all: a short read is a torn number the stub guarantees against, not a partial
\ delivery to be resumed.
: SIGNO@ ( -- n )
   SIGNO-BUF CELL ZERO-BYTES
   READ-FD FD>N SIGNO-BUF SIGNO-BYTES read {: got:n :}
   got SIGNO-BYTES <> if E-SIGNAL-READ throw then
   SIGNO-BUF cell-view @ ;

: NEED-READY ( -- )
   READY @ 0= if E-SIGNAL-STATE throw then ;

\ A restored image is a different process: its descriptors and its sigaction
\ table are gone and the engine's boot has already cleared the fd word, so the
\ facility's own cells are dropped and INIT arms the new process from scratch.
: RESET ( -- )
   0 STUB-A !
   NULL-PTR FD-WORD-SLOT !
   0 CAUGHT !
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
   REGISTER-CLEANUP
   KEEP-ABI
   OPEN-PIPE
   0 CAUGHT !
   true READY !
   WRITE-FD-CELL @ ARM ;

\ Deliver signal n to the descriptor instead of its default disposition.
: CATCH ( n -- ) {: sig:n :}
   NEED-READY
   sig SIG-CHECK
   sig INSTALL-STUB
   sig CAUGHT+ ;

\ The read end, for a program that polls it beside its sockets.
: FD ( -- fd )
   NEED-READY
   READ-FD ;

\ Readable now, without consuming.
: PENDING? ( -- bool )
   NEED-READY
   POLL-NOW POLL-RC 0 > ;

\ The next signal number, or the closing of the window. The poll restarts on
\ -EINTR against the deadline the first call set, so a signal storm cannot push
\ one WAIT past the milliseconds it was given.
: WAIT ( ms -- signal-result ) {: ms :}
   NEED-READY
   ms POLL-UNTIL POLL-RC 0= if SIGNAL-SIGNAL--RESULT:timeout exit then
   SIGNO@ SIGNAL-SIGNAL--RESULT:signal ;

\ The fd word is cleared FIRST, so a signal delivered during the restore is
\ absorbed instead of written to a descriptor that is about to close.
: RELEASE ( -- )
   NEED-READY
   DISARM
   RESTORE-ALL
   READ-FD-CELL @ WRITE-FD-CELL @ {: r:n w:n :}
   FORGET-FDS
   false READY !
   r w CLOSE-PIPE ;

;package
