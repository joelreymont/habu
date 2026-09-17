\ signal-stub.f - the baked async-signal-safe handler stub and the two cells
\ that publish it (src/habu/crash.f EMIT-SIGNAL-HANDLER, src/habu/layout.f
\ package SIGNAL-ABI, dot habu-bake-an-async-e5ba9580).
\
\ WHY A MACHINE-CODE STUB EXISTS AT ALL. No Forth word is async-signal-safe: a
\ handler that entered the engine would run the compiler's storage, the
\ dictionary and the VM stacks at whatever point the signal interrupted them.
\ So a program that wants a signal gets it on a file descriptor instead, and
\ the only code the kernel ever runs in the handler is the stub the engine
\ bakes beside its crash handler.
\
\ EVERYTHING BELOW IS READ OUT OF THE RUNNING ENGINE. The cells are this
\ engine's own, published at its boot; the stub address the test hands sigaction
\ is the one the engine published, never a source constant.
\
\ Run: bin/hb --load test/signal-stub.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/ffi-abi.f
require src/habu/layout.f

package SIGNAL-STUB-TEST
private

\ glibc's struct sigaction on aarch64: sa_handler, then the 128-byte mask, the
\ flags and the restorer. Only offset zero is spelled here - the handler - and
\ the rest of the record is zeroed, which is an empty mask and no flags.
152 constant SA-BYTES
10 constant SIGUSR1
12 constant SIGUSR2
4 constant SIGNO-BYTES

create SA-ACT SA-BYTES allot
create SA-OLD SA-BYTES allot
create SIGNO-BUF 8 allot

variable PIPE-R
variable PIPE-W

PROCESS-SYMBOLS

FUNCTION: SIGACTION-CALL sigaction ( n ptr u8 ptr u8 -- n )
   2 SA-BYTES WRITES-BYTES
;FUNCTION

: ZERO-BYTES ( ptr u8 n -- ) {: a u :}
   0 begin dup u < while
      0 over a + c!
      1+
   repeat drop ;

: STUB@ ( -- n )
   data-base SIGNAL-ABI:STUB-CELL + @ ;

: FD-PTR@ ( -- n )
   data-base SIGNAL-ABI:FD-PTR-CELL + @ ;

\ The published pointer, read as a pointer: the fd word is reached through
\ FD-PTR-CELL and never through an offset of this task's own region, which is
\ the whole point of publishing an address.
: FD-PTR-SLOT ( -- ptr n )
   data-base SIGNAL-ABI:FD-PTR-CELL + ;

: FD-WORD ( -- ptr n )
   FD-PTR-SLOT 0 ptr-field @ ;

: FD! ( n -- )
   FD-WORD ! ;

: FD@ ( -- n )
   FD-WORD @ ;

: DATA-BASE-N ( -- n )
   data-base NULL-PTR - ;

: INSTALL-STUB ( n -- ) {: sig:n :}
   SA-ACT SA-BYTES ZERO-BYTES
   SA-OLD SA-BYTES ZERO-BYTES
   STUB@ SA-ACT cell-view !
   sig SA-ACT SA-OLD SIGACTION-CALL 0 <> if E-PROC-OUTPUT throw then ;

: RAISE ( n -- ) {: sig:n :}
   getpid sig kill 0 <> if E-PROC-OUTPUT throw then ;

: READABLE? ( n -- bool ) {: ms:n :}
   PIPE-R @ >FD ms >MS POLL-IN COUNT>N 0 > ;

\ Reads only what poll says is there, so a case that expects bytes and finds
\ none FAILS with a zero count instead of waiting for a delivery that is never
\ coming. Nothing here has to wait long: POSIX delivers an unblocked signal a
\ process sends to itself before kill() returns, so the stub has already run by
\ the time RAISE does, and the milliseconds are only slack for a loaded host.
: PIPE-READ ( n -- n ) {: ms:n :}
   SIGNO-BUF 8 ZERO-BYTES
   ms READABLE? 0= if 0 exit then
   PIPE-R @ SIGNO-BUF SIGNO-BYTES read ;

: SIGNO-BUF@ ( -- n )
   SIGNO-BUF cell-view @ ;

\ ---- case one: the engine published both cells ------------------------------

: PUBLISHED-CASE ( -- )
   s" the engine published the signal stub's address at boot" T-LABEL
   STUB@ 0 <> TTRUE

   s" ... and the address of the fd word the stub reads" T-LABEL
   FD-PTR@ 0 <> TTRUE

   s" the published fd pointer is the claimed process-wide cell" T-LABEL
   FD-PTR@ DATA-BASE-N - SIGNAL-ABI:FD-CELL T=

   s" the fd word starts clear, whatever this engine's image carried" T-LABEL
   FD@ 0 T= ;

\ ---- case two: the cells were legal to take ---------------------------------

: BAND-CASE ( -- )
   s" the three cells sit directly above AOT-SPAN's band" T-LABEL
   SIGNAL-ABI:STUB-CELL AOT-SPAN:BASE-CELL - CELL T=
   SIGNAL-ABI:FD-PTR-CELL SIGNAL-ABI:STUB-CELL - CELL T=
   SIGNAL-ABI:FD-CELL SIGNAL-ABI:FD-PTR-CELL - CELL T=

   s" ... and below the word-frame cell that ends the header hole" T-LABEL
   SIGNAL-ABI:FD-CELL CELL + FRAME-CELL <= TTRUE

   s" each is addressable by the `DATA <off> STR` form the boot uses" T-LABEL
   SIGNAL-ABI:FD-CELL $7FF8 < TTRUE

   s" each is below DATA-START, so no compiled source can reach it" T-LABEL
   SIGNAL-ABI:FD-CELL DATA-START < TTRUE

   s" each is cell-aligned, as the stub's own load requires" T-LABEL
   SIGNAL-ABI:STUB-CELL CELL mod 0 T=
   SIGNAL-ABI:FD-PTR-CELL CELL mod 0 T=
   SIGNAL-ABI:FD-CELL CELL mod 0 T= ;

\ ---- case three: a signal with the fd word clear is absorbed ------------------
\ This runs FIRST, and it is also what makes case four's number mean something:
\ if a cleared fd word wrote anything, the byte case four reads back would be
\ SIGUSR1's number and not SIGUSR2's.

: ABSORBED-CASE ( -- )
   0 FD!
   SIGUSR1 RAISE

   s" a signal delivered with the fd word clear writes nothing" T-LABEL
   0 PIPE-READ SIGNO-BYTES < TTRUE

   s" ... and the process runs on past it" T-LABEL
   FD@ 0 T= ;

\ ---- case four: an armed fd word carries the signal number out ---------------

: DELIVERED-CASE ( -- )
   PIPE-W @ FD!

   s" the fd word holds the descriptor stored through the published pointer" T-LABEL
   FD@ PIPE-W @ T=

   SIGUSR2 RAISE

   s" a signal delivered with the fd word armed reaches the pipe" T-LABEL
   1000 PIPE-READ SIGNO-BYTES T=

   s" ... as the four-byte number of the signal that was raised" T-LABEL
   SIGNO-BUF@ SIGUSR2 T=

   s" and it wrote that number once, not once per delivery" T-LABEL
   0 PIPE-READ SIGNO-BYTES < TTRUE ;

: OPEN-PIPE ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r FD>N PIPE-R !
   w FD>N PIPE-W ! ;

: CLOSE-PIPE ( -- )
   0 FD!
   PIPE-R @ close
   PIPE-W @ close ;

public

: RUN ( -- )
   PUBLISHED-CASE
   BAND-CASE
   OPEN-PIPE
   SIGUSR1 INSTALL-STUB
   SIGUSR2 INSTALL-STUB
   ABSORBED-CASE
   DELIVERED-CASE
   CLOSE-PIPE
   T-REPORT
   s" signal-stub: ok" type cr ;

;package

SIGNAL-STUB-TEST:RUN
