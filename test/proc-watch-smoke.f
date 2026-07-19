\ proc-watch-smoke.f - focused proof of the proc-watch-open process-lifetime
\ watch primitive: the returned descriptor becomes readable when the watched
\ child exits, on both the live-then-exit fast path and the already-dead race,
\ and a non-existent pid fails closed.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f \
\      lib/process.f lib/process-fork.f test/proc-watch-smoke.f

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/process.f
require lib/process-fork.f

package PROC-WATCH-SMOKE

1 constant POLLIN
8 constant POLLERR
$20 constant POLLNVAL
$1388 constant READY-MS            \ 5s deadline for the live->dead transition
$7FFF0000 constant DEAD-PID        \ a positive pid guaranteed not to exist

create POLL-FDS 1 cells allot
create GO-BYTE 1 allot

variable GO-R    variable GO-W     \ go-pipe: parent write, child blocks on read
variable ALIVE-R variable ALIVE-W  \ alive-pipe: child holds write end, closes on exit

: CLOSE-BAD ( fd -- n ) {: fd:fd :}
   fd FD>N 0 < if 0 exit then
   fd FD>N close-rc 0 <> if 1 exit then
   0 ;

: CLOSE-OK ( fd -- )
   CLOSE-BAD 0 T= ;

: WATCH-OPEN ( pid -- fd )
   PID>N proc-watch-open >FD ;

: PFD! ( fd -- ) {: wfd:fd :}
   POLLIN 32 lshift wfd FD>N $FFFFFFFF and or POLL-FDS ! ;

: PFD-REVENTS ( -- n )
   POLL-FDS @ 48 rshift $FFFF and ;

\ Poll the watch fd for ms; true only when it reports a clean readable exit,
\ throwing on POLLERR/POLLNVAL so a broken descriptor cannot masquerade as ready.
: WATCH-READY? ( fd n -- bool ) {: wfd:fd ms:n :}
   wfd PFD!
   POLL-FDS 1 ms poll {: rc:n :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if false exit then
   PFD-REVENTS {: ev:n :}
   ev POLLERR POLLNVAL or and 0 <> if E-PROC-OUTPUT throw then
   ev POLLIN and 0 <> ;

\ ---- children (each ends in die; never returns) -----------------------------
: FAST-CHILD ( -- )
   GO-W @ >FD CLOSE-BAD drop
   GO-R @ >FD FD>N GO-BYTE 1 read drop
   s" " 0 die ;

: DEAD-CHILD ( -- )
   ALIVE-R @ >FD CLOSE-BAD drop
   s" " 0 die ;

: FORK-FAST ( -- pid )
   PROC-FORK dup PID>N 0= if drop FAST-CHILD then ;

: FORK-DEAD ( -- pid )
   PROC-FORK dup PID>N 0= if drop DEAD-CHILD then ;

\ ---- checks ------------------------------------------------------------------
\ Fast path: watch a LIVE child, then release it to exit; the descriptor must
\ signal readiness within the deadline.
: CHECK-FAST ( -- )
   PIPE-PAIR GO-W ! GO-R !
   FORK-FAST {: cpid:pid :}
   GO-R @ >FD CLOSE-OK
   cpid WATCH-OPEN {: wfd:fd :}
   wfd FD>N 0 < if E-PROC-OUTPUT throw then
   1 GO-BYTE c!
   GO-W @ >FD FD>N GO-BYTE 1 write drop
   GO-W @ >FD CLOSE-OK
   wfd READY-MS WATCH-READY? TTRUE
   cpid PROC-WAIT-STATUS 0 T=
   wfd CLOSE-OK ;

\ Already-dead race: the child exits BEFORE the watch is opened (confirmed by the
\ alive-pipe reaching end-of-file). The two backends differ by design here, and
\ both are fail-closed: Linux `pidfd_open` still opens on the surviving zombie and
\ the descriptor is immediately readable, while macOS `kqueue`/`EVFILT_PROC`
\ cannot register on an already-exited process and returns -1 (the caller then
\ learns of the exit through wait()). Assert the backend-correct outcome.
: DEAD-WATCH-LINUX ( pid -- )   {: cpid:pid :}
   cpid WATCH-OPEN {: wfd:fd :}
   wfd FD>N 0 < if E-PROC-OUTPUT throw then
   wfd 0 WATCH-READY? TTRUE
   wfd CLOSE-OK ;

: DEAD-WATCH-MACOS ( pid -- )   {: cpid:pid :}
   cpid WATCH-OPEN {: wfd:fd :}
   wfd FD>N 0 < TTRUE ;

: CHECK-DEAD ( -- )
   PIPE-PAIR ALIVE-W ! ALIVE-R !
   FORK-DEAD {: cpid:pid :}
   ALIVE-W @ >FD CLOSE-OK
   ALIVE-R @ >FD FD>N GO-BYTE 1 read 0 T=
   ALIVE-R @ >FD CLOSE-OK
   HB-TARGET-LINUX? if cpid DEAD-WATCH-LINUX else cpid DEAD-WATCH-MACOS then
   cpid PROC-WAIT-STATUS 0 T= ;

\ Negative: a positive but non-existent pid must fail closed (fd < 0), matching
\ the recovered supervisor's `proc-watch-open dup 0 < if ... throw` contract.
: CHECK-INVALID ( -- )
   DEAD-PID >PID WATCH-OPEN {: wfd:fd :}
   wfd FD>N 0 < TTRUE ;

: RUN ( -- )
   T-RESET
   CHECK-FAST
   CHECK-DEAD
   CHECK-INVALID
   T-REPORT
   s" proc-watch-smoke: ok" type cr ;

RUN

;package
