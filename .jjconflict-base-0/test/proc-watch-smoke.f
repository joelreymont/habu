\ proc-watch-smoke.f - focused proof of the proc-watch-open process-lifetime
\ watch primitive: the returned descriptor becomes readable when the watched
\ child exits, a second watch observes the surviving zombie correctly, and a
\ non-existent pid fails closed.
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

: CLOSE-BAD ( fd -- n ) {: fd:fd :}
   fd FD>N 0 < if 0 exit then
   fd FD>N close-rc 0 <> if 1 exit then
   0 ;

: CLOSE-OK ( fd -- )
   CLOSE-BAD 0 T= ;

: CLOSE-CHECKED ( fd -- )
   CLOSE-BAD 0 <> if E-PROC-OUTPUT throw then ;

: WATCH-OPEN ( pid -- fd )
   PID>N proc-watch-open >FD ;

: WATCH ( pid -- fd )
   WATCH-OPEN dup FD>N 0 < if E-PROC-OUTPUT throw then ;

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
: BLOCKED-CHILD ( -- )
   GO-W @ >FD CLOSE-CHECKED
   GO-R @ >FD FD>N GO-BYTE 1 read 1 <> if E-PROC-OUTPUT throw then
   s" " 0 die ;

: FORK-HELD ( -- pid )
   PIPE-PAIR GO-W ! GO-R !
   PROC-FORK:CHECKED dup PID>N 0= if drop BLOCKED-CHILD then
   {: cpid:pid :}
   GO-R @ >FD CLOSE-OK
   cpid ;

: RELEASE ( -- )
   1 GO-BYTE c!
   GO-W @ >FD FD>N GO-BYTE 1 write 1 <> if E-PROC-OUTPUT throw then
   GO-W @ >FD CLOSE-OK ;

\ ---- checks ------------------------------------------------------------------
\ Fast path: watch a LIVE child, then release it to exit; the descriptor must
\ signal readiness within the deadline.
: CHECK-FAST ( -- )
   FORK-HELD {: cpid:pid :}
   cpid WATCH {: wfd:fd :}
   RELEASE
   wfd READY-MS WATCH-READY? TTRUE
   cpid PROC-WAIT-STATUS 0 T=
   wfd CLOSE-OK ;

\ Watch A is opened while the child is held on the go pipe. Its readiness is
\ the non-consuming death barrier. Watch B is then opened before wait() reaps
\ the child. Linux opens the surviving zombie and reports immediate readiness;
\ macOS cannot register an already-exited process and fails closed.
: DEAD-WATCH-LINUX ( pid -- )   {: cpid:pid :}
   cpid WATCH {: wb:fd :}
   wb 0 WATCH-READY? TTRUE
   wb CLOSE-OK ;

: DEAD-WATCH-MACOS ( pid -- )   {: cpid:pid :}
   cpid WATCH-OPEN {: wb:fd :}
   wb FD>N 0 < TTRUE ;

: CHECK-DEAD ( -- )
   FORK-HELD {: cpid:pid :}
   cpid WATCH {: wa:fd :}
   RELEASE
   wa READY-MS WATCH-READY? TTRUE
   HB-TARGET-LINUX? if cpid DEAD-WATCH-LINUX else cpid DEAD-WATCH-MACOS then
   wa CLOSE-OK
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
