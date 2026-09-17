\ process.f - checked process helpers.
\
\ STORAGE CLASS. TASK-LOCAL. The path staging buffer, the pollfd array and the
\ per-call capture slots are one PROC-STORAGE-BYTES TASK:+USER row, so each task
\ spawns, polls, drains and reaps through its own descriptors, lengths, deadline
\ and wait status, and any number of tasks may run children at once. The byte
\ spans the capture and POLL words take are caller-owned. The PROC-REAP-ARM
\ vector is process-wide: it is one installed policy, not per-call state.
\ See docs/threads.md.
\
\ Load after lib/errors.f.

s" lib/errors.f" required
s" lib/adt/result.f" required            \ result<n,n> for PROC-RUN-IO-RC (switchover wave B)
s" lib/task.f" required                  \ TASK:+USER carries the per-call row

\ outcome - how a child process completed (switchover wave C): a clean exit
\ carrying the exit code, a signal death carrying the signal, or a capture
\ timeout (always SIGKILL-reaped, so no payload). The checker forces every
\ consumer through exhaustive MATCH; PROC-OUTCOME>RC is the rc flattener.
SUMTYPE outcome 0
  VARIANT exited n ;VARIANT
  VARIANT signaled n ;VARIANT
  VARIANT timeout ;VARIANT
;SUMTYPE

\ pcap - the byte-length pair a bounded capture always produces, split by how the
\ child completed (switchover wave B). A clean exit carries just the two captured
\ lengths (captured); a nonzero completion carries the SAME two lengths PLUS its
\ completion code — a nonzero exit code, or 128+signal (failed). The lengths are
\ valid on BOTH arms: captured output is real whether or not the child exited
\ clean, and callers replay it either way, so both structures keep the lengths and
\ only the failure adds the code. The rc sentinel this replaces buried the
\ clean-vs-failed distinction in one signed cell; the result forces every caller
\ to MATCH both arms. (No errno is returned here — OS spawn/wait failures THROW
\ E-PROC-SPAWN/E-PROC-WAIT inside the spawn path; the code is the child's own
\ completion code, never an errno.)
package PCAP
public
STRUCTURE captured 0
  FIELD out len
  FIELD err len
;STRUCTURE
STRUCTURE failed 0
  FIELD out len
  FIELD err len
  FIELD code rc
;STRUCTURE
private
;package

1024 constant PROC-PATHZ-CAP
1000000 constant PROC-NS-PER-MS
1 constant POLLIN
4 constant POLLOUT
8 constant POLLERR
16 constant POLLHUP
32 constant POLLNVAL
4 constant EINTR#                  \ "interrupted by a signal" (POSIX, identical on Linux/macOS)
9 constant SIGKILL
2 constant F-SETFD
3 constant F-GETFL
4 constant F-SETFL
73 constant F-SETNOSIGPIPE
1 constant FD-CLOEXEC
$7F constant PROC-WAIT-TERM-MASK
$FF constant PROC-WAIT-EXIT-MASK
4096 constant PROC-STDIN-CHUNK-CAP


\ O_NONBLOCK is not one number: $800 (0o4000) on Linux, 4 on macOS. F_SETFL
\ silently drops a bit the host does not know, so the wrong constant arms
\ nothing at all and the descriptor stays blocking.
: O-NONBLOCK ( -- n )
   HB-TARGET-LINUX? if $800 exit then
   HB-TARGET-MACOS? if 4 exit then
   E-PROC-HOST throw ;


-1 constant PROC-NO-FD                   \ a closed / never-opened descriptor cell
-1 constant PROC-NO-PID                  \ no child, and no armed reaper

\ --- the per-task row --------------------------------------------------------
\ One TASK:+USER row, laid out like lib/net/tcp4.f's: a poll array shared by two
\ tasks hands each of them the other's descriptor in slot 0, so a task capturing
\ a child and a task in POLL-IN used to poll each other's pipes
\ (dot habu-make-the-process-6e615161). Every cell below is per-call state of one
\ spawn/capture, so the whole set moves together; a task that shares nothing
\ shares no slot.
\
\ A fresh task's region is zeroed, so the row reads 0 before its first capture,
\ exactly as the dictionary cells it replaces read 0 before the image's first
\ one: every entry point resets the row through PROC-CAPTURE-RESET before any
\ word reads a descriptor, and PROC-REAP-DISARM's `0 >` guard treats the zero
\ and the PROC-NO-PID sentinel alike.
3 constant PROC-PFD-SLOTS                \ stdout, stderr, stdin
8 constant PROC-PFD-SLOT-BYTES           \ one struct pollfd: fd | events | revents
0 constant PROC-PATHZ-OFF
PROC-PATHZ-OFF PROC-PATHZ-CAP + constant PROC-PFD-OFF
PROC-PFD-OFF PROC-PFD-SLOTS PROC-PFD-SLOT-BYTES * + constant PROC-PROBE-OFF
PROC-PROBE-OFF 1 + 7 + $FFFFFFFFFFFFFFF8 and constant PROC-PID-OFF
PROC-PID-OFF     1 cells + constant PROC-RC-OFF
PROC-RC-OFF      1 cells + constant PROC-OUT-R-OFF
PROC-OUT-R-OFF   1 cells + constant PROC-OUT-W-OFF
PROC-OUT-W-OFF   1 cells + constant PROC-ERR-R-OFF
PROC-ERR-R-OFF   1 cells + constant PROC-ERR-W-OFF
PROC-ERR-W-OFF   1 cells + constant PROC-IN-R-OFF
PROC-IN-R-OFF    1 cells + constant PROC-IN-W-OFF
PROC-IN-W-OFF    1 cells + constant PROC-OUT-LEN-OFF
PROC-OUT-LEN-OFF 1 cells + constant PROC-ERR-LEN-OFF
PROC-ERR-LEN-OFF 1 cells + constant PROC-IN-OFF-OFF
PROC-IN-OFF-OFF  1 cells + constant PROC-DEADLINE-OFF
PROC-DEADLINE-OFF 1 cells + constant PROC-RD-OFF
PROC-RD-OFF      1 cells + constant PROC-STATUS-OFF
PROC-STATUS-OFF  1 cells + constant PROC-TIMED-OUT-OFF
PROC-TIMED-OUT-OFF 1 cells + constant PROC-REAP-PID-OFF
PROC-REAP-PID-OFF 1 cells + constant PROC-STORAGE-BYTES

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and PROC-STORAGE-BYTES TASK:+USER PROC-STORAGE drop

: PROC-ROW ( -- ptr u8 ) PROC-STORAGE BYTE-VIEW ;

: PROC-PATHZ-BUF ( -- ptr u8 ) PROC-ROW PROC-PATHZ-OFF + ;
: PROC-PFD ( -- ptr u8 ) PROC-ROW PROC-PFD-OFF + ;
: PROC-PROBE ( -- ptr u8 ) PROC-ROW PROC-PROBE-OFF + ;

\ The capture slots. TASK:+USER publishes one `ptr n` row, so these are cells of
\ n and the fd/pid/rc/len/off roles are converted where the value crosses into a
\ word that names the role, not carried by the cell.
: PROC-PID ( -- ptr n ) PROC-ROW PROC-PID-OFF + CELL-VIEW ;
: PROC-RC ( -- ptr n ) PROC-ROW PROC-RC-OFF + CELL-VIEW ;
: PROC-OUT-R ( -- ptr n ) PROC-ROW PROC-OUT-R-OFF + CELL-VIEW ;
: PROC-OUT-W ( -- ptr n ) PROC-ROW PROC-OUT-W-OFF + CELL-VIEW ;
: PROC-ERR-R ( -- ptr n ) PROC-ROW PROC-ERR-R-OFF + CELL-VIEW ;
: PROC-ERR-W ( -- ptr n ) PROC-ROW PROC-ERR-W-OFF + CELL-VIEW ;
: PROC-IN-R ( -- ptr n ) PROC-ROW PROC-IN-R-OFF + CELL-VIEW ;
: PROC-IN-W ( -- ptr n ) PROC-ROW PROC-IN-W-OFF + CELL-VIEW ;
: PROC-OUT-LEN ( -- ptr n ) PROC-ROW PROC-OUT-LEN-OFF + CELL-VIEW ;
: PROC-ERR-LEN ( -- ptr n ) PROC-ROW PROC-ERR-LEN-OFF + CELL-VIEW ;
: PROC-IN-OFF ( -- ptr n ) PROC-ROW PROC-IN-OFF-OFF + CELL-VIEW ;
: PROC-DEADLINE ( -- ptr n ) PROC-ROW PROC-DEADLINE-OFF + CELL-VIEW ;
: PROC-RD ( -- ptr n ) PROC-ROW PROC-RD-OFF + CELL-VIEW ;
: PROC-STATUS ( -- ptr n ) PROC-ROW PROC-STATUS-OFF + CELL-VIEW ;
: PROC-TIMED-OUT ( -- ptr n ) PROC-ROW PROC-TIMED-OUT-OFF + CELL-VIEW ;
\ Capture-child death-reaper pid; see the PROC-REAP-ARM seam below.
: PROC-REAP-PID ( -- ptr n ) PROC-ROW PROC-REAP-PID-OFF + CELL-VIEW ;

: PROC-WAIT-STATUS-RAW ( pid -- n ) {: pid :}
   pid PID>N wait-status ;

: PROC-SPAWN-RAW ( ptr u8 fd fd fd -- pid ) {: pathz:ptr infd outfd errfd :}
   pathz infd FD>N outfd FD>N errfd FD>N spawn-io >PID ;

: PROC-KILL-RAW ( pid n -- rc ) {: pid sig :}
   pid PID>N sig kill >RC ;

: PROC-ZCOPY ( ptr u8 len ptr u8 len -- ptr u8 ) {: a:ptr u dst:ptr cap :}
   u LEN>N 1 + cap LEN>N > if E-PROC-OUTPUT throw then
   0 begin dup u LEN>N < while
      dup a + c@  over dst + c!
      1 +
   repeat drop
   0 dst u LEN>N + c!
   dst ;

: PROC-PATHZ ( ptr u8 len -- ptr u8 )
   PROC-PATHZ-BUF PROC-PATHZ-CAP >LEN PROC-ZCOPY ;

: PROC-WAIT-STATUS ( pid -- n )
   PROC-WAIT-STATUS-RAW {: status :}
   status 0 < if E-PROC-WAIT throw then
   status ;

: PROC-STATUS>OUTCOME ( n -- outcome ) {: status:n :}
   status PROC-WAIT-TERM-MASK and {: term:n :}
   term 0= if
      status 8 rshift PROC-WAIT-EXIT-MASK and OUTCOME:EXITED
      exit
   then
   term OUTCOME:SIGNALED ;

: PROC-OUTCOME>RC ( outcome -- rc )   \ 128+sig for non-exits (regression habu-wait-rc-masks-9ae37cd0)
   MATCH outcome
     exited OF >RC ENDOF
     signaled OF 128 + >RC ENDOF
     timeout OF 128 SIGKILL + >RC ENDOF
   ;MATCH ;

: PROC-STATUS>RC ( n -- rc )
   PROC-STATUS>OUTCOME PROC-OUTCOME>RC ;

: PROC-WAIT-OUTCOME ( pid -- outcome )
   PROC-WAIT-STATUS PROC-STATUS>OUTCOME ;

\ ok = clean exit (rc 0); err = the nonzero completion rc (a nonzero exit code, or
\ 128+signal — the signal-death encoding, regression habu-wait-rc-masks-9ae37cd0).
\ A wait failure (e.g. ECHILD) still THROWS E-PROC-WAIT inside PROC-WAIT-STATUS; no
\ errno is returned, so err carries only the process's own completion code.
: PROC-WAIT-RC ( pid -- result<n,n> )
   PROC-WAIT-STATUS PROC-STATUS>RC RC>N {: rc:n :}
   rc 0 = if rc RESULT:OK else rc RESULT:ERR then ;

: PROC-SPAWN-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PROC-PATHZ infd outfd errfd PROC-SPAWN-RAW {: pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: PROC-RUN-IO-RC ( ptr u8 len fd fd fd -- result<n,n> )   \ spawn with explicit stdio, wait -> result
   PROC-SPAWN-IO PROC-WAIT-RC ;

: PROC-RUN-RC ( ptr u8 len -- result<n,n> )   \ inherit-stdio run
   -1 >FD -1 >FD -1 >FD PROC-RUN-IO-RC ;

: FD-CLOEXEC! ( fd -- ) {: fd :}
   fd FD>N F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT throw then ;

: FD-NOSIGPIPE! ( fd -- ) {: fd :}
   fd FD>N F-SETNOSIGPIPE 1 fcntl 0 <> if E-PROC-OUTPUT throw then ;

: PIPE-PAIR ( -- fd fd )
   pipe {: r w rc :}
   rc 0 <> if E-PROC-OUTPUT throw then
   r >FD w >FD ;

: PROC-PFD-SLOT ( idx -- ptr n ) {: idx :}
   idx IDX>N PROC-PFD-SLOT-BYTES * PROC-PFD + CELL-VIEW ;

: PROC-PFD-AT! ( fd n idx -- ) {: fd events idx :}
   events 32 lshift  fd FD>N $FFFFFFFF and  or  idx PROC-PFD-SLOT ! ;

: PROC-PFD! ( fd n -- ) {: fd events :}
   fd events 0 >IDX PROC-PFD-AT! ;

: PROC-PFD-REVENTS ( idx -- n )
   PROC-PFD-SLOT @ 48 rshift $FFFF and ;

\ Milliseconds left before an absolute monotonic deadline, floored at zero. Both
\ the shared capture deadline and a caller's own one-shot window are measured
\ with this, so a restarted poll always waits out what is left rather than the
\ interval it was first given.
: PROC-LEFT-MS ( n -- ms ) {: deadline :}
   deadline mono-ns - dup 0 <= if
      drop 0 >MS
   else
      PROC-NS-PER-MS / >MS
   then ;

: PROC-DEADLINE-AT ( ms -- n ) {: timeout :}
   mono-ns timeout MS>N PROC-NS-PER-MS * + ;

: PROC-POLL-ONCE ( n n -- n ) {: nfds ms :}
   PROC-PFD nfds ms poll ;

\ poll(2) is the one blocking call SA_RESTART never restarts, so a -EINTR here
\ says a signal landed before any event did: nothing failed, nothing was
\ consumed, and the descriptors are untouched. The deadline is untouched too, so
\ each restart waits out only what is left of it and no signal storm can push a
\ capture past its timeout; a deadline that has already passed reports the
\ ordinary zero-event timeout. Every other negative rc is a real errno, and the
\ caller names it through its own E-PROC-* code.
: PROC-POLL-RESTART ( n n n -- n ) {: nfds ms deadline :}
   nfds ms PROC-POLL-ONCE {: rc :}
   rc EINTR# negate <> if rc exit then
   begin
      deadline PROC-LEFT-MS MS>N {: left :}
      left 0= if 0 exit then
      nfds left PROC-POLL-ONCE {: restarted :}
      restarted EINTR# negate <> if restarted exit then
   again ;

\ One raw poll of a single descriptor: a signal is reported as -EINTR, like any
\ other errno, because this word owns no deadline to restart against.
: POLL-IN ( fd ms -- count ) {: fd ms :}
   fd POLLIN PROC-PFD!
   PROC-PFD 1 ms MS>N poll >COUNT ;

: POLL-IN-OR-TIMEOUT ( fd ms -- count ) {: fd ms :}
   ms PROC-DEADLINE-AT {: deadline :}
   fd POLLIN PROC-PFD!
   1 ms MS>N deadline PROC-POLL-RESTART {: rc :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if E-PROC-TIMEOUT throw then
   rc >COUNT ;

\ Capture-child death-reaper seam. Contexts that own a death-watch fd (a pool
\ worker's worker-alive read end; lib/process-fork.f installs the live vector)
\ arm a co-located reaper for the just-spawned capture child so the child's
\ whole group dies with the arming process instead of orphan-lingering. The
\ default vector arms nothing and returns PROC-NO-PID.
\ The reaper is a DIRECT child of this process; every capture-termination path
\ disarms it (kill + wait by its specific pid), so no reaper outlives its
\ capture and no wait(-1) caller ever sees a stray child.
\
\ The vector itself is PROCESS-WIDE, unlike the row above: it is the one policy
\ the image installs at load, not state a call leaves behind.
defer PROC-REAP-ARM ( pid -- pid )
: PROC-REAP-ARM-OFF ( pid -- pid )
   drop PROC-NO-PID >PID ;
: PROC-REAP-ARM-DEFAULT ( -- )
   [: PROC-REAP-ARM-OFF ;] is PROC-REAP-ARM ;
PROC-REAP-ARM-DEFAULT

: PROC-CAPTURE-PID! ( pid -- ) {: pid:pid :}
   pid PID>N PROC-PID !
   pid PROC-REAP-ARM PID>N PROC-REAP-PID ! ;

: PROC-REAP-DISARM ( -- )
   PROC-REAP-PID @ dup 0 > if
      >PID dup SIGKILL PROC-KILL-RAW drop
      PROC-WAIT-STATUS drop
      PROC-NO-PID PROC-REAP-PID !
   else
      drop
   then ;

: PROC-CAPTURE-RESET ( -- )
   PROC-REAP-DISARM
   PROC-NO-PID PROC-PID !
   -1 PROC-RC !
   PROC-NO-FD PROC-OUT-R !
   PROC-NO-FD PROC-OUT-W !
   PROC-NO-FD PROC-ERR-R !
   PROC-NO-FD PROC-ERR-W !
   PROC-NO-FD PROC-IN-R !
   PROC-NO-FD PROC-IN-W !
   0 PROC-OUT-LEN !
   0 PROC-ERR-LEN !
   0 PROC-IN-OFF !
   0 PROC-STATUS !
   0 PROC-TIMED-OUT ! ;

: PROC-CLOSE-CELL ( ptr n -- ) {: p:ptr :}
   p @ dup 0 >= if
      close
      PROC-NO-FD p !
   else
      drop
   then ;

: PROC-CLOSE-CAPTURE-FDS ( -- )
   PROC-OUT-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-R PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-CLOSE-STDIN-FDS ( -- )
   PROC-IN-R PROC-CLOSE-CELL
   PROC-IN-W PROC-CLOSE-CELL ;

: PROC-CLOSE-ALL-CAPTURE-FDS ( -- )
   PROC-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS ;

: PROC-REAP-CAPTURE ( -- )
   PROC-PID @ dup 0 >= if
      >PID PROC-WAIT-STATUS dup PROC-STATUS !
      PROC-STATUS>RC RC>N PROC-RC !
      PROC-NO-PID PROC-PID !
   else
      drop
   then
   PROC-REAP-DISARM ;

: PROC-REAP-CAPTURE-TIMEOUT ( -- )
   PROC-PID @ dup 0 >= if
      >PID dup SIGKILL PROC-KILL-RAW drop
      PROC-WAIT-STATUS PROC-STATUS !
      PROC-NO-PID PROC-PID !
   else
      drop
   then
   PROC-REAP-DISARM
   1 PROC-TIMED-OUT !
   OUTCOME:TIMEOUT PROC-OUTCOME>RC RC>N PROC-RC ! ;

: PROC-KILL-CAPTURE ( -- )
   PROC-PID @ dup 0 >= if
      >PID SIGKILL PROC-KILL-RAW drop
      PROC-REAP-CAPTURE
   else
      drop
   then ;

: PROC-THROW-CAPTURE ( n -- ) {: code :}
   PROC-KILL-CAPTURE
   PROC-CLOSE-ALL-CAPTURE-FDS
   code throw ;

: PROC-OPEN-PIPE ( ptr n ptr n -- ) {: rp:ptr wp:ptr :}
   pipe {: r w rc :}
   rc 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   r rp !
   w wp ! ;

: PROC-CLOEXEC-CELL ( ptr n -- ) {: p:ptr :}
   p @ F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then ;

: PROC-NONBLOCK! ( fd -- ) {: fd :}
   fd FD>N F-GETFL 0 fcntl {: flags :}
   flags 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   fd FD>N F-SETFL flags O-NONBLOCK or fcntl 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then ;

: PROC-NOSIGPIPE! ( fd -- )
   FD-NOSIGPIPE! ;

: PROC-SETUP-CAPTURE-FDS ( -- )
   PROC-OUT-R PROC-OUT-W PROC-OPEN-PIPE
   PROC-ERR-R PROC-ERR-W PROC-OPEN-PIPE
   PROC-OUT-R PROC-CLOEXEC-CELL
   PROC-OUT-W PROC-CLOEXEC-CELL
   PROC-ERR-R PROC-CLOEXEC-CELL
   PROC-ERR-W PROC-CLOEXEC-CELL ;

: PROC-SETUP-STDIN-FDS ( -- )
   PROC-IN-R PROC-IN-W PROC-OPEN-PIPE
   PROC-IN-R PROC-CLOEXEC-CELL
   PROC-IN-W PROC-CLOEXEC-CELL
   PROC-IN-W @ >FD PROC-NOSIGPIPE!
   PROC-IN-W @ >FD PROC-NONBLOCK! ;

: PROC-CAPTURE-DEADLINE! ( ms -- ) {: timeout :}
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then
   timeout PROC-DEADLINE-AT PROC-DEADLINE ! ;

: PROC-REMAINING-MS ( -- ms )
   PROC-DEADLINE @ PROC-LEFT-MS ;

: PROC-ARM-CAPTURE-PFD ( -- )
   PROC-OUT-R @ >FD POLLIN 0 >IDX PROC-PFD-AT!
   PROC-ERR-R @ >FD POLLIN 1 >IDX PROC-PFD-AT! ;

: PROC-POLL-CAPTURE-RC ( n n -- n ) {: nfds ms :}
   nfds ms PROC-DEADLINE @ PROC-POLL-RESTART ;

: PROC-POLL-CAPTURE ( ms -- count ) {: ms :}
   PROC-ARM-CAPTURE-PFD
   2 ms MS>N PROC-POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT PROC-THROW-CAPTURE then
   rc >COUNT ;

: PROC-POLL-CAPTURE-OUTCOME ( ms -- count ) {: ms :}
   PROC-ARM-CAPTURE-PFD
   2 ms MS>N PROC-POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc >COUNT ;

: PROC-READ-STREAM ( ptr n ptr u8 len ptr n -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   lenp @ 0 < if E-PROC-TRUNCATED PROC-THROW-CAPTURE then
   lenp @ cap LEN>N > if E-PROC-TRUNCATED PROC-THROW-CAPTURE then
   cap LEN>N lenp @ - 0 <= if E-PROC-TRUNCATED PROC-THROW-CAPTURE then
   fdp @ buf lenp @ + cap LEN>N lenp @ - read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ cap LEN>N lenp @ - > if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      lenp @ PROC-RD @ + lenp !
   then ;

: PROC-PROBE-FULL-STREAM ( ptr n -- ) {: fdp:ptr :}
   fdp @ PROC-PROBE 1 read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 1 > if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      E-PROC-TRUNCATED PROC-THROW-CAPTURE
   then ;

: PROC-READ-OR-PROBE-STREAM ( ptr n ptr u8 len ptr n -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap LEN>N lenp @ - 0 <= if
      fdp PROC-PROBE-FULL-STREAM
   else
      fdp buf cap lenp PROC-READ-STREAM
   then ;

: PROC-DRAIN-READY ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   0 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-OUT-R out outcap PROC-OUT-LEN PROC-READ-OR-PROBE-STREAM
   then
   1 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-ERR-R err errcap PROC-ERR-LEN PROC-READ-OR-PROBE-STREAM
   then ;

: PROC-CAPTURE-DONE? ( -- bool )
   PROC-OUT-R @ 0 < PROC-ERR-R @ 0 < and ;

: PROC-STDIN-CHUNK ( len -- len ) {: u :}
   u LEN>N PROC-STDIN-CHUNK-CAP > if
      PROC-STDIN-CHUNK-CAP >LEN
   else
      u
   then ;

: PROC-CLOSE-STDIN-DONE ( len -- ) {: inu :}
   PROC-IN-OFF @ inu LEN>N >= if PROC-IN-W PROC-CLOSE-CELL then ;

: PROC-BROKEN-STDIN? ( n -- bool ) {: events :}
   events POLLERR and 0= 0= if 0 0= exit then
   events POLLHUP and 0= 0= if 0 0= exit then
   events POLLNVAL and 0= 0= if 0 0= exit then
   0 0= 0= ;

\ The write end is non-blocking, so a refused write is back pressure: the pipe
\ is full because the child stopped reading. The engine collapses every write
\ errno to -1, and the caller above has already asked poll whether the reader is
\ gone, so the descriptor keeps its offset and the next POLLOUT under the
\ capture deadline resumes the feed; a short write advances by what landed and
\ resumes the same way. Closing here instead would truncate the child's stdin.
: PROC-WRITE-STDIN-ACTIVE ( ptr u8 len -- ) {: src:ptr inu :}
   inu LEN>N PROC-IN-OFF @ - >LEN PROC-STDIN-CHUNK {: chunk :}
   PROC-IN-W @ src PROC-IN-OFF @ + chunk LEN>N write {: wrote :}
   wrote 0 < if exit then
   wrote chunk LEN>N > if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-IN-OFF @ wrote + PROC-IN-OFF !
   inu PROC-CLOSE-STDIN-DONE ;

: PROC-WRITE-STDIN ( ptr u8 len -- ) {: src:ptr inu :}
   PROC-IN-W @ 0 < if exit then
   PROC-IN-OFF @ inu LEN>N >= if PROC-IN-W PROC-CLOSE-CELL exit then
   src inu PROC-WRITE-STDIN-ACTIVE ;

: PROC-ARM-IO-PFD ( -- )
   PROC-ARM-CAPTURE-PFD
   PROC-IN-W @ 0 >= if
      PROC-IN-W @ >FD POLLOUT 2 >IDX PROC-PFD-AT!
   else
      PROC-NO-FD >FD 0 2 >IDX PROC-PFD-AT!
   then ;

: PROC-POLL-IO ( ms -- count ) {: ms :}
   PROC-ARM-IO-PFD
   3 ms MS>N PROC-POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT PROC-THROW-CAPTURE then
   rc >COUNT ;

: PROC-POLL-IO-OUTCOME ( ms -- count ) {: ms :}
   PROC-ARM-IO-PFD
   3 ms MS>N PROC-POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc >COUNT ;

: PROC-DRIVE-STDIN ( ptr u8 len -- ) {: in:ptr inu :}
   2 >IDX PROC-PFD-REVENTS {: events :}
   events PROC-BROKEN-STDIN? if PROC-IN-W PROC-CLOSE-CELL exit then
   events POLLOUT and 0= 0= if
      in inu PROC-WRITE-STDIN
   then ;

: PROC-STDIN-CAPTURE-DONE? ( -- bool )
   PROC-CAPTURE-DONE? PROC-IN-W @ 0 < and ;

: PROC-RUN-CAPTURE-LOOP ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   begin PROC-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-CAPTURE drop
      out outcap err errcap PROC-DRAIN-READY
   repeat ;

: PROC-RUN-CAPTURE-OUTCOME-LOOP ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   begin PROC-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-CAPTURE-OUTCOME dup COUNT>N 0= if
         drop
         PROC-REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      out outcap err errcap PROC-DRAIN-READY
   repeat
   PROC-REAP-CAPTURE ;

: PROC-RUN-STDIN-CAPTURE-LOOP ( ptr u8 len ptr u8 len ptr u8 len -- )
   {: in:ptr inu out:ptr outcap err:ptr errcap :}
   inu LEN>N 0 <= if PROC-IN-W PROC-CLOSE-CELL then
   begin PROC-STDIN-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-IO drop
      in inu PROC-DRIVE-STDIN
      out outcap err errcap PROC-DRAIN-READY
   repeat ;

: PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP ( ptr u8 len ptr u8 len ptr u8 len -- )
   {: in:ptr inu out:ptr outcap err:ptr errcap :}
   inu LEN>N 0 <= if PROC-IN-W PROC-CLOSE-CELL then
   begin PROC-STDIN-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-IO-OUTCOME dup COUNT>N 0= if
         drop
         PROC-CLOSE-STDIN-FDS
         PROC-REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      in inu PROC-DRIVE-STDIN
      out outcap err errcap PROC-DRAIN-READY
   repeat
   PROC-REAP-CAPTURE ;

: PROC-CAPTURE-CHECK-CAPS ( len len -- ) {: outcap errcap :}
   outcap LEN>N 0 < if E-PROC-OUTPUT throw then
   errcap LEN>N 0 < if E-PROC-OUTPUT throw then ;

: PROC-CAPTURE-CHECK-STDIN ( len -- ) {: inu :}
   inu LEN>N 0 < if E-PROC-OUTPUT throw then ;

: PROC-CAPTURE-BEGIN ( ms -- ) {: timeout :}
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS ;

: PROC-STDIN-CAPTURE-BEGIN ( ms -- ) {: timeout :}
   timeout PROC-CAPTURE-BEGIN
   PROC-SETUP-STDIN-FDS ;

\ Wrap a captured (out-len, err-len) plus the stored completion rc into a result:
\ a clean exit (rc 0) is ok(captured); any nonzero completion is err(failed)
\ carrying the code. Neither arm drops the lengths.
: PROC-CAPTURE>RESULT ( len len rc -- result<pcap:captured,pcap:failed> )
   {: o:len e:len r:rc :}
   r RC>N 0 = if
      o e PCAP-CAPTURED:MAKE RESULT:OK
   else
      o e r PCAP-FAILED:MAKE RESULT:ERR
   then ;

: PROC-CAPTURE-RC@ ( -- result<pcap:captured,pcap:failed> )
   PROC-OUT-LEN @ >LEN PROC-ERR-LEN @ >LEN PROC-RC @ >RC PROC-CAPTURE>RESULT ;

\ The capture outcome is DERIVED, never stored: the machine keeps only the
\ raw wait status plus the timed-out flag (both one cell), so no (kind code)
\ pair state exists to drift from the truth.
: PROC-CAPTURE-OUTCOME ( -- outcome )
   PROC-TIMED-OUT @ 0 <> if OUTCOME:TIMEOUT exit then
   PROC-STATUS @ PROC-STATUS>OUTCOME ;

: PROC-CAPTURE-OUTCOME@ ( -- len len outcome )
   PROC-OUT-LEN @ >LEN PROC-ERR-LEN @ >LEN
   PROC-CAPTURE-OUTCOME ;

: PROC-CAPTURE-FINISH-RC ( -- result<pcap:captured,pcap:failed> )
   PROC-CLOSE-ALL-CAPTURE-FDS
   PROC-REAP-CAPTURE
   PROC-CAPTURE-RC@ ;

: PROC-CAPTURE-FINISH-OUTCOME ( -- len len outcome )
   PROC-CLOSE-ALL-CAPTURE-FDS
   PROC-CAPTURE-OUTCOME@ ;

: PROC-SPAWN-CAPTURE ( ptr u8 -- )
   PROC-NO-FD >FD PROC-OUT-W @ >FD PROC-ERR-W @ >FD PROC-SPAWN-RAW {: pid :}
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-CAPTURE-PID!
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: RUN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   pathu LEN>N 0 < if E-PROC-OUTPUT throw then
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-PATHZ {: pathz:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz PROC-SPAWN-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC ;

: RUN-CAPTURE-OUTCOME ( ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   pathu LEN>N 0 < if E-PROC-OUTPUT throw then
   outcap errcap PROC-CAPTURE-CHECK-CAPS
   path pathu PROC-PATHZ {: pathz:ptr :}
   timeout PROC-CAPTURE-BEGIN
   pathz PROC-SPAWN-CAPTURE
   out outcap err errcap PROC-RUN-CAPTURE-OUTCOME-LOOP
   PROC-CAPTURE-FINISH-OUTCOME ;
