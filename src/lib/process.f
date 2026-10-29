\ process.f - spawn, capture, poll and reap child processes.
\ The path buffer, pollfd array and capture slots are one PROC-STORAGE-BYTES
\ TASK:+USER row per task, so any number of tasks run children at once; the
\ PROC-REAP-ARM and PROC-STOP vectors are process-wide policies. Spawn requests
\ are src/lib/spawn.f's, tree kills src/lib/process-tree.f's; process-argv.f,
\ process-env.f and process-cwd.f add spawn shapes.

s" src/lib/errors.f" required
s" src/lib/memory.f" required                \ MEM-MAX-N bounds PROC-DEADLINE-AT
s" src/lib/adt/result.f" required            \ result<n,n> for PROC-RUN-IO-RC
s" src/lib/task.f" required                  \ TASK:+USER carries the per-call row
s" src/lib/fd-lock.f" required               \ FD-LOCK:WITH orders descriptor creation against spawns
s" src/lib/spawn.f" required                 \ PROCESS:request and PROCESS:SPAWN, the spawn every RAW word makes
s" src/lib/process-tree.f" required          \ PROC-TREE:KILL-TREE ends a capture child's tree

\ outcome - how a child completed. timeout carries nothing: no terminal status
\ was collected before the deadline. After a timeout a capture has killed its
\ child's tree with SIGKILL and reaped the child; PROC-WAIT-BOUNDED leaves the
\ child running and the pid with its caller to reap. A reader that turns an
\ outcome into a status or a test verdict throws E-PROC-TIMEOUT for a timeout
\ (PROC-OUTCOME>RC, the asserts in src/lib/test/outcome.f), so a deadline under
\ a gate row leaves the row uncaught; only a caller that acts on the deadline
\ itself MATCHes it as data.
SUMTYPE outcome
  VARIANT exited n ;VARIANT
  VARIANT signaled n ;VARIANT
  VARIANT timeout ;VARIANT
;SUMTYPE

\ pcap - a bounded capture's two output lengths: captured on a clean exit, failed
\ on a nonzero completion with its code (a nonzero exit code or 128+signal, never
\ an errno: spawn and wait failures throw E-PROC-SPAWN and E-PROC-WAIT). The
\ lengths are valid on both arms; callers replay the output either way.
package PCAP
public
STRUCTURE captured
  FIELD out len
  FIELD err len
;STRUCTURE
STRUCTURE failed
  FIELD out len
  FIELD err len
  FIELD code rc
;STRUCTURE
private
;package

PATH-CAP 1 + constant PROC-PATHZ-CAP     \ a path of PATH-CAP bytes and its NUL
1000000 constant PROC-NS-PER-MS
1 constant POLLIN
4 constant POLLOUT
8 constant POLLERR
16 constant POLLHUP
32 constant POLLNVAL
4 constant EINTR#                  \ "interrupted by a signal" (POSIX, identical on Linux/macOS)
3 constant ESRCH#                  \ process unavailable (Linux/macOS)
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
   HB-TARGET-LINUX-KERNEL? if $800 exit then
   HB-TARGET-MACOS? if 4 exit then
   E-PROC-HOST throw ;

\ F_DUPFD_CLOEXEC is not one number either: 1030 (F_LINUX_SPECIFIC_BASE + 6) on
\ both Linux targets, 67 on macOS.
: F-DUPFD-CLOEXEC ( -- n )
   HB-TARGET-LINUX-KERNEL? if 1030 exit then
   HB-TARGET-MACOS? if 67 exit then
   E-PROC-HOST throw ;


-1 constant PROC-NO-FD                   \ a closed / never-opened descriptor cell
-1 constant PROC-NO-PID                  \ no child, and no armed reaper

\ The per-task row, laid out like src/lib/net/tcp4.f's: a poll array shared by
\ two tasks hands each the other's descriptors.
\ A fresh task's row reads 0: every entry point resets it through
\ PROC-CAPTURE-RESET before any word reads a descriptor, and every pid guard is
\ `0 >`, treating 0 like PROC-NO-PID. PROC-KILL-CAPTURE also runs outside a
\ capture (tools/check-core.f answering a stop), where the row can read 0, and
\ kill(0) or waitpid(0) would reach the caller's whole process group.
5 constant PROC-PFD-SLOTS                \ stdout, stderr, stdin, stop, per-call cancellation
8 constant PROC-PFD-SLOT-BYTES           \ one struct pollfd: fd | events | revents
0 constant PROC-PATHZ-OFF
PROC-PATHZ-OFF PROC-PATHZ-CAP + 7 + $FFFFFFFFFFFFFFF8 and constant PROC-PFD-OFF
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
PROC-REAP-PID-OFF 1 cells + constant PROC-CANCEL-FD-OFF
PROC-CANCEL-FD-OFF 1 cells + constant PROC-RAW-PID-OFF
PROC-RAW-PID-OFF 1 cells + constant PROC-STORAGE-BYTES

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and PROC-STORAGE-BYTES TASK:+USER PROC-STORAGE drop

: PROC-ROW ( -- ptr u8 ) PROC-STORAGE BYTE-VIEW ;

: PROC-PATHZ-BUF ( -- ptr u8 ) PROC-ROW PROC-PATHZ-OFF + ;
: PROC-PFD ( -- ptr u8 ) PROC-ROW PROC-PFD-OFF + ;
: PROC-PROBE ( -- ptr u8 ) PROC-ROW PROC-PROBE-OFF + ;

\ TASK:+USER rows are `ptr n`: the fd, pid, rc, len and off roles are converted
\ where a value enters a word that names the role.
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
: PROC-REAP-PID ( -- ptr n ) PROC-ROW PROC-REAP-PID-OFF + CELL-VIEW ;
\ Borrowed, never read, duplicated or closed. CMD installs it after BEGIN.
: PROC-CANCEL-FD ( -- ptr n ) PROC-ROW PROC-CANCEL-FD-OFF + CELL-VIEW ;

: PROC-WAIT-STATUS-RAW ( pid -- n ) {: pid :}
   pid PID>N wait-status ;

PROCESS-SYMBOLS
FUNCTION: PROC-WAITPID-CALL waitpid ( n ptr u8 n -- i32 )
   1 4 WRITES-BYTES                    \ wait status is a C int, not a cell
;FUNCTION

\ The bodies the RAW spawn words run under catch. Each builds a HERMETIC PROCESS
\ request, so the child's environment is only the rows it is given; a RAW word
\ answers the pid, or the negated errno of a refused spawn. process-argv.f,
\ process-env.f and process-cwd.f add the bodies for their shapes.
\ SPAWN. A spawned child leads a process group of its own before it execs: macOS
\ spawns with POSIX_SPAWN_SETPGROUP; on Linux the child calls setpgid(0,0) and the
\ parent answers a pid only once a close-on-exec pipe reports the exec or its
\ failure. A parent-side setpgid would race; the gate pool's reaper relies on
\ this. The child's fds 0, 1 and 2 are the fds a RAW word names; a negative one
\ inherits the parent's fd of that number while it is open, close-on-exec or
\ not, and the child gets no other descriptor. A forked child
\ (src/lib/process-fork.f) copies every descriptor and keeps across exec those
\ not marked close-on-exec: mark parent-only pipe and pty ends close-on-exec, or
\ that child holds a writer and the parent never sees EOF. src/lib/fd-lock.f
\ orders the mark against forks.
package PROC-RAW
private

CAST: >ROWS ( n -- ptr ptr u8 )

: ROWS ( ptr a -- ptr ptr u8 ) NULL-PTR - >ROWS ;

: PID ( -- ptr n ) PROC-ROW PROC-RAW-PID-OFF + CELL-VIEW ;

: FD ( PROCESS:request n fd -- PROCESS:request ) {: child parent :}
   parent FD>N 0 >= if child >FD parent PROCESS:FD then ;

: STDIO ( PROCESS:request fd fd fd -- PROCESS:request ) {: infd outfd errfd :}
   0 infd FD
   1 outfd FD
   2 errfd FD ;

: LAUNCH ( PROCESS:request fd fd fd -- )
   STDIO PROCESS:SPAWN PID>N PID ! ;

public

: ANSWER ( n -- pid ) {: rc :}
   rc 0= if PID @ >PID exit then
   rc E-PROC-SPAWN <> if rc throw then
   PROCESS:ERRNO negate >PID ;

: SPAWN ( ptr u8 fd fd fd -- ptr u8 fd fd fd )
   {: pathz:ptr infd outfd errfd :}
   pathz dup ZLEN PROCESS:NEW PROCESS:HERMETIC
   infd outfd errfd LAUNCH
   pathz infd outfd errfd ;

;package

: PROC-SPAWN-RAW ( ptr u8 fd fd fd -- pid )
   [: PROC-RAW:SPAWN ;] catch {: rc :}
   drop drop drop drop
   rc PROC-RAW:ANSWER ;

: PROC-KILL-RAW ( pid n -- rc ) {: pid sig :}
   pid PID>N sig kill >RC ;

: PROC-ZCOPY ( ptr u8 len ptr u8 len -- ptr u8 ) {: a:ptr u dst:ptr cap :}
   u LEN>N 0 < if E-PROC-OUTPUT throw then
   u LEN>N cap LEN>N >= if E-PROC-OUTPUT throw then
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

\ The child's rc: its exit code, or 128 + the signal that ended it, so a
\ deliberate kill reads 137. A timeout throws E-PROC-TIMEOUT: read as the 137 of
\ its SIGKILL, an assertion on it would fail like any wrong answer.
: PROC-OUTCOME>RC ( outcome -- rc )
   MATCH outcome
     exited OF >RC ENDOF
     signaled OF 128 + >RC ENDOF
     timeout OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH ;

: PROC-STATUS>RC ( n -- rc )
   PROC-STATUS>OUTCOME PROC-OUTCOME>RC ;

\ A throw code does not cross a process boundary: `die` and the uncaught-throw
\ exit turn every negative code into UNCAUGHT-RC (67). A tool whose work can end
\ on a deadline exits PROC-TIMEOUT-RC for E-PROC-TIMEOUT, and the process that
\ runs it throws E-PROC-TIMEOUT again on that status, so the verdict reaches the
\ top of a chain of tools. 124 is coreutils `timeout`'s status; no exit-status
\ constant, `die` literal or positive throw code in src/ or tools/ is 124, and
\ a signal death reads as 128 + signal.
124 constant PROC-TIMEOUT-RC

: PROC-EXIT-RC ( n n -- n ) {: code:n fail:n :}
   code 0= if 0 exit then
   code E-PROC-TIMEOUT = if PROC-TIMEOUT-RC exit then
   fail ;

\ PROC-OUTCOME>RC with a timeout read as PROC-TIMEOUT-RC instead of thrown: a
\ caller that throws E-PROC-TIMEOUT again on that rc reports this deadline and
\ one that expired inside the child alike.
: PROC-OUTCOME>DEADLINE-RC ( outcome -- rc )
   MATCH outcome
     exited OF >RC ENDOF
     signaled OF 128 + >RC ENDOF
     timeout OF PROC-TIMEOUT-RC >RC ENDOF
   ;MATCH ;

: PROC-WAIT-OUTCOME ( pid -- outcome )
   PROC-WAIT-STATUS PROC-STATUS>OUTCOME ;

\ err is the nonzero completion rc (an exit code, or 128+signal); a wait failure
\ throws E-PROC-WAIT inside PROC-WAIT-STATUS.
: PROC-WAIT-RC ( pid -- result<n,n> )
   PROC-WAIT-STATUS PROC-STATUS>RC RC>N {: rc:n :}
   rc 0 = if rc RESULT:OK else rc RESULT:ERR then ;

: PROC-SPAWN-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PROC-PATHZ infd outfd errfd PROC-SPAWN-RAW {: pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: PROC-RUN-IO-RC ( ptr u8 len fd fd fd -- result<n,n> )
   PROC-SPAWN-IO PROC-WAIT-RC ;

: PROC-RUN-RC ( ptr u8 len -- result<n,n> )
   -1 >FD -1 >FD -1 >FD PROC-RUN-IO-RC ;

: FD-CLOEXEC! ( fd -- ) {: fd :}
   fd FD>N F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT throw then ;

: FD-NOSIGPIPE! ( fd -- ) {: fd :}
   fd FD>N F-SETNOSIGPIPE 1 fcntl 0 <> if E-PROC-OUTPUT throw then ;

: PIPE-PAIR ( -- fd fd )
   pipe {: r w rc :}
   rc 0 <> if E-PROC-OUTPUT throw then
   r >FD w >FD ;

: PIPE-PAIR-CLOEXEC-HELD ( -- fd fd )
   PIPE-PAIR {: r:fd w:fd :}
   r FD-CLOEXEC!
   w FD-CLOEXEC!
   r w ;

: PIPE-PAIR-CLOEXEC ( -- fd fd )
   [: PIPE-PAIR-CLOEXEC-HELD ;] FD-LOCK:WITH ;

: PROC-PFD-SLOT ( idx -- ptr n ) {: idx :}
   idx IDX>N PROC-PFD-SLOT-BYTES * PROC-PFD + CELL-VIEW ;

: PROC-PFD-AT! ( fd n idx -- ) {: fd events idx :}
   events 32 lshift  fd FD>N $FFFFFFFF and  or  idx PROC-PFD-SLOT ! ;

: PROC-PFD! ( fd n -- ) {: fd events :}
   fd events 0 >IDX PROC-PFD-AT! ;

: PROC-PFD-REVENTS ( idx -- n )
   PROC-PFD-SLOT @ 48 rshift $FFFF and ;

: PROC-LEFT-MS ( n -- ms ) {: deadline :}
   deadline mono-ns - dup 0 <= if
      drop 0 >MS
   else
      PROC-NS-PER-MS / >MS
   then ;

\ A negative ms names no window: its deadline is the reading itself, and a poll
\ caller hands the negative ms to poll(2) for an unbounded wait. A window whose
\ nanoseconds would pass the largest cell is E-PROC-TIMEOUT before anything is
\ multiplied: it never wraps into the past or saturates to forever.
: PROC-DEADLINE-AT ( ms -- n ) {: timeout:ms :}
   mono-ns {: now:n :}
   timeout MS>N 0 < if now exit then
   timeout MS>N MEM-MAX-N now - PROC-NS-PER-MS / > if E-PROC-TIMEOUT throw then
   timeout MS>N PROC-NS-PER-MS * now + ;

: PROC-POLL-ONCE ( n n -- n ) {: nfds ms :}
   PROC-PFD nfds ms poll ;

\ poll(2) is the one blocking call SA_RESTART never restarts: -EINTR means a
\ signal landed before any event and nothing was consumed. Each restart waits
\ out only what is left of the deadline, so no signal storm pushes a capture
\ past its timeout; a passed deadline reports the zero-event timeout. Any other
\ negative rc is an errno the caller names through its own E-PROC-* code. A
\ negative initial timeout stays unbounded across interruptions.
: PROC-POLL-RESTART ( n n n -- n ) {: nfds ms deadline :}
   nfds ms PROC-POLL-ONCE {: rc :}
   rc EINTR# negate <> if rc exit then
   begin
      ms 0 < if ms else deadline PROC-LEFT-MS MS>N then {: left :}
      left 0= if 0 exit then
      nfds left PROC-POLL-ONCE {: restarted :}
      restarted EINTR# negate <> if restarted exit then
   again ;

\ A signal is reported as -EINTR: this word owns no deadline to restart against.
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

\ waitpid's flag and pid_t bound, the same on Linux and macOS.
1 constant WNOHANG
$7FFFFFFF constant PROC-PID-MAX    \ the largest signed 32-bit pid_t

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 1 cells TASK:+USER PROC-BOUNDED-STATUS drop

\ A zero return leaves the cleared cell unread; a stop (status low bits $7F,
\ which only a traced child reports without WUNTRACED) ends nothing. EINTR
\ collects nothing; any other failure, ECHILD included, is E-PROC-WAIT. waitpid
\ writes a C int, so the whole cell is cleared first.
: PROC-WAIT-PROBE ( pid -- bool )
   {: pid:pid :}
   0 PROC-BOUNDED-STATUS !
   pid PID>N PROC-BOUNDED-STATUS BYTE-VIEW WNOHANG PROC-WAITPID-CALL {: got:n :}
   got pid PID>N = if
      PROC-BOUNDED-STATUS @ PROC-WAIT-TERM-MASK and PROC-WAIT-TERM-MASK <> exit
   then
   got 0= if 0 0= 0= exit then
   FFI:ERRNO EINTR# <> if E-PROC-WAIT throw then
   0 0= 0= ;

\ A timeout collects nothing and leaves the pid the caller's to reap. The wait
\ sends no signal, closes no descriptor and leaves the capture row and reaper
\ alone. pid must be a pid_t above zero: waitpid's C int would narrow a wider
\ cell to another process or a process group; both arguments are refused before
\ any waitpid. Each turn probes before it reads the deadline, so ms 0 is one
\ probe and a status already there wins; PROC-LEFT-MS floors, so a window can
\ end under a millisecond early.
: PROC-WAIT-BOUNDED ( pid ms -- outcome )
   {: pid:pid timeout:ms :}
   pid PID>N 0 <= if E-PROC-WAIT throw then
   pid PID>N PROC-PID-MAX > if E-PROC-WAIT throw then
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then
   timeout PROC-DEADLINE-AT {: deadline:n :}
   begin pid PROC-WAIT-PROBE 0= while
      deadline PROC-LEFT-MS MS>N 0= if OUTCOME:TIMEOUT exit then
      1 >MS TASK:SLEEP
   repeat
   PROC-BOUNDED-STATUS @ PROC-STATUS>OUTCOME ;

\ A context that owns a death-watch fd (a pool worker's worker-alive read end;
\ src/lib/process-fork.f installs the live vector) arms a reaper for the
\ just-spawned capture child, so the child's group dies with the arming process.
\ The default arms nothing and returns PROC-NO-PID. A vector that cannot arm
\ throws, and PROC-CAPTURE-PID! refuses the capture. The reaper is a direct
\ child of this process and every capture-termination path kills and waits it
\ by pid, so no wait(-1) caller sees a stray child. Process-wide: one policy
\ the image installs at load.
defer PROC-REAP-ARM ( pid -- pid )
: PROC-REAP-ARM-OFF ( pid -- pid )
   drop PROC-NO-PID >PID ;
: PROC-REAP-ARM-DEFAULT ( -- )
   [: PROC-REAP-ARM-OFF ;] is PROC-REAP-ARM ;
PROC-REAP-ARM-DEFAULT

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
   PROC-NO-FD PROC-CANCEL-FD !
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
   PROC-PID @ dup 0 > if
      >PID PROC-WAIT-STATUS dup PROC-STATUS !
      PROC-STATUS>RC RC>N PROC-RC !
      PROC-NO-PID PROC-PID !
   else
      drop
   then
   PROC-REAP-DISARM ;

\ A child is killed for its caller's reason (a capture's deadline, an overflow,
\ a refused reaper arm, a stop, a pty reap's expiry, a session's teardown) and
\ the caller acts on it: tools/check.f names a run past its deadline, and
\ src/lib/process-pty-io.f's scope hands its slot back. A tree walk that throws
\ there must neither replace that reason nor vanish: stderr gets one line naming
\ its code, written in pieces from constant text so it takes no storage two
\ tasks could share.

: PROC-DIAG ( ptr u8 n -- )
   {: a:ptr u:n :}
   2 a u write drop ;

\ v is at most 0, so MIN-N has a magnitude too: / and mod truncate toward zero.
: PROC-DIAG-DIGITS ( n -- )
   {: v:n :}
   v -10 <= if v 10 / RECURSE then
   s" 0123456789" drop v 10 mod negate + 1 PROC-DIAG ;

: PROC-WALK-REPORT ( n -- )
   {: code:n :}
   code 0= if exit then
   s" process: process tree of a killed child not walked, throw " PROC-DIAG
   code 0 < if s" -" PROC-DIAG code else code negate then PROC-DIAG-DIGITS
   s\" \n" PROC-DIAG ;

\ Never throws. A spawned child leads a process group of its own, and so does
\ each process it spawns, so no kill of its pid or group reaches them;
\ src/lib/process-tree.f KILL-TREE freezes the tree before it lists it. A walk
\ that throws has still killed every member it found, the child first, so the
\ caller reaps the child all the same. The pid rides through the catch, so both
\ arms leave the same depth.
: PROC-KILL-TREE ( pid -- )
   [: dup PROC-TREE:KILL-TREE ;] catch PROC-WALK-REPORT drop ;

: PROC-KILL-CAPTURE ( -- )
   PROC-PID @ 0 > if
      PROC-PID @ >PID PROC-KILL-TREE
      PROC-REAP-CAPTURE
   then ;

\ The tree is killed before the capture descriptors close, and they close
\ whatever the kill throws: closing first could end the child before the walk
\ froze it, leaving what it spawned to init.
: PROC-END-CAPTURE ( -- )
   [: PROC-KILL-CAPTURE ;] [: PROC-CLOSE-ALL-CAPTURE-FDS ;] finally ;

: PROC-REAP-CAPTURE-TIMEOUT ( -- )
   PROC-END-CAPTURE
   1 PROC-TIMED-OUT ! ;

: PROC-THROW-CAPTURE ( n -- ) {: code :}
   PROC-END-CAPTURE
   code throw ;

\ A refused reaper arm refuses the capture: the child's tree is killed, the
\ child reaped and the descriptors closed before the code goes on.
: PROC-CAPTURE-PID! ( pid -- ) {: pid:pid :}
   pid PID>N PROC-PID !
   pid [: PROC-REAP-ARM ;] catch {: rpid code:n :}
   code 0= if rpid PID>N PROC-REAP-PID ! exit then
   code PROC-THROW-CAPTURE ;

: PROC-CLOEXEC-CELL ( ptr n -- ) {: p:ptr :}
   p @ F-SETFD FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then ;

: PROC-OPEN-PIPE-HELD ( ptr n ptr n -- ) {: rp:ptr wp:ptr :}
   pipe {: r w rc :}
   rc 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   r rp !
   w wp !
   rp PROC-CLOEXEC-CELL
   wp PROC-CLOEXEC-CELL ;

: PROC-OPEN-PIPE ( ptr n ptr n -- )
   [: PROC-OPEN-PIPE-HELD ;] FD-LOCK:WITH ;

: PROC-CAPTURE-NULL-INPUT-HELD ( -- )
   s\" /dev/null\z" drop open-rd
   dup 0 < if drop E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-IN-R !
   PROC-IN-R PROC-CLOEXEC-CELL ;

\ A capture's default stdin is EOF, whatever the parent's terminal or pipe; the
\ fd sits in the capture row so a refused spawn closes it too.
: PROC-CAPTURE-NULL-INPUT ( -- fd )
   [: PROC-CAPTURE-NULL-INPUT-HELD ;] FD-LOCK:WITH
   PROC-IN-R @ >FD ;

: PROC-NONBLOCK! ( fd -- ) {: fd :}
   fd FD>N F-GETFL 0 fcntl {: flags :}
   flags 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   fd FD>N F-SETFL flags O-NONBLOCK or fcntl 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then ;


: PROC-NOSIGPIPE! ( fd -- )
   {: fd :}
   fd FD>N F-SETNOSIGPIPE 1 fcntl 0 <> if E-PROC-OUTPUT PROC-THROW-CAPTURE then ;


: PROC-SETUP-CAPTURE-FDS ( -- )
   PROC-OUT-R PROC-OUT-W PROC-OPEN-PIPE
   PROC-ERR-R PROC-ERR-W PROC-OPEN-PIPE ;

: PROC-SETUP-STDIN-FDS ( -- )
   PROC-IN-R PROC-IN-W PROC-OPEN-PIPE
   PROC-IN-W @ >FD PROC-NOSIGPIPE!
   PROC-IN-W @ >FD PROC-NONBLOCK! ;

package PROCESS
public
: WAIT-FOREVER ( -- ms ) MEM-MAX-N >MS ;
;package

: PROC-CAPTURE-DEADLINE! ( ms -- ) {: timeout :}
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then
   timeout MS>N MEM-MAX-N = if MEM-MAX-N PROC-DEADLINE ! exit then
   timeout PROC-DEADLINE-AT PROC-DEADLINE ! ;

\ With no deadline this answers a negative ms: the capture loops never see zero
\ left, and poll(2) waits unbounded (PROC-POLL-RESTART).
: PROC-REMAINING-MS ( -- ms )
   PROC-DEADLINE @ MEM-MAX-N = if -1 >MS exit then
   PROC-DEADLINE @ PROC-LEFT-MS ;

\ A program that answers a stop signal (src/lib/signal.f CATCH-STOPS) must hear
\ it while a capture polls: every capture poll watches PROC-STOP-FD in the slot
\ after stdin's and runs PROC-STOP when it is readable, and the reap after the
\ pipes close asks between its turns. PROC-STOP ends the process when it takes
\ a signal and returns when it finds none; one that throws ends the capture.
\ The default is no descriptor, which poll passes over. Process-wide, like
\ PROC-REAP-ARM: tools/check-core.f CHECK:MAIN installs it.
3 constant PROC-STOP-SLOT

variable PROC-STOP-FD
PROC-NO-FD PROC-STOP-FD !

defer PROC-STOP ( -- )
: PROC-STOP-NONE ( -- ) ;
: PROC-STOP-DEFAULT ( -- )
   [: PROC-STOP-NONE ;] is PROC-STOP ;
PROC-STOP-DEFAULT

: PROC-ARM-STOP-PFD ( -- )
   PROC-STOP-FD @ >FD POLLIN PROC-STOP-SLOT >IDX PROC-PFD-AT! ;

: PROC-STOP-HEARD ( -- )
   PROC-STOP-SLOT >IDX PROC-PFD-REVENTS 0= if exit then
   [: PROC-STOP ;] catch {: code:n :}
   code 0<> if code PROC-THROW-CAPTURE then ;

4 constant PROC-CANCEL-SLOT

: PROC-ARM-CANCEL-PFD ( -- )
   PROC-CANCEL-FD @ >FD POLLIN PROC-CANCEL-SLOT >IDX PROC-PFD-AT! ;

: PROC-CANCEL-HEARD ( -- )
   PROC-CANCEL-SLOT >IDX PROC-PFD-REVENTS {: events:n :}
   events POLLERR POLLNVAL or and 0<> if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   events POLLIN POLLHUP or and 0<> if E-PROC-CANCELLED PROC-THROW-CAPTURE then ;

\ A capture without stdin leaves that slot empty: every capture poll covers
\ every slot, the stop descriptor's included.
: PROC-ARM-CAPTURE-PFD ( -- )
   PROC-OUT-R @ >FD POLLIN 0 >IDX PROC-PFD-AT!
   PROC-ERR-R @ >FD POLLIN 1 >IDX PROC-PFD-AT!
   PROC-NO-FD >FD 0 2 >IDX PROC-PFD-AT! ;

\ Where one poll ends: its own window, never past the capture deadline, so a
\ restart after a signal waits out what is left of the window.
: PROC-POLL-END ( n -- n ) {: ms:n :}
   ms 0 < if PROC-DEADLINE @ exit then
   ms >MS PROC-DEADLINE-AT PROC-DEADLINE @ min ;

: PROC-POLL-CAPTURE-RC ( n -- n ) {: ms:n :}
   PROC-ARM-STOP-PFD
   PROC-ARM-CANCEL-PFD
   PROC-PFD-SLOTS ms ms PROC-POLL-END PROC-POLL-RESTART {: rc:n :}
   rc 0 > if PROC-STOP-HEARD PROC-CANCEL-HEARD then
   rc ;

: PROC-POLL-CAPTURE ( ms -- count ) {: ms :}
   PROC-ARM-CAPTURE-PFD
   ms MS>N PROC-POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT PROC-THROW-CAPTURE then
   rc >COUNT ;

: PROC-POLL-CAPTURE-OUTCOME ( ms -- count ) {: ms :}
   PROC-ARM-CAPTURE-PFD
   ms MS>N PROC-POLL-CAPTURE-RC {: rc :}
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

\ A read that fills the span may leave an overflow byte already in the pipe.
\ Probe it without waiting, before an expired deadline ends the capture. This
\ uses poll slot 0 after its read; stderr's slot remains available to the drain.
: PROC-PROBE-READY-FULL-STREAM ( ptr n -- ) {: fdp:ptr :}
   fdp @ >FD 0 >MS POLL-IN COUNT>N {: ready:n :}
   ready EINTR# negate = if exit then
   ready 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   ready 0 > if fdp PROC-PROBE-FULL-STREAM then ;

: PROC-READ-OR-PROBE-STREAM ( ptr n ptr u8 len ptr n -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap LEN>N lenp @ - 0 <= if
      fdp PROC-PROBE-FULL-STREAM
   else
      fdp buf cap lenp PROC-READ-STREAM
      cap LEN>N lenp @ = if fdp PROC-PROBE-READY-FULL-STREAM then
   then ;

: PROC-DRAIN-READY ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   0 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-OUT-R out outcap PROC-OUT-LEN PROC-READ-OR-PROBE-STREAM
   then
   1 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-ERR-R err errcap PROC-ERR-LEN PROC-READ-OR-PROBE-STREAM
   then ;

\ A growing capture keeps an output stream in caller-owned storage: SINK, given
\ a byte count, makes it hold at least that many bytes, keeping its bytes, and
\ answers its byte 0, as a DYNAMIC-BUFFER u8's reserve then byte 0 do. A
\ refused growth fails the capture as a read failure does.
$10000 constant PROC-GROW-CHUNK

\ SINK's first byte replaces the null one PROC-READ-GROWING hands in, since
\ catch keeps the shape of the stack.
: PROC-SINK-ROOM ( ptr u8 [ n -- ptr u8 ] ptr n -- ptr u8 [ n -- ptr u8 ] ptr n )
   {: old:ptr sink lenp:ptr :}
   lenp @ PROC-GROW-CHUNK + sink execute sink lenp ;

: PROC-READ-INTO-ROOM ( ptr n ptr u8 ptr n -- )
   {: fdp:ptr buf:ptr lenp:ptr :}
   fdp @ buf lenp @ + PROC-GROW-CHUNK read PROC-RD !
   PROC-RD @ 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ PROC-GROW-CHUNK > if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   PROC-RD @ 0= if
      fdp PROC-CLOSE-CELL
   else
      lenp @ PROC-RD @ + lenp !
   then ;

: PROC-READ-GROWING ( ptr n ptr n [ n -- ptr u8 ] -- )
   {: fdp:ptr lenp:ptr sink :}
   NULL$ drop sink lenp [: PROC-SINK-ROOM ;] catch {: buf:ptr s lp:ptr code:n :}
   code 0= if fdp buf lp PROC-READ-INTO-ROOM exit then
   code PROC-THROW-CAPTURE ;

: PROC-DRAIN-READY-GROWING ( [ n -- ptr u8 ] ptr u8 len -- )
   {: sink err:ptr errcap :}
   0 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-OUT-R PROC-OUT-LEN sink PROC-READ-GROWING
   then
   1 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-ERR-R err errcap PROC-ERR-LEN PROC-READ-OR-PROBE-STREAM
   then ;

: PROC-DRAIN-READY-GROWING-BOTH ( [ n -- ptr u8 ] [ n -- ptr u8 ] -- )
   {: out err :}
   0 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-OUT-R PROC-OUT-LEN out PROC-READ-GROWING
   then
   1 >IDX PROC-PFD-REVENTS 0 <> if
      PROC-ERR-R PROC-ERR-LEN err PROC-READ-GROWING
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

\ A refused write on the non-blocking end is back pressure: the child stopped
\ reading. The engine collapses every write errno to -1 and the caller has
\ already asked poll whether the reader is gone, so the offset stays and the
\ next POLLOUT under the deadline resumes; a short write advances by what
\ landed. Closing here would truncate the child's stdin.
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
   ms MS>N PROC-POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT PROC-THROW-CAPTURE then
   rc >COUNT ;

: PROC-POLL-IO-OUTCOME ( ms -- count ) {: ms :}
   PROC-ARM-IO-PFD
   ms MS>N PROC-POLL-CAPTURE-RC {: rc :}
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

\ Interrupted zero-time polls retry without waiting out the deadline. CMD calls
\ this before spawning, so an interruption cannot skip a pending cancellation.
: PROC-STOP-ASK ( -- )
   PROC-ARM-IO-PFD
   PROC-ARM-STOP-PFD
   PROC-ARM-CANCEL-PFD
   begin
      PROC-PFD-SLOTS 0 PROC-POLL-ONCE
      dup EINTR# negate =
   while drop repeat {: rc:n :}
   rc 0 < if E-PROC-OUTPUT PROC-THROW-CAPTURE then
   rc 0 > if PROC-STOP-HEARD PROC-CANCEL-HEARD then ;

\ EOF says the child closed its pipes, not that it exited: its status is waited
\ for under the same deadline, and only the timeout path waits blocking, after
\ SIGKILL. waitpid writes four bytes, so the cell is cleared.
: PROC-REAP-CAPTURE-BOUNDED ( -- )
   begin PROC-PID @ 0 > while
      0 PROC-STATUS !
      PROC-PID @ PROC-STATUS BYTE-VIEW 1 PROC-WAITPID-CALL {: got:n :}
      got PROC-PID @ = if
         PROC-STATUS @ PROC-STATUS>RC RC>N PROC-RC !
         PROC-NO-PID PROC-PID !
      else
         got 0<> if
            FFI:ERRNO EINTR# <> if E-PROC-WAIT PROC-THROW-CAPTURE then
         then
         PROC-REMAINING-MS MS>N {: left:n :}
         left 0= if PROC-REAP-CAPTURE-TIMEOUT exit then
         PROC-STOP-ASK
         \ A halt at PAUSE would unwind out of this loop and leave the child
         \ unreaped. SLEEP returns here to reap it, and does not burn a core while
         \ the child runs.
         1 >MS TASK:SLEEP
      then
   repeat
   PROC-REAP-DISARM ;

\ One ready read per stream survives an expired deadline. The check after the
\ drain ends the capture before another poll, even when a child floods output.
: PROC-RUN-CAPTURE-LOOP ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   begin PROC-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-CAPTURE drop
      out outcap err errcap PROC-DRAIN-READY
      PROC-REMAINING-MS MS>N 0= PROC-CAPTURE-DONE? 0= and if
         E-PROC-TIMEOUT PROC-THROW-CAPTURE
      then
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
      PROC-REMAINING-MS MS>N 0= PROC-CAPTURE-DONE? 0= and if
         PROC-REAP-CAPTURE-TIMEOUT exit
      then
   repeat
   PROC-REAP-CAPTURE-BOUNDED ;

: PROC-RUN-STDIN-CAPTURE-LOOP ( ptr u8 len ptr u8 len ptr u8 len -- )
   {: in:ptr inu out:ptr outcap err:ptr errcap :}
   inu LEN>N 0 <= if PROC-IN-W PROC-CLOSE-CELL then
   begin PROC-STDIN-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-IO drop
      in inu PROC-DRIVE-STDIN
      out outcap err errcap PROC-DRAIN-READY
      PROC-REMAINING-MS MS>N 0= PROC-STDIN-CAPTURE-DONE? 0= and if
         E-PROC-TIMEOUT PROC-THROW-CAPTURE
      then
   repeat ;

: PROC-RUN-STDIN-CAPTURE-OUTCOME-LOOP ( ptr u8 len ptr u8 len ptr u8 len -- )
   {: in:ptr inu out:ptr outcap err:ptr errcap :}
   inu LEN>N 0 <= if PROC-IN-W PROC-CLOSE-CELL then
   begin PROC-STDIN-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-IO-OUTCOME dup COUNT>N 0= if
         drop
         PROC-REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      in inu PROC-DRIVE-STDIN
      out outcap err errcap PROC-DRAIN-READY
      PROC-REMAINING-MS MS>N 0= PROC-STDIN-CAPTURE-DONE? 0= and if
         PROC-REAP-CAPTURE-TIMEOUT exit
      then
   repeat
   PROC-REAP-CAPTURE-BOUNDED ;

: PROC-RUN-STDIN-GROWING-CAPTURE-OUTCOME-LOOP ( ptr u8 len [ n -- ptr u8 ] ptr u8 len -- )
   {: in:ptr inu sink err:ptr errcap :}
   inu LEN>N 0 <= if PROC-IN-W PROC-CLOSE-CELL then
   begin PROC-STDIN-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-IO-OUTCOME dup COUNT>N 0= if
         drop
         PROC-REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      in inu PROC-DRIVE-STDIN
      sink err errcap PROC-DRAIN-READY-GROWING
      PROC-REMAINING-MS MS>N 0= PROC-STDIN-CAPTURE-DONE? 0= and if
         PROC-REAP-CAPTURE-TIMEOUT exit
      then
   repeat
   PROC-REAP-CAPTURE-BOUNDED ;

: PROC-RUN-STDIN-GROWING-BOTH-CAPTURE-OUTCOME-LOOP ( ptr u8 len [ n -- ptr u8 ] [ n -- ptr u8 ] -- )
   {: in:ptr inu out err :}
   inu LEN>N 0 <= if PROC-IN-W PROC-CLOSE-CELL then
   begin PROC-STDIN-CAPTURE-DONE? 0= while
      PROC-REMAINING-MS PROC-POLL-IO-OUTCOME dup COUNT>N 0= if
         drop
         PROC-REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      in inu PROC-DRIVE-STDIN
      out err PROC-DRAIN-READY-GROWING-BOTH
      PROC-REMAINING-MS MS>N 0= PROC-STDIN-CAPTURE-DONE? 0= and if
         PROC-REAP-CAPTURE-TIMEOUT exit
      then
   repeat
   PROC-REAP-CAPTURE-BOUNDED ;

: PROC-CAPTURE-CHECK-CAP ( len -- )
   {: cap :}
   cap LEN>N 0 < if E-PROC-OUTPUT throw then ;

: PROC-CAPTURE-CHECK-CAPS ( len len -- ) {: outcap errcap :}
   outcap PROC-CAPTURE-CHECK-CAP
   errcap PROC-CAPTURE-CHECK-CAP ;

: PROC-CAPTURE-CHECK-STDIN ( len -- ) {: inu :}
   inu LEN>N 0 < if E-PROC-OUTPUT throw then ;

: PROC-CAPTURE-BEGIN ( ms -- ) {: timeout :}
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS ;

: PROC-STDIN-CAPTURE-BEGIN ( ms -- ) {: timeout :}
   timeout PROC-CAPTURE-BEGIN
   PROC-SETUP-STDIN-FDS ;

: PROC-CAPTURE>RESULT ( len len rc -- result<pcap:captured,pcap:failed> )
   {: o:len e:len r:rc :}
   r RC>N 0 = if
      o e PCAP-CAPTURED:MAKE RESULT:OK
   else
      o e r PCAP-FAILED:MAKE RESULT:ERR
   then ;

: PROC-CAPTURE-RC@ ( -- result<pcap:captured,pcap:failed> )
   PROC-OUT-LEN @ >LEN PROC-ERR-LEN @ >LEN PROC-RC @ >RC PROC-CAPTURE>RESULT ;

\ Derived from the raw wait status and the timed-out flag: no stored outcome can
\ drift from them.
: PROC-CAPTURE-OUTCOME ( -- outcome )
   PROC-TIMED-OUT @ 0 <> if OUTCOME:TIMEOUT exit then
   PROC-STATUS @ PROC-STATUS>OUTCOME ;

: PROC-CAPTURE-OUTCOME@ ( -- len len outcome )
   PROC-OUT-LEN @ >LEN PROC-ERR-LEN @ >LEN
   PROC-CAPTURE-OUTCOME ;

: PROC-CAPTURE-FINISH-RC ( -- result<pcap:captured,pcap:failed> )
   PROC-CLOSE-ALL-CAPTURE-FDS
   PROC-REAP-CAPTURE-BOUNDED
   PROC-TIMED-OUT @ 0<> if E-PROC-TIMEOUT throw then
   PROC-CAPTURE-RC@ ;

: PROC-CAPTURE-FINISH-OUTCOME ( -- len len outcome )
   PROC-CLOSE-ALL-CAPTURE-FDS
   PROC-CAPTURE-OUTCOME@ ;

\ Every spawn shape ends here: a failed spawn closes the capture descriptors and
\ throws; a live pid becomes the capture child, and the child's pipe ends close.
: PROC-CAPTURE-ADOPT-SPAWN ( pid -- ) {: pid :}
   pid PID>N 0 < if E-PROC-SPAWN PROC-THROW-CAPTURE then
   pid PROC-CAPTURE-PID!
   PROC-IN-R PROC-CLOSE-CELL
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PROC-SPAWN-CAPTURE ( ptr u8 -- )
   PROC-CAPTURE-NULL-INPUT PROC-OUT-W @ >FD PROC-ERR-W @ >FD PROC-SPAWN-RAW
   PROC-CAPTURE-ADOPT-SPAWN ;

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
