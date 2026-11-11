\ process.f - spawn, capture, poll and reap child processes.
\ The path buffer, pollfd array and capture slots are one STORAGE-BYTES
\ TASK:+USER row per task, so any number of tasks run children at once; the
\ REAP-ARM and STOP vectors are process-wide policies. Spawn requests
\ are src/lib/spawn.f's, tree kills src/lib/process-tree.f's; process-argv.f,
\ process-env.f and process-cwd.f add spawn shapes.

s" src/lib/error.f" required
s" src/lib/memory.f" required                \ MEM-MAX-N bounds DEADLINE-AT
s" src/lib/adt/result.f" required            \ result<n,n> for RUN-IO-RC
s" src/lib/task.f" required                  \ TASK:+USER carries the per-call row
s" src/lib/fd-lock.f" required               \ FD-LOCK:WITH orders descriptor creation against spawns
s" src/lib/fd.f" required                    \ FD:PIPE, the capture pipes
s" src/lib/spawn.f" required                 \ PROCESS:request and PROCESS:SPAWN, the spawn every RAW word makes
s" src/lib/process-tree.f" required          \ PROC-TREE:KILL-TREE ends a capture child's tree

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

package PROCESS
public
\ status - how an ended child ended: what a wait with no deadline answers.
ENUM status
  VARIANT exited FIELD code n ;VARIANT
  VARIANT signaled FIELD signal n ;VARIANT
;ENUM

\ ending - how the child's run ended, or that it had not by the deadline. After
\ a timeout a capture has ended its child's tree and reaped the child.
ENUM ending
  VARIANT exited FIELD code n ;VARIANT
  VARIANT signaled FIELD signal n ;VARIANT
  VARIANT timeout ;VARIANT
;ENUM

PATH-CAP 1 + constant PATHZ-CAP          \ a path of PATH-CAP bytes and its NUL
1000000 constant NS-PER-MS
3 constant ESRCH#                        \ process unavailable (Linux/macOS)
9 constant SIGKILL
private
$7F constant WAIT-TERM-MASK
$FF constant WAIT-EXIT-MASK
public
4096 constant STDIN-CHUNK-CAP

-1 constant NO-FD                        \ a closed / never-opened descriptor cell
-1 constant NO-PID                       \ no child, and no armed reaper

private
\ The per-task row, laid out like src/lib/net/tcp4.f's: a poll array shared by
\ two tasks hands each the other's descriptors.
\ A fresh task's row reads 0: every entry point resets it through
\ CAPTURE-RESET before any word reads a descriptor, and every pid guard is
\ `0 >`, treating 0 like NO-PID. KILL-CAPTURE also runs outside a
\ capture (tools/check-core.f answering a stop), where the row can read 0, and
\ kill(0) or waitpid(0) would reach the caller's whole process group.
5 constant PFD-SLOTS                     \ stdout, stderr, stdin, stop, per-call cancellation
8 constant PFD-SLOT-BYTES                \ one struct pollfd: fd | events | revents
0 constant PATHZ-OFF
PATHZ-OFF PATHZ-CAP + 7 + $FFFFFFFFFFFFFFF8 and constant PFD-OFF
PFD-OFF PFD-SLOTS PFD-SLOT-BYTES * + constant PROBE-OFF
PROBE-OFF 1 + 7 + $FFFFFFFFFFFFFFF8 and constant PID-OFF
public
PID-OFF     1 cells + constant RC-OFF
private
RC-OFF      1 cells + constant OUT-R-OFF
OUT-R-OFF   1 cells + constant OUT-W-OFF
OUT-W-OFF   1 cells + constant ERR-R-OFF
ERR-R-OFF   1 cells + constant ERR-W-OFF
ERR-W-OFF   1 cells + constant IN-R-OFF
IN-R-OFF    1 cells + constant IN-W-OFF
IN-W-OFF    1 cells + constant OUT-LEN-OFF
OUT-LEN-OFF 1 cells + constant ERR-LEN-OFF
ERR-LEN-OFF 1 cells + constant IN-OFF-OFF
IN-OFF-OFF  1 cells + constant DEADLINE-OFF
DEADLINE-OFF 1 cells + constant RD-OFF
RD-OFF      1 cells + constant STATUS-OFF
STATUS-OFF  1 cells + constant TIMED-OUT-OFF
TIMED-OUT-OFF 1 cells + constant REAP-PID-OFF
REAP-PID-OFF 1 cells + constant CANCEL-FD-OFF
public
CANCEL-FD-OFF 1 cells + constant RAW-PID-OFF
private
RAW-PID-OFF 1 cells + constant STORAGE-BYTES

public
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and STORAGE-BYTES TASK:+USER STORAGE drop

: ROW ( -- ptr u8 ) STORAGE BYTE-VIEW ;

private
: PATHZ-BUF ( -- ptr u8 ) ROW PATHZ-OFF + ;
: PFD ( -- ptr u8 ) ROW PFD-OFF + ;
: PROBE ( -- ptr u8 ) ROW PROBE-OFF + ;

public
\ TASK:+USER rows are `ptr n`: the fd, pid, rc, len and off roles are converted
\ where a value enters a word that names the role.
: PID ( -- ptr n ) ROW PID-OFF + CELL-VIEW ;
: RC ( -- ptr n ) ROW RC-OFF + CELL-VIEW ;
: OUT-R ( -- ptr n ) ROW OUT-R-OFF + CELL-VIEW ;
: OUT-W ( -- ptr n ) ROW OUT-W-OFF + CELL-VIEW ;
: ERR-R ( -- ptr n ) ROW ERR-R-OFF + CELL-VIEW ;
: ERR-W ( -- ptr n ) ROW ERR-W-OFF + CELL-VIEW ;
: IN-R ( -- ptr n ) ROW IN-R-OFF + CELL-VIEW ;
: IN-W ( -- ptr n ) ROW IN-W-OFF + CELL-VIEW ;
: OUT-LEN ( -- ptr n ) ROW OUT-LEN-OFF + CELL-VIEW ;
: ERR-LEN ( -- ptr n ) ROW ERR-LEN-OFF + CELL-VIEW ;
: IN-OFF ( -- ptr n ) ROW IN-OFF-OFF + CELL-VIEW ;
private
: DEADLINE ( -- ptr n ) ROW DEADLINE-OFF + CELL-VIEW ;
: RD ( -- ptr n ) ROW RD-OFF + CELL-VIEW ;
: STATUS ( -- ptr n ) ROW STATUS-OFF + CELL-VIEW ;
: TIMED-OUT ( -- ptr n ) ROW TIMED-OUT-OFF + CELL-VIEW ;
public
: REAP-PID ( -- ptr n ) ROW REAP-PID-OFF + CELL-VIEW ;
\ Borrowed, never read, duplicated or closed. CMD installs it after BEGIN.
: CANCEL-FD ( -- ptr n ) ROW CANCEL-FD-OFF + CELL-VIEW ;
;package

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

: PID ( -- ptr n ) PROCESS:ROW PROCESS:RAW-PID-OFF + CELL-VIEW ;

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

package PROCESS

private
PROCESS-SYMBOLS
FUNCTION: WAITPID-CALL waitpid ( n ptr u8 n -- i32 )
   1 4 WRITES-BYTES                    \ wait status is a C int, not a cell
;FUNCTION

public
: SPAWN-RAW ( ptr u8 fd fd fd -- pid )
   [: PROC-RAW:SPAWN ;] catch {: rc :}
   drop drop drop drop
   rc PROC-RAW:ANSWER ;

: KILL-RAW ( pid n -- rc ) {: pid sig :}
   pid PID>N sig kill >RC ;

: ZCOPY ( ptr u8 len ptr u8 len -- ptr u8 ) {: a:ptr u dst:ptr cap :}
   u LEN>N 0 < if E-PROC-OUTPUT throw then
   u LEN>N cap LEN>N >= if E-PROC-OUTPUT throw then
   0 begin dup u LEN>N < while
      dup a + c@  over dst + c!
      1 +
   repeat drop
   0 dst u LEN>N + c!
   dst ;

: PATHZ ( ptr u8 len -- ptr u8 )
   PATHZ-BUF PATHZ-CAP >LEN ZCOPY ;

private
\ The kernel's wait status.
: WAIT-RAW ( pid -- n )
   PID>N wait-status {: raw:n :}
   raw 0 < if E-PROC-WAIT throw then
   raw ;

: >STATUS ( n -- status ) {: raw:n :}
   raw WAIT-TERM-MASK and {: term:n :}
   term 0= if
      raw 8 rshift WAIT-EXIT-MASK and PROCESS-STATUS:exited
      exit
   then
   term PROCESS-STATUS:signaled ;

public
: STATUS>ENDING ( status -- ending )
   MATCH status
     exited OF PROCESS-ENDING:exited ENDOF
     signaled OF PROCESS-ENDING:signaled ENDOF
   ;MATCH ;

\ A timeout throws E-PROC-TIMEOUT rather than reading as an rc, so an assertion
\ on it fails as a deadline.
: ENDING>RC ( ending -- rc )
   MATCH ending
     exited OF >RC ENDOF
     signaled OF 128 + >RC ENDOF
     timeout OF E-PROC-TIMEOUT throw ENDOF
   ;MATCH ;

: STATUS>RC ( status -- rc )
   STATUS>ENDING ENDING>RC ;

\ A throw code does not cross a process boundary: `die` and the uncaught-throw
\ exit turn every negative code into UNCAUGHT-RC (67). A tool whose work can end
\ on a deadline exits TIMEOUT-RC for E-PROC-TIMEOUT, and the process that
\ runs it throws E-PROC-TIMEOUT again on that status, so the verdict reaches the
\ top of a chain of tools. 124 is coreutils `timeout`'s status; no exit-status
\ constant, `die` literal or positive throw code in src/ or tools/ is 124, and
\ a signal death reads as 128 + signal.
124 constant TIMEOUT-RC

: EXIT-RC ( n n -- n ) {: code:n fail:n :}
   code 0= if 0 exit then
   code E-PROC-TIMEOUT = if TIMEOUT-RC exit then
   fail ;

\ ENDING>RC with a timeout read as TIMEOUT-RC instead of thrown: a
\ caller that throws E-PROC-TIMEOUT again on that rc reports this deadline and
\ one that expired inside the child alike.
: ENDING>DEADLINE-RC ( ending -- rc )
   MATCH ending
     exited OF >RC ENDOF
     signaled OF 128 + >RC ENDOF
     timeout OF TIMEOUT-RC >RC ENDOF
   ;MATCH ;

\ Waits for the child to end, with no deadline; a failed wait throws
\ E-PROC-WAIT.
: WAIT ( pid -- status ) WAIT-RAW >STATUS ;

\ ok is a clean exit, err the nonzero rc (an exit code, or 128 + signal).
: RC>RESULT ( rc -- result<n,n> )
   RC>N {: rc:n :}
   rc 0 = if rc RESULT:OK else rc RESULT:ERR then ;

: SPAWN-IO ( ptr u8 len fd fd fd -- pid ) {: a:ptr u infd outfd errfd :}
   a u PATHZ infd outfd errfd SPAWN-RAW {: pid :}
   pid PID>N 0 < if E-PROC-SPAWN throw then
   pid ;

: RUN-IO-RC ( ptr u8 len fd fd fd -- result<n,n> )
   SPAWN-IO WAIT STATUS>RC RC>RESULT ;

: RUN-RC ( ptr u8 len -- result<n,n> )
   -1 >FD -1 >FD -1 >FD RUN-IO-RC ;

private
\ An idx outside 0..PFD-SLOTS-1 is E-A-BOUNDS: PFD-AT! and PFD-REVENTS are
\ public, and slot PFD-SLOTS would reach the cells after the table (PROBE-OFF).
: PFD-SLOT ( idx -- ptr n ) {: idx :}
   idx IDX>N 0 < if E-A-BOUNDS throw then
   idx IDX>N PFD-SLOTS >= if E-A-BOUNDS throw then
   idx IDX>N PFD-SLOT-BYTES * PFD + CELL-VIEW ;

public
: PFD-AT! ( fd n idx -- ) {: fd events idx :}
   events 32 lshift  fd FD>N $FFFFFFFF and  or  idx PFD-SLOT ! ;

: PFD! ( fd n -- ) {: fd events :}
   fd events 0 >IDX PFD-AT! ;

: PFD-REVENTS ( idx -- n )
   PFD-SLOT @ 48 rshift $FFFF and ;

\ Rounds up, so a wait for what is left never ends before the deadline
\ (src/lib/process-test.f TEST-WAIT-REACHES-DEADLINE).
: LEFT-MS ( n -- ms ) {: deadline :}
   deadline mono-ns - dup 0 <= if
      drop 0 >MS
   else
      NS-PER-MS 1- + NS-PER-MS / >MS
   then ;

\ A negative ms names no window: its deadline is the reading itself, and a poll
\ caller hands the negative ms to poll(2) for an unbounded wait. A window whose
\ nanoseconds would pass the largest cell is E-PROC-TIMEOUT before anything is
\ multiplied: it never wraps into the past or saturates to forever.
: DEADLINE-AT ( ms -- n ) {: timeout:ms :}
   mono-ns {: now:n :}
   timeout MS>N 0 < if now exit then
   timeout MS>N MEM-MAX-N now - NS-PER-MS / > if E-PROC-TIMEOUT throw then
   timeout MS>N NS-PER-MS * now + ;

private
\ The one call of the primitive poll: from POLL on, a bare poll here is POLL.
: POLL-ONCE ( n n -- n ) {: nfds ms :}
   PFD nfds ms poll ;

public
\ poll(2) is the one blocking call SA_RESTART never restarts: -EINTR means a
\ signal landed before any event and nothing was consumed. Each restart waits
\ out only what is left of the deadline, so no signal storm pushes a capture
\ past its timeout; a passed deadline reports the zero-event timeout. Any other
\ negative rc is an errno the caller names through its own E-PROC-* code. A
\ negative initial timeout stays unbounded across interruptions.
: POLL-RESTART ( n n n -- n ) {: nfds ms deadline :}
   nfds ms POLL-ONCE {: rc :}
   rc FD:EINTR# negate <> if rc exit then
   begin
      ms 0 < if ms else deadline LEFT-MS MS>N then {: left :}
      left 0= if 0 exit then
      nfds left POLL-ONCE {: restarted :}
      restarted FD:EINTR# negate <> if restarted exit then
   again ;

\ Polls slots 0..n-1 as they stand, armed or not (NO-FD leaves one out), for an event
\ PFD-REVENTS reads. E-A-BOUNDS: n not 1..PFD-SLOTS; E-PROC-OUTPUT; E-PROC-TIMEOUT.
: POLL ( n ms -- count ) {: nfds ms :}
   nfds 1 < if E-A-BOUNDS throw then
   nfds PFD-SLOTS > if E-A-BOUNDS throw then
   ms DEADLINE-AT {: deadline :}
   nfds ms MS>N deadline POLL-RESTART {: rc :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if E-PROC-TIMEOUT throw then
   rc >COUNT ;

\ A signal is reported as -EINTR: this word owns no deadline to restart against.
: POLL-IN ( fd ms -- count ) {: fd ms :}
   fd FD:POLLIN PFD!
   1 ms MS>N POLL-ONCE >COUNT ;

: POLL-IN-OR-TIMEOUT ( fd ms -- count ) {: fd ms :}
   fd FD:POLLIN PFD!  1 ms POLL ;

private
\ waitpid's flag and pid_t bound, the same on Linux and macOS.
1 constant WNOHANG
$7FFFFFFF constant PID-MAX    \ the largest signed 32-bit pid_t

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and 1 cells TASK:+USER BOUNDED-STATUS drop

public
\ A zero return leaves the cleared cell unread; a stop (status low bits $7F,
\ which only a traced child reports without WUNTRACED) ends nothing. EINTR
\ collects nothing; any other failure, ECHILD included, is E-PROC-WAIT. waitpid
\ writes a C int, so the whole cell is cleared first.
: WAIT-PROBE ( pid -- bool )
   {: pid:pid :}
   0 BOUNDED-STATUS !
   pid PID>N BOUNDED-STATUS BYTE-VIEW WNOHANG WAITPID-CALL {: got:n :}
   got pid PID>N = if
      BOUNDED-STATUS @ WAIT-TERM-MASK and WAIT-TERM-MASK <> exit
   then
   got 0= if 0 0= 0= exit then
   FFI:ERRNO FD:EINTR# <> if E-PROC-WAIT throw then
   0 0= 0= ;

\ pid must be a pid_t above zero (waitpid's C int would narrow a wider one); a
\ timeout leaves the child running, its pid for the caller to reap.
: WAIT-BOUNDED ( pid ms -- ending )
   {: pid:pid timeout:ms :}
   pid PID>N 0 <= if E-PROC-WAIT throw then
   pid PID>N PID-MAX > if E-PROC-WAIT throw then
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then
   timeout DEADLINE-AT {: deadline:n :}
   begin pid WAIT-PROBE 0= while
      deadline LEFT-MS MS>N 0= if PROCESS-ENDING:timeout exit then
      1 >MS TASK:SLEEP
   repeat
   BOUNDED-STATUS @ >STATUS STATUS>ENDING ;

\ A context that owns a death-watch fd (a pool worker's worker-alive read end;
\ src/lib/process-fork.f installs the live vector) arms a reaper for the
\ just-spawned capture child, so the child's group dies with the arming process.
\ The default arms nothing and returns NO-PID. A vector that cannot arm
\ throws, and CAPTURE-PID! refuses the capture. The reaper is a direct
\ child of this process and every capture-termination path kills and waits it
\ by pid, so no wait(-1) caller sees a stray child. Process-wide: one policy
\ the image installs at load.
defer REAP-ARM ( pid -- pid )
private
: REAP-ARM-OFF ( pid -- pid )
   drop NO-PID >PID ;
public
: REAP-ARM-DEFAULT ( -- )
   [: REAP-ARM-OFF ;] is REAP-ARM ;
REAP-ARM-DEFAULT

private
: REAP-DISARM ( -- )
   REAP-PID @ dup 0 > if
      >PID dup SIGKILL KILL-RAW drop
      WAIT drop
      NO-PID REAP-PID !
   else
      drop
   then ;

public
: CAPTURE-RESET ( -- )
   REAP-DISARM
   NO-PID PID !
   -1 RC !
   NO-FD OUT-R !
   NO-FD OUT-W !
   NO-FD ERR-R !
   NO-FD ERR-W !
   NO-FD IN-R !
   NO-FD IN-W !
   NO-FD CANCEL-FD !
   0 OUT-LEN !
   0 ERR-LEN !
   0 IN-OFF !
   0 STATUS !
   0 TIMED-OUT ! ;

: CLOSE-CELL ( ptr n -- ) {: p:ptr :}
   p @ dup 0 >= if
      close
      NO-FD p !
   else
      drop
   then ;

: CLOSE-CAPTURE-FDS ( -- )
   OUT-R CLOSE-CELL
   OUT-W CLOSE-CELL
   ERR-R CLOSE-CELL
   ERR-W CLOSE-CELL ;

: CLOSE-STDIN-FDS ( -- )
   IN-R CLOSE-CELL
   IN-W CLOSE-CELL ;

: CLOSE-ALL-CAPTURE-FDS ( -- )
   CLOSE-STDIN-FDS
   CLOSE-CAPTURE-FDS ;

: REAP-CAPTURE ( -- )
   PID @ dup 0 > if
      >PID WAIT-RAW dup STATUS !
      >STATUS STATUS>RC RC>N RC !
      NO-PID PID !
   else
      drop
   then
   REAP-DISARM ;

\ A throw while a child is ended must neither replace its caller's reason nor
\ vanish: CODE-REPORT names it on stderr from constant text, sharing no storage.

private
: DIAG ( ptr u8 n -- )
   {: a:ptr u:n :}
   2 a u write drop ;

\ v is at most 0, so MIN-N has a magnitude too: / and mod truncate toward zero.
: DIAG-DIGITS ( n -- )
   {: v:n :}
   v -10 <= if v 10 / RECURSE then
   s" 0123456789" drop v 10 mod negate + 1 DIAG ;

: CODE-REPORT ( ptr u8 n n -- )
   {: what:ptr whatu:n code:n :}
   code 0= if exit then
   what whatu DIAG
   code 0 < if s" -" DIAG code else code negate then DIAG-DIGITS
   s\" \n" DIAG ;

: WALK-REPORT ( n -- )
   {: code:n :}
   s" process: process tree of a killed child not walked, throw "
   code CODE-REPORT ;

public
\ Never throws. A spawned child leads a process group of its own, and so does
\ each process it spawns, so no kill of its pid or group reaches them;
\ src/lib/process-tree.f KILL-TREE freezes the tree before it lists it. A walk
\ that throws has still killed every member it found, the child first, so the
\ caller reaps the child all the same. The pid rides through the catch, so both
\ arms leave the same depth.
: KILL-TREE ( pid -- )
   [: dup PROC-TREE:KILL-TREE ;] catch WALK-REPORT drop ;

private
\ SIGNAL:SIGTERM's number: src/lib/signal.f requires this file.
15 constant SIGTERM
\ Outlasts test/db/pg-cluster.f ANSWER's QUIT-MS wait.
10000 constant END-GRACE-MS

\ The flag rides beside the pid through the catch, so both arms leave the same
\ depth and no storage two tasks could share holds it.
: CATCHES-TERM? ( pid -- bool ) {: pid:pid :}
   false pid [: nip dup SIGTERM PROC-TREE:CATCHES? swap ;] catch {: code:n :}
   code 0= if drop exit then
   2drop
   s" process: signals of a killed child not read, throw " code CODE-REPORT
   false ;

\ Until the asked child exits or END-GRACE-MS passes; proc-watch-open's negative
\ errno (on macOS, a child already gone) or a failed poll ends the wait.
: AWAIT-END ( pid -- ) {: pid:pid :}
   END-GRACE-MS >MS DEADLINE-AT {: deadline:n :}
   pid PID>N proc-watch-open {: w:n :}
   w 0 < if exit then
   w >FD FD:POLLIN PFD!
   1 deadline LEFT-MS MS>N deadline POLL-RESTART drop
   w close ;

\ Never throws. Asks only a child that catches SIGTERM (PROC-TREE:CATCHES? says
\ why); unreaped, its pid names no other process when its tree is killed.
: END-TREE ( pid -- ) {: pid:pid :}
   pid CATCHES-TERM? if
      pid SIGTERM KILL-RAW drop
      pid AWAIT-END
   then
   pid KILL-TREE ;

public
\ Ends the capture child's tree and reaps the child; only the reap throws
\ (E-PROC-WAIT). It waits up to END-GRACE-MS on a child that catches SIGTERM.
: KILL-CAPTURE ( -- )
   PID @ 0 > if
      PID @ >PID END-TREE
      REAP-CAPTURE
   then ;

private
\ The tree is killed before the capture descriptors close, and they close
\ whatever the kill throws: closing first could end the child before the walk
\ froze it, leaving what it spawned to init.
: END-CAPTURE ( -- )
   [: KILL-CAPTURE ;] [: CLOSE-ALL-CAPTURE-FDS ;] finally ;

public
: REAP-CAPTURE-TIMEOUT ( -- )
   END-CAPTURE
   1 TIMED-OUT ! ;

: THROW-CAPTURE ( n -- ) {: code :}
   END-CAPTURE
   code throw ;

\ A refused reaper arm refuses the capture: the child's tree is killed, the
\ child reaped and the descriptors closed before the code goes on.
: CAPTURE-PID! ( pid -- ) {: pid:pid :}
   pid PID>N PID !
   pid [: REAP-ARM ;] catch {: rpid code:n :}
   code 0= if rpid PID>N REAP-PID ! exit then
   code THROW-CAPTURE ;

private
: CLOEXEC-CELL ( ptr n -- ) {: p:ptr :}
   p @ FD:F-SETFD FD:FD-CLOEXEC fcntl 0 <> if E-PROC-OUTPUT THROW-CAPTURE then ;

: PIPE-CELLS ( ptr n ptr n -- ptr n ptr n ) {: rp:ptr wp:ptr :}
   FD:PIPE FD>N wp ! FD>N rp !
   rp wp ;

: OPEN-PIPE ( ptr n ptr n -- )
   [: PIPE-CELLS ;] catch {: code:n :} 2drop
   code 0<> if E-PROC-OUTPUT THROW-CAPTURE then ;

: CAPTURE-NULL-INPUT-HELD ( -- )
   s\" /dev/null\z" drop open-rd
   dup 0 < if drop E-PROC-OUTPUT THROW-CAPTURE then
   IN-R !
   IN-R CLOEXEC-CELL ;

public
\ A capture's default stdin is EOF, whatever the parent's terminal or pipe; the
\ fd sits in the capture row so a refused spawn closes it too.
: CAPTURE-NULL-INPUT ( -- fd )
   [: CAPTURE-NULL-INPUT-HELD ;] FD-LOCK:WITH
   IN-R @ >FD ;

: NONBLOCK! ( fd -- ) {: fd :}
   fd FD>N FD:F-GETFL 0 fcntl {: flags :}
   flags 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   fd FD>N FD:F-SETFL flags FD:O-NONBLOCK or fcntl 0 <> if E-PROC-OUTPUT THROW-CAPTURE then ;


private
: NOSIGPIPE! ( fd -- )
   {: fd :}
   fd FD>N FD:F-SETNOSIGPIPE 1 fcntl 0 <> if E-PROC-OUTPUT THROW-CAPTURE then ;


public
: SETUP-CAPTURE-FDS ( -- )
   OUT-R OUT-W OPEN-PIPE
   ERR-R ERR-W OPEN-PIPE ;

: SETUP-STDIN-FDS ( -- )
   IN-R IN-W OPEN-PIPE
   IN-W @ >FD NOSIGPIPE!
   IN-W @ >FD NONBLOCK! ;

: WAIT-FOREVER ( -- ms ) MEM-MAX-N >MS ;

: CAPTURE-DEADLINE! ( ms -- ) {: timeout :}
   timeout MS>N 0 < if E-PROC-TIMEOUT throw then
   timeout MS>N MEM-MAX-N = if MEM-MAX-N DEADLINE ! exit then
   timeout DEADLINE-AT DEADLINE ! ;

\ With no deadline this answers a negative ms: the capture loops never see zero
\ left, and poll(2) waits unbounded (POLL-RESTART).
: REMAINING-MS ( -- ms )
   DEADLINE @ MEM-MAX-N = if -1 >MS exit then
   DEADLINE @ LEFT-MS ;

private
\ Each capture poll and reap turn runs STOP when STOP-FD is readable,
\ so a program answering stops (CATCH-STOPS) hears one; LSP-CHECK:MAIN sets it.
3 constant STOP-SLOT

public
variable STOP-FD
NO-FD STOP-FD !

defer STOP ( -- )
private
: STOP-NONE ( -- ) ;
public
: STOP-DEFAULT ( -- )
   [: STOP-NONE ;] is STOP ;
STOP-DEFAULT

private
: ARM-STOP-PFD ( -- )
   STOP-FD @ >FD FD:POLLIN STOP-SLOT >IDX PFD-AT! ;

: STOP-HEARD ( -- )
   STOP-SLOT >IDX PFD-REVENTS 0= if exit then
   [: STOP ;] catch {: code:n :}
   code 0<> if code THROW-CAPTURE then ;

4 constant CANCEL-SLOT

: ARM-CANCEL-PFD ( -- )
   CANCEL-FD @ >FD FD:POLLIN CANCEL-SLOT >IDX PFD-AT! ;

: CANCEL-HEARD ( -- )
   CANCEL-SLOT >IDX PFD-REVENTS {: events:n :}
   events FD:POLLERR FD:POLLNVAL or and 0<> if E-PROC-OUTPUT THROW-CAPTURE then
   events FD:POLLIN FD:POLLHUP or and 0<> if E-PROC-CANCELLED THROW-CAPTURE then ;

\ A capture without stdin leaves that slot empty: every capture poll covers
\ every slot, the stop descriptor's included.
: ARM-CAPTURE-PFD ( -- )
   OUT-R @ >FD FD:POLLIN 0 >IDX PFD-AT!
   ERR-R @ >FD FD:POLLIN 1 >IDX PFD-AT!
   NO-FD >FD 0 2 >IDX PFD-AT! ;

\ Where one poll ends: its own window, never past the capture deadline, so a
\ restart after a signal waits out what is left of the window.
: POLL-END ( n -- n ) {: ms:n :}
   ms 0 < if DEADLINE @ exit then
   ms >MS DEADLINE-AT DEADLINE @ min ;

: POLL-CAPTURE-RC ( n -- n ) {: ms:n :}
   ARM-STOP-PFD
   ARM-CANCEL-PFD
   PFD-SLOTS ms ms POLL-END POLL-RESTART {: rc:n :}
   rc 0 > if STOP-HEARD CANCEL-HEARD then
   rc ;

: POLL-CAPTURE ( ms -- count ) {: ms :}
   ARM-CAPTURE-PFD
   ms MS>N POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT THROW-CAPTURE then
   rc >COUNT ;

public
: POLL-CAPTURE-ENDING ( ms -- count ) {: ms :}
   ARM-CAPTURE-PFD
   ms MS>N POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   rc >COUNT ;

: READ-STREAM ( ptr n ptr u8 len ptr n -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   lenp @ 0 < if E-PROC-TRUNCATED THROW-CAPTURE then
   lenp @ cap LEN>N > if E-PROC-TRUNCATED THROW-CAPTURE then
   cap LEN>N lenp @ - 0 <= if E-PROC-TRUNCATED THROW-CAPTURE then
   fdp @ buf lenp @ + cap LEN>N lenp @ - read RD !
   RD @ 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   RD @ cap LEN>N lenp @ - > if E-PROC-OUTPUT THROW-CAPTURE then
   RD @ 0= if
      fdp CLOSE-CELL
   else
      lenp @ RD @ + lenp !
   then ;

private
: PROBE-FULL-STREAM ( ptr n -- ) {: fdp:ptr :}
   fdp @ PROBE 1 read RD !
   RD @ 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   RD @ 1 > if E-PROC-OUTPUT THROW-CAPTURE then
   RD @ 0= if
      fdp CLOSE-CELL
   else
      E-PROC-TRUNCATED THROW-CAPTURE
   then ;

\ A read that fills the span may leave an overflow byte already in the pipe.
\ Probe it without waiting, before an expired deadline ends the capture. This
\ uses poll slot 0 after its read; stderr's slot remains available to the drain.
: PROBE-READY-FULL-STREAM ( ptr n -- ) {: fdp:ptr :}
   fdp @ >FD 0 >MS POLL-IN COUNT>N {: ready:n :}
   ready FD:EINTR# negate = if exit then
   ready 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   ready 0 > if fdp PROBE-FULL-STREAM then ;

: READ-OR-PROBE-STREAM ( ptr n ptr u8 len ptr n -- ) {: fdp:ptr buf:ptr cap lenp:ptr :}
   cap LEN>N lenp @ - 0 <= if
      fdp PROBE-FULL-STREAM
   else
      fdp buf cap lenp READ-STREAM
      cap LEN>N lenp @ = if fdp PROBE-READY-FULL-STREAM then
   then ;

public
: DRAIN-READY ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   0 >IDX PFD-REVENTS 0 <> if
      OUT-R out outcap OUT-LEN READ-OR-PROBE-STREAM
   then
   1 >IDX PFD-REVENTS 0 <> if
      ERR-R err errcap ERR-LEN READ-OR-PROBE-STREAM
   then ;

private
\ A growing capture keeps an output stream in caller-owned storage: SINK, given
\ a byte count, makes it hold at least that many bytes, keeping its bytes, and
\ answers its byte 0, as a DYNAMIC-BUFFER u8's reserve then byte 0 do. A
\ refused growth fails the capture as a read failure does.
$10000 constant GROW-CHUNK

\ SINK's first byte replaces the null one READ-GROWING hands in, since
\ catch keeps the shape of the stack.
: SINK-ROOM ( ptr u8 [ n -- ptr u8 ] ptr n -- ptr u8 [ n -- ptr u8 ] ptr n )
   {: old:ptr sink lenp:ptr :}
   lenp @ GROW-CHUNK + sink execute sink lenp ;

: READ-INTO-ROOM ( ptr n ptr u8 ptr n -- )
   {: fdp:ptr buf:ptr lenp:ptr :}
   fdp @ buf lenp @ + GROW-CHUNK read RD !
   RD @ 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   RD @ GROW-CHUNK > if E-PROC-OUTPUT THROW-CAPTURE then
   RD @ 0= if
      fdp CLOSE-CELL
   else
      lenp @ RD @ + lenp !
   then ;

: READ-GROWING ( ptr n ptr n [ n -- ptr u8 ] -- )
   {: fdp:ptr lenp:ptr sink :}
   NULL$ drop sink lenp [: SINK-ROOM ;] catch {: buf:ptr s lp:ptr code:n :}
   code 0= if fdp buf lp READ-INTO-ROOM exit then
   code THROW-CAPTURE ;

: DRAIN-READY-GROWING ( [ n -- ptr u8 ] ptr u8 len -- )
   {: sink err:ptr errcap :}
   0 >IDX PFD-REVENTS 0 <> if
      OUT-R OUT-LEN sink READ-GROWING
   then
   1 >IDX PFD-REVENTS 0 <> if
      ERR-R err errcap ERR-LEN READ-OR-PROBE-STREAM
   then ;

: DRAIN-READY-GROWING-BOTH ( [ n -- ptr u8 ] [ n -- ptr u8 ] -- )
   {: out err :}
   0 >IDX PFD-REVENTS 0 <> if
      OUT-R OUT-LEN out READ-GROWING
   then
   1 >IDX PFD-REVENTS 0 <> if
      ERR-R ERR-LEN err READ-GROWING
   then ;

public
: CAPTURE-DONE? ( -- bool )
   OUT-R @ 0 < ERR-R @ 0 < and ;

private
: STDIN-CHUNK ( len -- len ) {: u :}
   u LEN>N STDIN-CHUNK-CAP > if
      STDIN-CHUNK-CAP >LEN
   else
      u
   then ;

: CLOSE-STDIN-DONE ( len -- ) {: inu :}
   IN-OFF @ inu LEN>N >= if IN-W CLOSE-CELL then ;

: BROKEN-STDIN? ( n -- bool ) {: events :}
   events FD:POLLERR and 0= 0= if 0 0= exit then
   events FD:POLLHUP and 0= 0= if 0 0= exit then
   events FD:POLLNVAL and 0= 0= if 0 0= exit then
   0 0= 0= ;

\ A refused write on the non-blocking end is back pressure: the child stopped
\ reading. The engine collapses every write errno to -1 and the caller has
\ already asked poll whether the reader is gone, so the offset stays and the
\ next POLLOUT under the deadline resumes; a short write advances by what
\ landed. Closing here would truncate the child's stdin.
: WRITE-STDIN-ACTIVE ( ptr u8 len -- ) {: src:ptr inu :}
   inu LEN>N IN-OFF @ - >LEN STDIN-CHUNK {: chunk :}
   IN-W @ src IN-OFF @ + chunk LEN>N write {: wrote :}
   wrote 0 < if exit then
   wrote chunk LEN>N > if E-PROC-OUTPUT THROW-CAPTURE then
   IN-OFF @ wrote + IN-OFF !
   inu CLOSE-STDIN-DONE ;

public
: WRITE-STDIN ( ptr u8 len -- ) {: src:ptr inu :}
   IN-W @ 0 < if exit then
   IN-OFF @ inu LEN>N >= if IN-W CLOSE-CELL exit then
   src inu WRITE-STDIN-ACTIVE ;

private
: ARM-IO-PFD ( -- )
   ARM-CAPTURE-PFD
   IN-W @ 0 >= if
      IN-W @ >FD FD:POLLOUT 2 >IDX PFD-AT!
   else
      NO-FD >FD 0 2 >IDX PFD-AT!
   then ;

: POLL-IO ( ms -- count ) {: ms :}
   ARM-IO-PFD
   ms MS>N POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   rc 0= if E-PROC-TIMEOUT THROW-CAPTURE then
   rc >COUNT ;

public
: POLL-IO-ENDING ( ms -- count ) {: ms :}
   ARM-IO-PFD
   ms MS>N POLL-CAPTURE-RC {: rc :}
   rc 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   rc >COUNT ;

: DRIVE-STDIN ( ptr u8 len -- ) {: in:ptr inu :}
   2 >IDX PFD-REVENTS {: events :}
   events BROKEN-STDIN? if IN-W CLOSE-CELL exit then
   events FD:POLLOUT and 0= 0= if
      in inu WRITE-STDIN
   then ;

: STDIN-CAPTURE-DONE? ( -- bool )
   CAPTURE-DONE? IN-W @ 0 < and ;

\ Interrupted zero-time polls retry without waiting out the deadline. CMD calls
\ this before spawning, so an interruption cannot skip a pending cancellation.
: STOP-ASK ( -- )
   ARM-IO-PFD
   ARM-STOP-PFD
   ARM-CANCEL-PFD
   begin
      PFD-SLOTS 0 POLL-ONCE
      dup FD:EINTR# negate =
   while drop repeat {: rc:n :}
   rc 0 < if E-PROC-OUTPUT THROW-CAPTURE then
   rc 0 > if STOP-HEARD CANCEL-HEARD then ;

private
\ EOF says the child closed its pipes, not that it exited: its status is waited
\ for under the same deadline, and only the timeout path waits blocking, after
\ SIGKILL. waitpid writes four bytes, so the cell is cleared.
: REAP-CAPTURE-BOUNDED ( -- )
   begin PID @ 0 > while
      0 STATUS !
      PID @ STATUS BYTE-VIEW 1 WAITPID-CALL {: got:n :}
      got PID @ = if
         STATUS @ >STATUS STATUS>RC RC>N RC !
         NO-PID PID !
      else
         got 0<> if
            FFI:ERRNO FD:EINTR# <> if E-PROC-WAIT THROW-CAPTURE then
         then
         REMAINING-MS MS>N {: left:n :}
         left 0= if REAP-CAPTURE-TIMEOUT exit then
         STOP-ASK
         \ A halt at PAUSE would unwind out of this loop and leave the child
         \ unreaped. SLEEP returns here to reap it, and does not burn a core while
         \ the child runs.
         1 >MS TASK:SLEEP
      then
   repeat
   REAP-DISARM ;

public
\ One ready read per stream survives an expired deadline. The check after the
\ drain ends the capture before another poll, even when a child floods output.
: RUN-CAPTURE-LOOP ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   begin CAPTURE-DONE? 0= while
      REMAINING-MS POLL-CAPTURE drop
      out outcap err errcap DRAIN-READY
      REMAINING-MS MS>N 0= CAPTURE-DONE? 0= and if
         E-PROC-TIMEOUT THROW-CAPTURE
      then
   repeat ;

: RUN-CAPTURE-ENDING-LOOP ( ptr u8 len ptr u8 len -- ) {: out:ptr outcap err:ptr errcap :}
   begin CAPTURE-DONE? 0= while
      REMAINING-MS POLL-CAPTURE-ENDING dup COUNT>N 0= if
         drop
         REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      out outcap err errcap DRAIN-READY
      REMAINING-MS MS>N 0= CAPTURE-DONE? 0= and if
         REAP-CAPTURE-TIMEOUT exit
      then
   repeat
   REAP-CAPTURE-BOUNDED ;

: RUN-STDIN-CAPTURE-LOOP ( ptr u8 len ptr u8 len ptr u8 len -- )
   {: in:ptr inu out:ptr outcap err:ptr errcap :}
   inu LEN>N 0 <= if IN-W CLOSE-CELL then
   begin STDIN-CAPTURE-DONE? 0= while
      REMAINING-MS POLL-IO drop
      in inu DRIVE-STDIN
      out outcap err errcap DRAIN-READY
      REMAINING-MS MS>N 0= STDIN-CAPTURE-DONE? 0= and if
         E-PROC-TIMEOUT THROW-CAPTURE
      then
   repeat ;

: RUN-STDIN-CAPTURE-ENDING-LOOP ( ptr u8 len ptr u8 len ptr u8 len -- )
   {: in:ptr inu out:ptr outcap err:ptr errcap :}
   inu LEN>N 0 <= if IN-W CLOSE-CELL then
   begin STDIN-CAPTURE-DONE? 0= while
      REMAINING-MS POLL-IO-ENDING dup COUNT>N 0= if
         drop
         REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      in inu DRIVE-STDIN
      out outcap err errcap DRAIN-READY
      REMAINING-MS MS>N 0= STDIN-CAPTURE-DONE? 0= and if
         REAP-CAPTURE-TIMEOUT exit
      then
   repeat
   REAP-CAPTURE-BOUNDED ;

: RUN-STDIN-GROWING-CAPTURE-ENDING-LOOP ( ptr u8 len [ n -- ptr u8 ] ptr u8 len -- )
   {: in:ptr inu sink err:ptr errcap :}
   inu LEN>N 0 <= if IN-W CLOSE-CELL then
   begin STDIN-CAPTURE-DONE? 0= while
      REMAINING-MS POLL-IO-ENDING dup COUNT>N 0= if
         drop
         REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      in inu DRIVE-STDIN
      sink err errcap DRAIN-READY-GROWING
      REMAINING-MS MS>N 0= STDIN-CAPTURE-DONE? 0= and if
         REAP-CAPTURE-TIMEOUT exit
      then
   repeat
   REAP-CAPTURE-BOUNDED ;

: RUN-STDIN-GROWING-BOTH-CAPTURE-ENDING-LOOP ( ptr u8 len [ n -- ptr u8 ] [ n -- ptr u8 ] -- )
   {: in:ptr inu out err :}
   inu LEN>N 0 <= if IN-W CLOSE-CELL then
   begin STDIN-CAPTURE-DONE? 0= while
      REMAINING-MS POLL-IO-ENDING dup COUNT>N 0= if
         drop
         REAP-CAPTURE-TIMEOUT
         exit
      then
      drop
      in inu DRIVE-STDIN
      out err DRAIN-READY-GROWING-BOTH
      REMAINING-MS MS>N 0= STDIN-CAPTURE-DONE? 0= and if
         REAP-CAPTURE-TIMEOUT exit
      then
   repeat
   REAP-CAPTURE-BOUNDED ;

: CAPTURE-CHECK-CAP ( len -- )
   {: cap :}
   cap LEN>N 0 < if E-PROC-OUTPUT throw then ;

: CAPTURE-CHECK-CAPS ( len len -- ) {: outcap errcap :}
   outcap CAPTURE-CHECK-CAP
   errcap CAPTURE-CHECK-CAP ;

: CAPTURE-CHECK-STDIN ( len -- ) {: inu :}
   inu LEN>N 0 < if E-PROC-OUTPUT throw then ;

: CAPTURE-BEGIN ( ms -- ) {: timeout :}
   CAPTURE-RESET
   timeout CAPTURE-DEADLINE!
   SETUP-CAPTURE-FDS ;

: STDIN-CAPTURE-BEGIN ( ms -- ) {: timeout :}
   timeout CAPTURE-BEGIN
   SETUP-STDIN-FDS ;

private
: CAPTURE>RESULT ( len len rc -- result<pcap:captured,pcap:failed> )
   {: o:len e:len r:rc :}
   r RC>N 0 = if
      o e PCAP-CAPTURED:MAKE RESULT:OK
   else
      o e r PCAP-FAILED:MAKE RESULT:ERR
   then ;

public
: CAPTURE-RC@ ( -- result<pcap:captured,pcap:failed> )
   OUT-LEN @ >LEN ERR-LEN @ >LEN RC @ >RC CAPTURE>RESULT ;

\ Derived from the raw wait status and the timed-out flag: no stored ending can
\ drift from them.
: CAPTURE-ENDING ( -- ending )
   TIMED-OUT @ 0 <> if PROCESS-ENDING:timeout exit then
   STATUS @ >STATUS STATUS>ENDING ;

: CAPTURE-ENDING@ ( -- len len ending )
   OUT-LEN @ >LEN ERR-LEN @ >LEN
   CAPTURE-ENDING ;

: CAPTURE-FINISH-RC ( -- result<pcap:captured,pcap:failed> )
   CLOSE-ALL-CAPTURE-FDS
   REAP-CAPTURE-BOUNDED
   TIMED-OUT @ 0<> if E-PROC-TIMEOUT throw then
   CAPTURE-RC@ ;

: CAPTURE-FINISH-ENDING ( -- len len ending )
   CLOSE-ALL-CAPTURE-FDS
   CAPTURE-ENDING@ ;

\ Every spawn shape ends here: a failed spawn closes the capture descriptors and
\ throws; a live pid becomes the capture child, and the child's pipe ends close.
: CAPTURE-ADOPT-SPAWN ( pid -- ) {: pid :}
   pid PID>N 0 < if E-PROC-SPAWN THROW-CAPTURE then
   pid CAPTURE-PID!
   IN-R CLOSE-CELL
   OUT-W CLOSE-CELL
   ERR-W CLOSE-CELL ;

private
: SPAWN-CAPTURE ( ptr u8 -- )
   CAPTURE-NULL-INPUT OUT-W @ >FD ERR-W @ >FD SPAWN-RAW
   CAPTURE-ADOPT-SPAWN ;

public
: RUN-CAPTURE ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   pathu LEN>N 0 < if E-PROC-OUTPUT throw then
   outcap errcap CAPTURE-CHECK-CAPS
   path pathu PATHZ {: pathz:ptr :}
   timeout CAPTURE-BEGIN
   pathz SPAWN-CAPTURE
   out outcap err errcap RUN-CAPTURE-LOOP
   CAPTURE-FINISH-RC ;

private
: RUN-CAPTURE-ENDING ( ptr u8 len ptr u8 len ptr u8 len ms -- len len ending )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   pathu LEN>N 0 < if E-PROC-OUTPUT throw then
   outcap errcap CAPTURE-CHECK-CAPS
   path pathu PATHZ {: pathz:ptr :}
   timeout CAPTURE-BEGIN
   pathz SPAWN-CAPTURE
   out outcap err errcap RUN-CAPTURE-ENDING-LOOP
   CAPTURE-FINISH-ENDING ;
;package
