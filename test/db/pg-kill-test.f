\ pg-kill-test.f - a pg row the pool kills leaves no server process, no
\ shared-memory segment and no socket directory behind.
\
\     bin/hb --load test/db/pg-kill-test.f
\
\ THE SUBJECT is the pg row's harness, test/db/pg-cluster.f, run as the one row
\ of a pool in this process with test/db/pg-hold.f as its case file. The case
\ connects, reports the directory the server made its socket in and the SysV
\ segment the server holds, and waits in pg_sleep, so the row is killed with
\ its cluster up and a backend busy. Every
\ process under the row holds the write end of one pipe - the harness, initdb,
\ the case engine, the postmaster and each process it forks (measured: postgres
\ keeps an inherited descriptor open in every child) - and the read end reports
\ end of file only when the last of them is gone. Two cases, one for each way
\ the pool ends a live row: at the row's deadline, and GT-POOL-KILL-ALL, which
\ is how a signalled gate root ends its rows (test/gate-pool.f
\ GT-POOL-SIGNAL-DIE; test/gate-signal-test.f sends the signal itself).
\
\ THE WAYS THIS CAN FAIL, written down before the fix:
\
\  1. THE POSTMASTER LEAVES THE TREE. pg_ctl forks, calls setsid, and execs a
\     shell that execs postgres; then pg_ctl exits. The postmaster is left in
\     a session of its own with init for a parent, where the pool's walk
\     (lib/process-tree.f) cannot reach it, and it runs on until its lock-file
\     recheck notices the removed data directory, about a minute later. Any
\     start path with such a step - pg_ctl, a shell's `&`, a setsid - does the
\     same.
\     Asserted: the pipe reaches end of file within GONE-MS of the kill.
\  2. THE SERVER'S CHILDREN OUTLIVE IT. Every process the postmaster forks
\     calls setsid, so only its parent link ties it to the tree; one listed
\     after the postmaster died would have init for a parent, and a postmaster
\     left running forks more.
\     Asserted: the same pipe, which the auxiliary processes and the case's
\     backend, busy in pg_sleep, hold as well.
\  3. A KILL THAT RACES THE START. A walk that stops the harness while it is
\     spawning the postmaster. Not asserted: the kill here lands after the
\     report. The postmaster is the harness's child from the moment it
\     exists, so it is a member like any other; the walk's macOS limit for a
\     spawn blocked in the kernel remains (docs/gate.md).
\  4. A SOCKET PATH PAST sun_path. postgres refuses a socket path over 103
\     bytes on macOS, and every pool level nests HB_TMP deeper.
\     Asserted: this row runs two pools deep - the gate's and this file's -
\     and its server starts and reports.
\  5. DIRECTORIES THAT OUTLIVE A SIGKILL. Nothing in the row runs after its
\     SIGKILL, so whatever it made that the pool does not remove stays: the
\     data directory and the socket directory.
\     Asserted: the reported socket directory is there while the row runs and
\     gone once the pool has killed it, and so is the row's scratch, which
\     holds the data directory.
\  6. A CLEAN STOP THAT REGRESSES. The harness's own stop, and its time.
\     Asserted by the pg row's first file, test/db/pg-cluster.f itself, which
\     ends red unless its server's stop ends it with exit 0. Not timed.
\  7. TWO pg ROWS COLLIDING, on a port or a path. Not asserted: the server
\     listens on no TCP port, and every directory a row uses is made by
\     mkdir, which refuses a name that exists.
\  8. ANOTHER GATE'S SERVER. Other agents run pg rows on this host. Nothing
\     here names a process: the walk starts at the row's child, and this test
\     knows the row's processes only through its own pipe.
\  9. A SEGMENT THAT OUTLIVES THE SERVER. PostgreSQL makes a SysV shared-memory
\     segment keyed by its data directory's inode and removes it only on its
\     own way out, which a SIGKILL never runs. Nothing else removes it - APFS
\     does not hand the inode out again - and kern.sysv.shmmni caps the
\     segments host-wide (32 here), so each one left is one PostgreSQL fewer
\     on the host for good. Measured before the fix: one per kill, two per run
\     of this file.
\     Asserted: the segment the case reported (postmaster.pid line 7) is there
\     while the row runs and gone once the pool has killed it.
\ 10. A GRACE THAT ORPHANS. SIGTERM to a root that does not catch it ends it at
\     once and leaves what it spawned, each leading its own group, to init
\     before the walk can list it; a root that catches it and exits before it
\     has ended its own tree does the same.
\     Asserted for the harness, which catches it: the pipe check, which the
\     case engine - spawned, leading its own group - holds as well. A root
\     that does not catch SIGTERM is not sent it (lib/process-tree.f
\     CATCHES?), and test/gate-pool-test.f and test/gate-signal-test.f kill
\     such trees.
\ 11. A ROOT THAT NEVER ANSWERS. One that catches SIGTERM and has not ended
\     inside test/gate-pool.f GT-POOL-GRACE-MS is walked and killed as it
\     stands. Not asserted.
\
\ A FAILED CASE CLEANS UP AFTER ITSELF. Each case ends through CASE-END
\ whatever it asserted or threw: the row is killed, a server the kill missed is
\ given the minute its lock-file recheck takes to stop it, and the socket
\ directory it reported is removed. A segment left by a server that was
\ SIGKILLed is named with the command that removes it; nothing here removes
\ one.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require test/gate-pool.f

package PG-KILL-TEST

60000 constant READY-MS              \ initdb, the server's start and the case's connect
5000 constant GONE-MS                \ the killed take a moment to finish dying
90000 constant EXPIRE-MS             \ past the server's once-a-minute lock-file recheck
120000 constant ROW-MS               \ the row's own deadline, until a case ends it
64 constant SINK-CAP
10 constant NEWLINE
-1 constant NO-FD

2 constant IPC-STAT
256 constant SHMID-DS-BYTES          \ struct shmid_ds: 80 bytes on macOS, 112 on Linux

create LINE FS-PATH-CAP allot        \ the socket directory the case reported
create SHM FS-PATH-CAP allot         \ the segment line it reported: `<key> <shmid>`
create SINK SINK-CAP allot
create SHMID-DS SHMID-DS-BYTES allot

PROCESS-SYMBOLS

FUNCTION: SHM-CTL shmctl ( n n ptr u8 -- i32 )
   2 SHMID-DS-BYTES WRITES-BYTES
;FUNCTION

variable LINE-U
variable SHM-U
variable PIPE-RD
variable PIPE-WR

TYPED-VARIABLE REPORTED bool         \ the whole line is in
TYPED-VARIABLE PIPE-DONE bool        \ the read end has reported end of file
false REPORTED !
true PIPE-DONE !
NO-FD PIPE-RD !
NO-FD PIPE-WR !

: SOCKETS$ ( -- ptr u8 n )  LINE LINE-U @ ;

: CLOSE-CELL ( ptr n -- ) {: p:ptr :}
   p @ 0 >= if p @ close then
   NO-FD p ! ;

: FD-TEXT$ ( n -- ptr u8 n )
   SB-RESET FMT:SB-INT SB$ ;

\ The write end stays inheritable: it is what every process under the row
\ holds. The read end is this process's alone.
: OPEN-PIPE ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   r FD-CLOEXEC!
   r FD>N PIPE-RD !
   w FD>N PIPE-WR !
   0 LINE-U !
   0 SHM-U !
   false REPORTED !
   false PIPE-DONE ! ;

\ TRUE once fd has bytes to read or has reached end of file, within ms.
: READY-WITHIN? ( n n -- bool ) {: fd:n ms:n :}
   fd >FD POLLIN PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART 0 > ;

\ One line into buf, a byte at a time up to its newline, before the deadline.
\ End of file first means the case died before it reported.
: LINE-READ? ( ptr u8 ptr n n -- bool ) {: buf:ptr count:ptr deadline:n :}
   0 count !
   begin
      count @ FS-PATH-CAP >= if false exit then
      PIPE-RD @ deadline PROC-LEFT-MS MS>N READY-WITHIN? 0= if false exit then
      PIPE-RD @ buf count @ + 1 read 1 <> if false exit then
      buf count @ + c@ NEWLINE = if true exit then
      count @ 1+ count !
   again ;

\ Both lines, before READY-MS runs out.
: REPORT-READ? ( -- bool )
   READY-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   LINE LINE-U deadline LINE-READ? 0= if false exit then
   SHM SHM-U deadline LINE-READ? 0= if false exit then
   true REPORTED !
   true ;

\ The second field of the segment line; -1 when it holds no number there.
: SHMID ( -- n )
   0 begin dup SHM-U @ < if SHM over + c@ STR-SPACE <> else false then while 1+ repeat
   begin dup SHM-U @ < if SHM over + c@ STR-SPACE = else false then while 1+ repeat
   {: at:n :}
   SHM at + SHM-U @ at - STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ TRUE while the segment the case reported exists: IPC_STAT (2 on both
\ hosts) answers 0 for it and -1 once it has been removed. A segment id
\ carries a sequence number, so a removed one is not answered for by the next
\ segment in its slot.
: SEGMENT? ( -- bool )
   SHMID {: id:n :}
   id 0 < if false exit then
   id IPC-STAT SHMID-DS SHM-CTL 0= ;

: SEGMENT-LEFT. ( -- )
   s" pg-kill: segment " type SHMID FMT:.INT
   s"  outlived its server; `ipcrm -m " type SHMID FMT:.INT
   s" ` removes it" type cr ;
\ End of file within ms: nothing under the row holds the write end.
: GONE-WITHIN? ( n -- bool ) {: ms:n :}
   PIPE-DONE @ if true exit then
   ms >MS PROC-DEADLINE-AT {: deadline:n :}
   begin
      PIPE-RD @ deadline PROC-LEFT-MS MS>N READY-WITHIN? 0= if false exit then
      PIPE-RD @ SINK SINK-CAP read {: got:n :}
      got 0= if true PIPE-DONE ! true exit then
      got 0 < if false exit then
   again ;

\ ---- the row -----------------------------------------------------------------

: REPORT-ENV ( -- )
   s" PG_HOLD_FD" >LEN PIPE-WR @ FD-TEXT$ >LEN PROC-ENV+ ;

\ The row's deadline is one it cannot reach: a fixed short one would race
\ initdb and the server's start on a loaded host. Each case ends the row
\ itself once the case file has reported.
: ROW-START ( -- )
   OPEN-PIPE
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/db/pg-cluster.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/db/pg-hold.f" >LEN PROC-ARGV+
   REPORT-ENV
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ s" pg row" ROW-MS GT-POOL-START
   PIPE-WR CLOSE-CELL ;

\ Up: the case reported, and the directory and the segment it named are there.
: EXPECT-LIVE ( -- )
   s" the case reported the server's socket directory and segment" T-LABEL
   REPORT-READ? TTRUE
   s" the socket directory is there while the row runs" T-LABEL
   REPORTED @ if SOCKETS$ EXISTS? else false then TTRUE
   s" the server's shared-memory segment is there while the row runs" T-LABEL
   REPORTED @ if SEGMENT? else false then TTRUE ;

\ Failure modes 1, 2, 5, 9 and 10, asked once the pool has ended the row.
: EXPECT-GONE ( -- )
   s" no process under the row is left" T-LABEL
   GONE-MS GONE-WITHIN? TTRUE
   s" the server's shared-memory segment is gone" T-LABEL
   REPORTED @ if SEGMENT? 0= else false then {: freed:bool :}
   freed TTRUE
   freed 0= REPORTED @ and if SEGMENT-LEFT. then
   s" the socket directory is gone" T-LABEL
   REPORTED @ if SOCKETS$ EXISTS? 0= else false then TTRUE
   s" the row's scratch, and the data directory in it, is gone" T-LABEL
   0 >IDX GT-POOL-TMP$ EXISTS? TFALSE ;

: DEADLINE-BODY ( -- )
   ROW-START
   EXPECT-LIVE
   0 0 >IDX GT-POOL-TIMEOUT-PTR !
   GT-POOL-DRAIN-SOFT
   s" the pool retired the row as a timeout" T-LABEL
   GT-POOL-RED# 1 T=
   0 GT-POOL-RED-TIMED-OUT-PTR @ TTRUE
   EXPECT-GONE ;

: KILL-ALL-BODY ( -- )
   ROW-START
   EXPECT-LIVE
   GT-POOL-KILL-ALL
   EXPECT-GONE ;

\ What a failed case owes. A server the kill missed stops itself once its
\ lock-file recheck, which runs once a minute, finds its data directory gone,
\ so the row's directories go first and the pipe is given that long. The
\ socket directory the case reported goes last, wherever it was.
: SWEEP ( -- )
   0 >IDX GT-POOL-CHILD-TMP-REMOVE
   s" the sweep left no process behind" T-LABEL
   EXPIRE-MS GONE-WITHIN? TTRUE
   REPORTED @ 0= if exit then
   SOCKETS$ EXISTS? if SOCKETS$ REMOVE-TREE then ;

\ Whatever the body asserted or threw. Every step is a no-op for a case that
\ passed.
: CASE-END ( -- )
   GT-POOL-KILL-ALL
   PIPE-DONE @ 0= if SWEEP then
   PIPE-WR CLOSE-CELL
   PIPE-RD CLOSE-CELL ;

\ The pool is reset before the body: GT-POOL-KILL-ALL reads the slot table the
\ reset fills in.
: POOL-RESET ( -- )
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET ;

public

: MAIN ( -- )
   T-RESET
   s" pg-kill: deadline" type cr
   POOL-RESET
   [: DEADLINE-BODY ;] [: CASE-END ;] finally
   s" pg-kill: kill-all" type cr
   POOL-RESET
   [: KILL-ALL-BODY ;] [: CASE-END ;] finally
   GT-POOL-FALLBACK-REMOVE ;

;package

PG-KILL-TEST:MAIN
T-REPORT
s" pg-kill-test: ok" type cr
