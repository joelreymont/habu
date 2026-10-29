\ pg-kill-test.f - a pg row the pool kills leaves no server process, no
\ shared-memory segment and no socket directory behind. It runs
\ test/db/pg-cluster.f as a pool's one row with test/db/pg-hold.f as its case
\ file, which reports the socket directory and SysV segment and waits in
\ pg_sleep; the pool's walk starts at the row's child and this test sees the
\ tree only through its FIFO.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/test/wait.f
require src/lib/test/runner.f
require src/lib/test/fifo.f
require test/gate-pool.f

package PG-KILL-TEST

60000 constant READY-MS              \ initdb, the server's start and the case's connect
5000 constant GONE-MS                \ the killed take a moment to finish dying
90000 constant EXPIRE-MS             \ past the server's once-a-minute lock-file recheck
10 constant NEWLINE

2 constant IPC-STAT
256 constant SHMID-DS-BYTES          \ struct shmid_ds: 80 bytes on macOS, 112 on Linux

create LINE PATH-CAP allot        \ the socket directory the case reported
create SHM PATH-CAP allot         \ the segment line it reported: `<key> <shmid>`
create SHMID-DS SHMID-DS-BYTES allot
create FIFO PATH-CAP allot

PROCESS-SYMBOLS

FUNCTION: SHM-CTL shmctl ( n n ptr u8 -- i32 )
   2 SHMID-DS-BYTES WRITES-BYTES
;FUNCTION

variable LINE-U
variable SHM-U
variable FIFO-U
variable FIFO-RD

TYPED-VARIABLE REPORTED bool         \ the whole line is in
TYPED-VARIABLE FIFO-DONE bool        \ the read end has reported end of file
false REPORTED !
true FIFO-DONE !
PROC-NO-FD FIFO-RD !

: SOCKETS$ ( -- ptr u8 n )  LINE LINE-U @ ;

: FIFO$ ( -- ptr u8 n )    FIFO FIFO-U @ ;

\ A new FIFO for the case, its read end open before the row starts, so the
\ harness's open of the write end does not wait. The harness and the case
\ engine open it by path (PG_HOLD_FIFO), the harness maps it into initdb and the
\ postmaster, and every process the postmaster forks inherits it (measured:
\ postgres keeps an inherited descriptor open in every child). The read end
\ reports end of file only when the last writer is gone.
: OPEN-FIFO ( -- )
   GT-ROOT FIFO TEST-FIFO:MAKE {: u:n r:fd :}
   u FIFO-U !
   r FD>N FIFO-RD !
   0 LINE-U !
   0 SHM-U !
   false REPORTED !
   false FIFO-DONE ! ;

\ fd within ms: a positive count once it has bytes to read or has reached end of
\ file, 0 when ms ran out first, a negative errno when the poll failed.
: READY-WITHIN ( n n -- n )
   {: fd:n ms:n :}
   fd >FD POLLIN PROC-PFD!
   1 ms ms >MS PROC-DEADLINE-AT PROC-POLL-RESTART ;

\ One line into buf, a byte at a time up to its newline, before the deadline.
\ End of file first means the case died before it reported: that, a line that
\ fills buf with no newline, a failed read or a failed poll answers false. The
\ deadline running out first is thrown as a timeout, named for REPORT-READ?.
: LINE-READ? ( ptr u8 ptr n n -- bool )
   {: buf:ptr count:ptr deadline:n :}
   0 count !
   begin
      count @ PATH-CAP >= if false exit then
      FIFO-RD @ deadline PROC-LEFT-MS MS>N READY-WITHIN {: ready:n :}
      ready 0= if s" pg-kill-test REPORT-READ" TEST-WAIT:EXPIRED then
      ready 0 < if false exit then
      FIFO-RD @ buf count @ + 1 read 1 <> if false exit then
      buf count @ + c@ NEWLINE = if true exit then
      count @ 1+ count !
   again ;

\ A report the case could not finish answers false and leaves REPORTED false.
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

\ True while the segment the case reported exists: IPC_STAT (2 on both hosts)
\ answers 0 for it and -1 once it has been removed. A segment id carries a
\ sequence number, so the next segment in its slot does not answer for it.
: SEGMENT? ( -- bool )
   SHMID {: id:n :}
   id 0 < if false exit then
   id IPC-STAT SHMID-DS SHM-CTL 0= ;

: SEGMENT-LEFT. ( -- )
   s" pg-kill: segment " type SHMID FMT:.INT
   s"  outlived its server; `ipcrm -m " type SHMID FMT:.INT
   s" ` removes it" type cr ;
\ End of file within ms: nothing under the row holds the write end. A failed
\ read or poll answers false. ms running out first is this test's own
\ deadline, named and thrown as a timeout. A FIFO no writer has opened reads
\ end of file too, so it means anything only after the case's report. On macOS,
\ whose poll never reports a FIFO's end of file, a case file that dies before
\ its report is the report wait's timeout instead of a failed report.
: GONE-WITHIN? ( n -- bool )
   {: ms:n :}
   FIFO-DONE @ if true exit then
   FIFO-RD @ >FD ms TEST-FIFO:EOF-WITHIN {: eof:n :}
   eof 0= if s" pg-kill-test GONE-WITHIN" TEST-WAIT:EXPIRED then
   eof 0 < if false exit then
   true FIFO-DONE !
   true ;

: REPORT-ENV ( -- )
   s" PG_HOLD_FIFO" >LEN FIFO$ >LEN PROC-ENV+ ;

\ The case ends the row itself once the case file has reported.
: ROW-START ( -- )
   OPEN-FIFO
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/db/pg-cluster.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" test/db/pg-hold.f" >LEN PROC-ARGV+
   REPORT-ENV
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ s" pg row" GT-POOL-START ;

\ The row runs two pools deep (the gate's and this file's), and every pool level
\ nests TMPDIR deeper: postgres refuses a socket path over 103 bytes on macOS,
\ so its server starting and reporting is the check.
: EXPECT-LIVE ( -- )
   s" the case reported the server's socket directory and segment" T-LABEL
   REPORT-READ? TTRUE
   s" the socket directory is there while the row runs" T-LABEL
   REPORTED @ if SOCKETS$ FS:EXISTS? else false then TTRUE
   s" the server's shared-memory segment is there while the row runs" T-LABEL
   REPORTED @ if SEGMENT? else false then TTRUE ;

\ Asked once the pool has ended the row; each catches one way a kill leaks:
\ - a postmaster outside the tree: pg_ctl (or a shell's `&`, a setsid) leaves
\   it in a session of its own under init, out of the walk's reach
\   (src/lib/process-tree.f), running until its lock-file recheck notices the
\   removed data directory about a minute later;
\ - the server's children: each calls setsid, so only its parent link ties it
\   to the tree; the auxiliaries and the busy backend hold the FIFO as well;
\ - a grace that orphans: a root that does not catch SIGTERM, or exits before
\   ending its tree, leaves its group leaders to init; the harness catches it
\   and the case engine, leading its own group, holds the FIFO. A root that
\   does not catch SIGTERM is not sent it (src/lib/process-tree.f CATCHES?);
\ - a SysV segment, keyed by the data directory's inode and removed only on
\   postgres's own way out, which a SIGKILL never runs: kern.sysv.shmmni caps
\   segments host-wide (32 here), so each one left is one PostgreSQL fewer on
\   the host for good (measured: one per kill). The segment is postmaster.pid
\   line 7;
\ - directories a SIGKILL leaves: the socket directory and the row's scratch,
\   which holds the data directory.
\ Not asserted: a kill racing the postmaster's spawn (the walk's macOS limit for
\ a spawn blocked in the kernel remains); a root that has not ended inside
\ test/gate-pool.f GT-POOL-GRACE-MS (walked and killed as it stands); a kill
\ during initdb, whose backend holds a segment (the harness sends initdb
\ SIGTERM; a case would need initdb slowed past the grace); two rows colliding
\ (no TCP port; mkdir refuses an existing name). test/db/pg-cluster.f ends red
\ unless its server's clean stop exits 0.
: EXPECT-GONE ( -- )
   s" no process under the row is left" T-LABEL
   GONE-MS GONE-WITHIN? TTRUE
   s" the server's shared-memory segment is gone" T-LABEL
   REPORTED @ if SEGMENT? 0= else false then {: freed:bool :}
   freed TTRUE
   freed 0= REPORTED @ and if SEGMENT-LEFT. then
   s" the socket directory is gone" T-LABEL
   REPORTED @ if SOCKETS$ FS:EXISTS? 0= else false then TTRUE
   s" the row's scratch, and the data directory in it, is gone" T-LABEL
   0 >IDX GT-POOL-TMP$ FS:EXISTS? TFALSE ;

\ The case ends the live row with GT-POOL-KILL-ALL, how a signalled gate root
\ ends its rows (test/gate-pool.f GT-POOL-SIGNAL-DIE).
: KILL-ALL-BODY ( -- )
   ROW-START
   EXPECT-LIVE
   GT-POOL-KILL-ALL
   EXPECT-GONE ;

\ What a failed case owes. A server the kill missed stops itself once its
\ lock-file recheck, which runs once a minute, finds its data directory gone,
\ so the row's directories go first and the FIFO is given that long; a writer
\ still there after it is this test's own deadline, thrown as a timeout. The
\ socket directory the case reported goes last, wherever it was, and goes even
\ when that wait throws.
: SWEEP ( -- )
   [: 0 >IDX GT-POOL-CHILD-TMP-REMOVE
      s" the sweep left no process behind" T-LABEL
      EXPIRE-MS GONE-WITHIN? TTRUE ;]
   [: REPORTED @ if SOCKETS$ FS:EXISTS? if SOCKETS$ FS:REMOVE-TREE then then ;]
   finally ;

\ Runs after every body, whatever it asserted or threw; each step is a no-op for
\ a case that passed. The sweep runs even when the kill throws, and the read end
\ closes even when the sweep throws. A segment a SIGKILL left is named with the
\ command that removes it (SEGMENT-LEFT.); nothing here removes one.
: CASE-END ( -- )
   [: [: GT-POOL-KILL-ALL ;]
      [: FIFO-DONE @ 0= if SWEEP then ;] finally ;]
   [: FIFO-RD PROC-CLOSE-CELL ;] finally ;

\ The pool is reset before the body: GT-POOL-KILL-ALL reads the slot table the
\ reset fills in.
: POOL-RESET ( -- )
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   GT-POOL-RED-RESET ;

public

: MAIN ( -- )
   T-RESET
   s" pg-kill" GT-START
   s" pg-kill: kill-all" type cr
   POOL-RESET
   [: KILL-ALL-BODY ;] [: CASE-END ;] finally
   GT-CLEANUP ;

;package

PG-KILL-TEST:MAIN
T-REPORT
s" pg-kill-test: ok" type cr
